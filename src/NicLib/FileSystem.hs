{-# LANGUAGE
  FlexibleContexts
, LambdaCase
, MultiWayIf
, OverloadedStrings
, ScopedTypeVariables
, TupleSections
, ViewPatterns
#-}

-- if someone's auditing this code, please help me understand why I need ScopedTypeVariables and explicit type specification for iso :: ExceptT s m a -> WriterT s m a, and what I can do to make it nicer! >^<

-- | a module that replaces System.Directory functions with UNIX fileutil names (e.g. cp, mv) and wraps everything in ExceptT for automatic error handling
module NicLib.FileSystem
( FilePath(..)
, subDirsS
, subDirsT
, subDirsIO
, mkdir
, mkcd
, ln
, cd
, ls
, lsRel
, lsRel'
, mv
, cp
, cpWithMetadata
, cpPermissions
, nextDuplicateFileName
, pathOrNextDuplicate
, fileExtension
, dirAndBase
, dirname
, basename
, noPathEnd
, dirdiff
, fileEq
, realPath
) where

import System.Posix.Files (createSymbolicLink, readSymbolicLink)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer
import Data.Foldable
import Data.ListLike (ListLike)
import Data.String (IsString(..))
import Data.Tree
import NicLib.Errors
import NicLib.Text (quote)
import NicLib.List
import NicLib.NStdLib
import Prelude hiding (FilePath)
import System.FilePath (pathSeparator)
import System.IO hiding (FilePath)
import qualified Data.ListLike as LL
import qualified Data.Set as S
import qualified Prelude
import qualified System.Directory as D

newtype FilePath = FilePath { unwrapFilePath :: Prelude.FilePath }
instance IsString FilePath where fromString = FilePath
instance Show FilePath where show = unwrapFilePath
instance Monoid FilePath where
    mempty = FilePath mempty
    (unwrapFilePath -> a) `mappend` (unwrapFilePath -> []) = FilePath a
    (unwrapFilePath -> []) `mappend` (unwrapFilePath -> b) = FilePath b
    (unwrapFilePath -> a)  `mappend` (unwrapFilePath -> b) = FilePath $
        if | c a && d b -> a <> tail b -- we don't want a//b (posix) or a\\b (win); System.FilePath.(</>) doesn't account for this
           | c a || d b -> a <> b
           | otherwise  -> a <> (pathSeparator:b)
        where c = (pathSeparator ==) . last
              d = (pathSeparator ==) . head
instance Semigroup FilePath where (<>) = mappend -- GHC < 8.4.1 compat

-- | prettier alias
ls :: MonadIO m => String -> m [String]
ls = liftIO . D.listDirectory

-- | Suppose System.Directory.listDirectory "dir" returns ["a.file", "b.ext"]; then
-- lsRel "dir" returns ["dir/a.file", "dir/b.ext"] (or with backslashes for Windows)
lsRel :: (MonadIO m, MonadCatch m, Semigroup s, LL.ListLike s Char, IsString s) => String -> ExceptT s m [String]
lsRel fp@(FilePath -> fp') = do
    liftIO (D.doesDirectoryExist fp) >>= flip when (err . fromString $ "The directory " ++ fp ++ " does not exist (perhaps something by that name exists, but isn't a directory?)") . not
    fmap (unwrapFilePath . ((fp' <>) . FilePath)) <$> ls fp `orError` (fromString $ "can't list contents of directory " ++ fp)

-- | same as lsRel, but returns in a WriterT rather than an ExceptT
lsRel' :: (MonadIO m, MonadCatch m, Semigroup s, LL.ListLike s Char, IsString s, Monoid s, Eq s) => String -> WriterT [s] m [String]
lsRel' = (iso :: (Monoid s, Eq s, Monad m, MonadIO m) => ExceptT s m [String] -> WriterT s m [String]) . withExceptT pure . lsRel

-- | find filesystem objects matching a given predicate (for general map/reduce, use subDirsIO)
-- subDirsS is basically the same function as subDirsT; the two differences are that 1) subDirsT allows you to apply a non-Kleisli morphism over the filenames, 2) subDirsT returns a Tree, whereas subDirsS returns a Set
-- a simple way to list all the files in a directory recursively is subDirsS (const (return True)) dir
-- the WriterT variable is an accumulation of error messages; subDirsS continues where it can, but notes places where it failed (usually insufficient permissions to read a directory)
-- btw, "mr" in "subDirsS" was for map/reduce, but it's a misnomer now, since it's really just filterLs
-- this is not a lazy function! S.take 4 <$> subDirsS (const $ return True) dir may take lots of time and memory before returning! (this is actually a bug, as it's unacceptable behavior.)
subDirsS :: (LL.ListLike s Char, Semigroup s, MonadIO m, MonadCatch m, IsString s, Eq s, Monoid s) => (String -> m Bool) -> String -> WriterT [s] m (S.Set String)
subDirsS p s = let this = lift $ bool S.empty (S.singleton s) <$> p s in liftIO (D.doesDirectoryExist s) >>= bool this (lsRel' s >>= foldl' (\b a -> b `mappend` subDirsS p a) this)

-- | Get file tree recursively, with map/reduce. The filter and map are applied to the absolute path, for each file.
-- Filter is Kleisli in order to work with doesFileExist &c
-- it's advisable to only ever give absolute paths as arguments to this function!
-- it's recommended to not try to use subDirsT as a substitute to find(1); the fact that subDirsT returns a tree rather than a list gives it some power and limitations that make it behave simply differently from a map/reduce traversal over a filesystem that returns a list of filepaths.
-- the reason that subDirsT takes a morphism but subDirsS doesn't is that subDirsT uses unfoldTreeM, which takes a morphism. So may as well save a traversal since we're given the opportunity to....
subDirsT :: (LL.ListLike s Char, Semigroup s, IsString s, Eq s, MonadIO m, MonadCatch m) => (String -> m Bool) -> (String -> a) -> String -> WriterT [s] m (Tree a)
subDirsT p m = unfoldTreeM $ \fp -> fmap (m fp,) $ bool (return []) (filterM (lift . p) =<< lsRel' fp) =<< liftIO (D.doesDirectoryExist fp)

-- | Map an IO action breadth-first recursively over a file tree. The filter and map are applied to the path relative to the given file. Use subDirsIO (const (return True)) putStrLn to see this. 
-- Basically, subDirsIO p m fp = lsdir fp >>= filterM p >>= mapM_ m >> lsdir fp >>= mapM_ (subDirsIO p m)
-- Some filters you may want: doesFileExist, doesDirectoryExist, doesPathExist; otherwise, if you want seperate actions depending on type of filesystem object, use a MultiWayIf in the mapping, and use (const True) as your filter
-- Some mappings you may want: (putStrLn . unwrapFilePath)
-- Only works on directories. If you provide a path to a file object, even if the file satisfies the filter, it will not be mapped-over. Just do that yourself. This function is strictly for recursively acting over a directory. This function still maps actions over subfiles, however.
-- Remember to wrap in FilePath for mappend/<>, if filtering for equality of directory paths
subDirsIO :: (MonadIO m, MonadCatch m, Semigroup s, LL.ListLike s Char, IsString s, Eq s) => (String -> m Bool) -> (String -> m ()) -> String -> WriterT [s] m ()
subDirsIO p m fp = do
    e <- liftIO $ D.doesDirectoryExist fp
    when e $ do -- yes, we call lsRel twice here; it may return a different value the second time it's called!
        lsRel' fp >>= filterM (lift . p) >>= mapM_ (lift . m)
        lsRel' fp >>= mapM_ (subDirsIO p m)

-- | each creates parent directories as necessary
mv, cp, cpWithMetadata, cpPermissions :: (MonadIO m, MonadCatch m, Semigroup s, LL.ListLike s Char, IsString s) => String -> String -> ExceptT s m ()
mv = withNewDir (cT liftIO D.renameFile)
cp = withNewDir (cT liftIO D.copyFile)
cpWithMetadata = withNewDir (cT liftIO D.copyFileWithMetadata)
cpPermissions = withNewDir (cT liftIO D.copyPermissions)

-- | creates a destination directory if it doesn't already exist, then performs an action
-- helper (non-exported) method for mv, cp, &c
withNewDir :: (IsString s, ListLike s Char, Semigroup s, MonadCatch m, MonadIO m) => (t -> [Char] -> ExceptT s m b) -> t -> [Char] -> ExceptT s m b
withNewDir f a b@(dirname -> b') = (null b' ? return () ↔ mkdir b') >> f a b

-- | used to store files with duplicate names; transforms file.ext into file-1.ext; transforms file-1.ext into file-2.ext, &c
-- transforms file into file-1, if no extension
nextDuplicateFileName :: String -> String
nextDuplicateFileName [] = []
nextDuplicateFileName a = case breakAtLast '.' a of
    Nothing -> f a
    Just (b, ext) -> f (init b) ++ '.':ext
    where
        f c = case breakAtLast '-' c of
            Nothing -> c ++ "-1"
            Just (_, "") -> c ++ "1"
            Just (d, x) -> case (readMaybe :: String -> Maybe Int) x of
                Nothing -> d
                Just n -> d ++ (show $ succ n)

-- | checks whether object at path exists, and if not, returns original name; if so, returns nextDuplicateFileName name
-- works for relative and absolute pathnames, as per System.Directory.doesPathExist
pathOrNextDuplicate :: String -> IO String
pathOrNextDuplicate p = D.doesPathExist p >>= bool (return p) (return $ nextDuplicateFileName p)

-- | get file extension. returns empty string if no extension. extension includes leading dot
fileExtension :: String -> String
fileExtension s = if '.' `elem` s then ('.':) . reverse . takeWhile (/='.') $ reverse s else empty

-- | create a directory. Convenience method: catches IOError's
mkdir :: (MonadIO m, MonadCatch m, Semigroup s, ListLike s Char, IsString s) => String -> ExceptT s m ()
mkdir dir = (liftIO $ D.createDirectoryIfMissing True dir) `orError` "couldn't create directory"

-- equivalent to bash "mkdir -p dir && cd dir"
mkcd :: (MonadIO m, MonadCatch m, Semigroup s, ListLike s Char, IsString s) => String -> ExceptT s m ()
mkcd = mkdir &&& cd >*> (>>)

-- | Create a symbolic link. (POSIX ONLY!)
-- same order as cp: ln src (Just dest) will create a link called dest that points to src.
-- ln src Nothing will create a link with the basename of src in the working directory
ln :: (MonadIO m, MonadCatch m, Semigroup s, LL.ListLike s Char, IsString s) => String -> Maybe String -> ExceptT s m ()
ln src = withNewDir (cT liftIO createSymbolicLink) src . \case
    Nothing -> "./" ++ basename src -- (1) TODO: make work for non-POSIX pathnames
    Just d -> isJust (breakAtLast pathSeparator d) ? d ↔ "./" ++ d -- (1) here too
-- note: we add "./" because, for some reason, an exception is thrown if dest is a relative directory: "changeWorkingDirectory: does not exist (No such file or directory)". We can easily get around this error by prefixing the destination path with "./"

-- | just easier to read than D.setCurrentDirectory (and lifted for convenience)
cd :: MonadIO m => String -> m ()
cd dir = liftIO $ D.setCurrentDirectory dir

-- | Split a filepath into its (directory, filename)
-- please note that the directory name returned (if a directory is given) will have a trailing pathSeparator
-- in other words, when sensible (i.e. pathname has a directory and filename,) pathname = uncurry ListLike.append (dirAndBase pathname)
dirAndBase :: ListLike full Char => full -> (full, full)
dirAndBase s | LL.null s = (LL.empty, LL.empty)
             | LL.last s == pathSeparator = (s, LL.empty)
             | otherwise = fromMaybe (LL.empty, s) $ breakAtLast pathSeparator s

-- | left-/longest parent segment of filepath, with trailing path separator removed, if any
dirname :: ListLike full Char => full -> full
dirname = fst . dirAndBase

-- | right-/child-most segment of filepath, with trailing path separator removed, if any
basename :: ListLike full Char => full -> full
basename = snd . dirAndBase

-- | remove trailing path separator from string
noPathEnd :: ListLike s Char => s -> s
noPathEnd s | LL.null s = LL.empty
            | otherwise = LL.last s == pathSeparator ? LL.init s ↔ s

-- | compare two directories recursively, for structure and file content; one can safely say that if dirdiff returns mempty on directories containing ONLY FILES AND DIRECTORIES (no symlinks or special filesystem objects!), then these two directories are exactly identical.
-- perhaps I'll add support for comparing other filesystem objects later
-- returns (files present in a but not b, files present in b but not a, files present in both but with different file contents)
-- the WriterT variable is a concatenation of any error messages from trying read both directories
dirdiff :: (ListLike s Char, Semigroup s, IsString s, Eq s) => String -> String -> WriterT [s] IO (S.Set String, S.Set String, S.Set String)
dirdiff a b = do
    let f d = -- get files with prefixes removed; suitable for set subtraction
            let dir = noPathEnd d
                prefixLen = length dir + 1 -- length to strip off: length of dirname plus 1 for pathSeparator; we do this because subDirsS always returns paths prefixed with pathSeparator
            in do
                (dirContents, errs) <- runWriterT $ subDirsS (const (return True)) dir
                return (dir, S.map (drop prefixLen) dirContents, errs)
    (d1, fs1, e1) <- f a
    (d2, fs2, e2) <- f b
    tell (e1 `LL.append` e2)
    let leftOutput  = fs1 S.\\ fs2
        rightOutput = fs2 S.\\ fs1
    overlap <- flip foldMapM (fs1 `S.intersection` fs2) $ \o ->
        let p1 = unwrapFilePath $ FilePath d1 <> FilePath o
            p2 = unwrapFilePath $ FilePath d2 <> FilePath o
        in (iso :: Eq s => ExceptT [s] IO (S.Set String) -> WriterT [s] IO (S.Set String)) . withExceptT pure $ do
            e1 <- liftIO $ D.doesFileExist p1 -- these two existence checks are just a fast way to filter (fs1 ∩ fs2) for files only (no directories)
            e2 <- liftIO $ D.doesFileExist p2
            if not e1 || not e2 then return mempty else bool mempty (S.singleton o) <$> fileEq p1 p2
    return (leftOutput, rightOutput, overlap)

-- | test whether files' contents are equal (opens files in binary mode)
-- please note that fileEq does not dereference links; supposing link1 -> file1, fileEq link1 file1 --> False
-- however, System.Posix.readSymbolicLink link1 == file1, implying too that (fileEq file1 =<< readSymbolicLink link1) --> True
fileEq :: (MonadIO m, MonadCatch m, LL.ListLike s Char, Semigroup s, IsString s) => String -> String -> ExceptT s m Bool
fileEq a b = flip orError ("Failed to compare contents of files " `LL.append` quote (fromString a) `LL.append` " and " `LL.append` quote (fromString b)) . liftIO $ do
    h1 <- openBinaryFile a ReadMode
    h2 <- openBinaryFile b ReadMode
    e <- liftA2 (/=) (hGetContents h1) (hGetContents h2)
    seq e (return ())
    hClose h1
    hClose h2
    return e

-- | dereferences a chain of symbolic links until the actual filesystem object is found, or Nothing if a link is broken
-- given that realPath, given an object name, returns that object name, realPath serves as a sort of "doesPathUltimatelyExist"
-- the [String] ExceptT error value is a stack of the links that realPath tried to read, with the one that failed at the head
realPath :: String -> ExceptT [String] IO String
realPath = go . pure
    where go ss@(s:_) = liftIO (D.doesPathExist s) >>= \case
            False -> err ss
            True -> liftIO (D.pathIsSymbolicLink s) >>= \case
                False -> return s
                True -> liftIO (readSymbolicLink s) >>= go . (:ss) -- equal to realPath, but I err on the side of recursion rather than mutual recursion, for optimization.
