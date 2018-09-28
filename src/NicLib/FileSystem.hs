-- | a module that replaces System.Directory functions with UNIX fileutil names (e.g. cp, mv) and wraps everything in ExceptT for automatic error handling
module NicLib.FileSystem
( FilePath(..)
, subDirsT
, subDirsIO
, mkdir
, mkcd
, ln
, cd
, ls
, lsRel
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

-- import NicLib.Structures.Trie (Trie)
-- import qualified NicLib.Structures.Trie as Trie
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Accum
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Foldable
import Data.ListLike (ListLike)
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Tree
import NicLib.Errors
import NicLib.List
import NicLib.NStdLib
import NicLib.Text (quote)
import Prelude hiding (FilePath)
import System.FilePath (pathSeparator)
import System.IO hiding (FilePath)
import System.Posix.Files (createSymbolicLink, readSymbolicLink)
import qualified Data.ListLike as LL
import qualified Data.Set as S
import qualified Data.Text as T'
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

-- TODO: use conduit's Data.Conduit.Combinators.sourceDirectory(Deep) <https://www.stackage.org/haddock/lts-12.9/conduit-1.3.0.3/Data-Conduit-Combinators.html#v:sourceDirectory>
-- | prettier alias
ls :: MonadIO m => String -> m [String]
ls = liftIO . D.listDirectory

-- | Suppose System.Directory.listDirectory "dir" returns ["a.file", "b.ext"]; then
-- lsRel "dir" returns ["dir/a.file", "dir/b.ext"] (or with backslashes for Windows)
lsRel :: (MonadIO m, MonadCatch m) => String -> ExceptT Text m [String]
lsRel fp@(FilePath -> fp') = do
    whenM (liftIO $ not <$> D.doesDirectoryExist fp) checkDir
    fmap (unwrapFilePath . ((fp' <>) . FilePath)) <$> ls fp `orError` (T'.pack $ "can't list contents of directory " <> fp)
    where
        checkDir = err . T'.pack =<< (bool ("The directory " <> fp <> " does not exist!") (fp <> " exists, but is not a directory!") <$> liftIO (D.doesPathExist fp))

-- | @lsRel@ modified to suit @AccumT@. More than a mere transformation, the logic is slightly different, as it's expecting to handle both directories and other filesystem objects (assumedly just files for now.)
lsRel' :: (MonadIO m, MonadCatch m, LL.ListLike full Text, Monoid full) => String -> AccumT full m [String]
lsRel' fp@(FilePath -> fp') = fmap (fromMaybe (pure [])) . as . withExceptT LL.singleton $ do -- @as@ will convert to a Maybe a, since that's the general case. Here we will exploit that Nothing ~ []
    eDir <- liftIO $ D.doesDirectoryExist fp
    eAny <- liftIO $ D.doesPathExist fp
    if | eDir -> fmap (unwrapFilePath . ((fp' <>) . FilePath)) <$> ls fp `orError` (T'.pack $ "can't list contents of directory " <> fp)
       | eAny -> return mempty
       | otherwise -> err . T'.pack $ "No filesystem object named " <> fp <> " exists!"

-- | Get file tree recursively, with map/reduce. The filter and map are applied to the absolute path, for each file.
-- Filter is Kleisli in order to work with @doesFileExist@ &c
-- it's advisable to only ever give absolute paths as arguments to this function!
-- it's recommended to not try to use @subDirsT@ as a substitute to find(1); the fact that @subDirsT@ returns a tree rather than a list gives it some power and limitations that make it behave simply differently from a map/reduce traversal over a filesystem that returns a list of filepaths.
subDirsT :: (MonadIO m, MonadCatch m, LL.ListLike full Text, Monoid full) => (String -> m Bool) -> (String -> a) -> String -> AccumT full m (Tree a) -- TODO: replace with Trie, after writing unfoldTrieM
subDirsT p m = unfoldTreeM $ \fp -> (m fp,) <$> bool' (pure []) (filterM (lift . p) =<< lsRel' fp) (liftIO $ D.doesDirectoryExist fp)

-- | Map an IO action breadth-first recursively over a file tree. The filter and map are applied to the path relative to the given file. Use subDirsIO (const (return True)) putStrLn to see this. 
-- Basically, subDirsIO p m fp = lsdir fp >>= filterM p >>= mapM_ m >> lsdir fp >>= mapM_ (subDirsIO p m)
-- Some filters you may want: doesFileExist, doesDirectoryExist, doesPathExist; otherwise, if you want seperate actions depending on type of filesystem object, use a MultiWayIf in the mapping, and use (const True) as your filter
-- Some mappings you may want: (putStrLn . unwrapFilePath)
-- Only works on directories. If you provide a path to a file object, even if the file satisfies the filter, it will not be mapped-over. Just do that yourself. This function is strictly for recursively acting over a directory. This function still maps actions over subfiles, however.
-- Remember to wrap in FilePath for mappend/<>, if filtering for equality of directory paths
subDirsIO :: (MonadIO m, MonadCatch m, LL.ListLike full Text, Monoid full) => (String -> m Bool) -> (String -> m ()) -> String -> AccumT full m ()
subDirsIO p m fp = whenM (liftIO $ D.doesDirectoryExist fp) $ do
    lsRel' fp >>= filterM (lift . p) >>= mapM_ (lift . m)
    lsRel' fp >>= mapM_ (subDirsIO p m) -- yes, we call lsRel twice here; it may return a different value the second time it's called, since the prior one may have changed the directory!

-- | each creates parent directories as necessary
mv, cp, cpWithMetadata, cpPermissions :: (MonadIO m, MonadCatch m) => String -> String -> ExceptT Text m ()
mv = withNewDir (cT liftIO D.renameFile)
cp = withNewDir (cT liftIO D.copyFile)
cpWithMetadata = withNewDir (cT liftIO D.copyFileWithMetadata)
cpPermissions = withNewDir (cT liftIO D.copyPermissions)

-- | creates a destination directory if it doesn't already exist, then performs an action
-- helper (non-exported) method for mv, cp, &c
withNewDir :: (MonadCatch m, MonadIO m) => (a -> String -> ExceptT Text m b) -> a -> String -> ExceptT Text m b
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
pathOrNextDuplicate p = bool' (return p) (return $ nextDuplicateFileName p) (D.doesPathExist p)

-- | get file extension. returns empty string if no extension. extension includes leading dot
fileExtension :: String -> String
fileExtension s = if '.' `elem` s then ('.':) . reverse . takeWhile (/='.') $ reverse s else empty

-- | create a directory. Convenience method: catches IOError's
mkdir :: (MonadIO m, MonadCatch m) => String -> ExceptT Text m ()
mkdir dir = (liftIO $ D.createDirectoryIfMissing True dir) `orError` "couldn't create directory"

-- equivalent to bash "mkdir -p dir && cd dir"
mkcd :: (MonadIO m, MonadCatch m) => String -> ExceptT Text m ()
mkcd = mkdir &&& cd >*> (>>)

-- | Create a symbolic link. (POSIX ONLY!)
-- same order as cp: ln src (Just dest) will create a link called dest that points to src.
-- ln src Nothing will create a link with the basename of src in the working directory
ln :: (MonadIO m, MonadCatch m) => String -> Maybe String -> ExceptT Text m ()
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
-- the AccumT variable is a concatenation of any error messages from trying read both directories
dirdiff :: (ListLike full Text) => String -> String -> AccumT full IO (S.Set String, S.Set String, S.Set String)
dirdiff a b = do
    (d1, fs1, e1) <- f a
    (d2, fs2, e2) <- f b
    add (e1 <> e2)
    let leftOutput  = fs1 S.\\ fs2
        rightOutput = fs2 S.\\ fs1
    overlap <- flip foldMapM (fs1 `S.intersection` fs2) $ \o ->
        let p1 = unwrapFilePath $ FilePath d1 <> FilePath o
            p2 = unwrapFilePath $ FilePath d2 <> FilePath o
        in do -- for some reason bool' ⋯ (liftIO $ D.doesFileExist p1 <&&> D.doesFileExist p2) didn't work, but if-then-else does :/
            bothFilesExist <- liftIO $ D.doesFileExist p1 <&&> D.doesFileExist p2
            fmap (fromMaybe mempty) . as . withExceptT LL.singleton $
                if bothFilesExist then
                    bool mempty (S.singleton o) <$> fileEq p1 p2
                else
                    return mempty
    return (leftOutput, rightOutput, overlap)
    where
        -- files with prefixes removed; suitable for set subtraction
        f d =
            let dir = noPathEnd d
                prefixLen = length dir + 1 -- length to strip off: length of dirname plus 1 for pathSeparator; we do this because subDirsS always returns paths prefixed with pathSeparator
            in liftIO $ do
                (dirContents, errs) <- runAccumT (subDirsT (const (return True)) (drop prefixLen) dir) mempty
                return (dir, foldr S.insert S.empty dirContents, errs)

-- | test whether files' contents are equal (opens files in binary mode)
-- please note that fileEq does not dereference links; supposing link1 -> file1, fileEq link1 file1 --> False
-- however, System.Posix.readSymbolicLink link1 == file1, implying too that (fileEq file1 =<< readSymbolicLink link1) --> True
fileEq :: (MonadIO m, MonadCatch m) => String -> String -> ExceptT Text m Bool
fileEq a b = flip orError ("Failed to compare contents of files " `LL.append` quote (T'.pack a) `LL.append` " and " `LL.append` quote (T'.pack b)) . liftIO $ do
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
realPath = go . pure -- ignore warning "non-exhaustive pattern not matched []"
    where go ss@(s:_) = liftIO (D.doesPathExist s) >>= \case
            False -> err ss
            True -> liftIO (D.pathIsSymbolicLink s) >>= \case
                False -> return s
                True -> liftIO (readSymbolicLink s) >>= go . (:ss) -- equal to realPath, but I err on the side of recursion rather than mutual recursion, for optimization.
