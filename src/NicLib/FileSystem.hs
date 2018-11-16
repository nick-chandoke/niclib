-- | Some helpful convenience functions. Note that much of this library has been obsoleted by conduit and safe-exceptions.
--
-- Concerning exceptions, this lib used to have everything wrapped in @AccumT@ or @ExceptT@. No longer! After all, exception handling is nuanced and specialized to the particular needs of the program being run. As of NicLib v0.1.3 it's now up to the user to handle (usually accumulating or short-circuiting on) exceptions. Use 'NicLib.Errors' or 'Control.Exception.Safe' for this ;)
--
-- Functions like 'ls' and 'mkdir' that may throw predictable exceptions (e.g. directory does not exist, permissions errors) do not have any exception handling done. However, functions that use such functions will account for their exceptions and concatenate them together (e.g. dirdiff will catch all file-not-found, can't-enter-directory, etc. exceptions, logging them in a @StringException@.) I do this because it's expected that any function f that concerns filesystem objects not passed to f as a parameter will probably throw some kind of exception, and we don't want to short-circuit the computation for something so commonplace.
module NicLib.FileSystem
( FilePath(..)
, mkdir
, mkcd
, ln
, cd
, ls
, mv
, mvt
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
{- , dirdiff
, dirsame -}
, fileEq
, realPath
) where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad (guard)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
-- import Control.Monad.Trans.Reader -- * uncomment when finishing dirdiff
import Data.Bool (bool)
import Data.Foldable
-- import Data.IORef -- *
import Data.ListLike (ListLike)
import Data.Maybe (isJust, fromMaybe)
-- import Data.Sequence ((|>)) -- *
import Data.String (IsString(..))
import Data.Text (Text)
-- import Data.Tree -- *
import NicLib.Errors
import NicLib.List
import NicLib.NStdLib
-- import NicLib.Text (quote) -- *
import Prelude hiding (FilePath)
import System.FilePath (pathSeparator)
import System.IO hiding (FilePath)
import System.Posix.Files (createSymbolicLink, readSymbolicLink)
import qualified Data.ListLike as LL
-- import qualified Data.Set as S -- *
-- import qualified Data.Text as T' -- *
import qualified Prelude
import qualified System.Directory as D

-- | Wrapper around @String@ filepaths such that their semigroup operation guarantees exactly one 'pathSeparator' between the @FilePath@'s. This is separate from @(\</\>)@, which sometimes puts two consecutive separators.
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

-- | Prettier alias for 'D.listDirectory'
ls :: MonadIO m => String -> m [String]
ls = liftIO . D.listDirectory

-- | creates parent directories as necessary
mv, cp, cpWithMetadata, cpPermissions :: (MonadIO m, MonadCatch m) => String -> String -> m ()
mv = withNewDir (cT liftIO D.renameFile)
cp = withNewDir (cT liftIO D.copyFile)
cpWithMetadata = withNewDir (cT liftIO D.copyFileWithMetadata)
cpPermissions = withNewDir (cT liftIO D.copyPermissions)

-- creates a destination directory if it doesn't already exist, then performs an action
-- helper (non-exported) method for mv, cp, &c
withNewDir :: (MonadCatch m, MonadIO m) => (a -> String -> m b) -> a -> String -> m b
withNewDir f a b@(dirname -> b') = (null b' ? return () ↔ mkdir b') >> f a b

-- | @mvt destDir files@ in Haskell, equals, in bash, @mv -t destDir ${files[\@]}@
mvt :: MonadIO m => String -> [String] -> m ()
mvt destDir@(FilePath -> dd) files = liftIO $ do
    guard =<< D.doesPathExist destDir
    mapM_ (\f -> D.renameFile f (unwrapFilePath $ dd <> FilePath (basename f))) files

-- | Stores files with duplicate names; transforms "file.ext" into "file-1.ext"; transforms "file-1.ext" into "file-2.ext", &c
--
-- For filepaths without extensions: transforms "file" into "file-1", and "file-1" into "file-2"
nextDuplicateFileName :: String -> String
nextDuplicateFileName [] = mempty
nextDuplicateFileName a = case breakAtLast '.' a of
    Nothing -> f a
    Just (b, ext) -> f (init b) ++ '.':ext
    where
        f c = case breakAtLast '-' c of
            Nothing -> c ++ "-1"
            Just (_, "") -> c ++ "1"
            Just (d, x) -> case readMaybe x :: Maybe Int of
                Nothing -> d
                Just n -> d ++ show (succ n)

-- | Checks whether object at path exists, and if not, returns original name; if so, returns 'nextDuplicateFileName' @name@
--
-- Works for both relative and absolute pathnames, as per 'System.Directory.doesPathExist'
pathOrNextDuplicate :: String -> IO String
pathOrNextDuplicate p = bool p (nextDuplicateFileName p) <$> D.doesPathExist p

-- | Get file extension. Returns empty string if no extension. Extension includes leading dot.
fileExtension :: String -> String
fileExtension s = if '.' `elem` s then ('.':) . reverse . takeWhile (/='.') $ reverse s else empty

mkdir :: (MonadIO m, MonadCatch m) => String -> m ()
mkdir dir = (liftIO $ D.createDirectoryIfMissing True dir) `orThrow` "couldn't create directory"

-- | Equivalent to @mkdir -p dir && cd dir@ in bash.
mkcd :: (MonadIO m, MonadCatch m) => String -> m ()
mkcd = mkdir &&& cd >*> (*>)

-- | Create a symbolic link. (POSIX ONLY!)
--
-- Same order as 'cp': @ln src (Just dest)@ will create a link called dest that points to @src@.
--
-- @ln src Nothing@ will create a link with the basename of @src@ in the working directory.
ln :: (MonadIO m, MonadCatch m) => String -> Maybe String -> m ()
ln src = withNewDir (cT liftIO createSymbolicLink) src . \case
    Nothing -> "./" ++ basename src -- (1) TODO: make work for non-POSIX pathnames
    Just d -> isJust (breakAtLast pathSeparator d) ? d ↔ "./" ++ d -- (1) here too
-- note: we add "./" because, for some reason, an exception is thrown if dest is a relative directory: "changeWorkingDirectory: does not exist (No such file or directory)". We can easily get around this error by prefixing the destination path with "./"

-- | Just easier to read than 'D.setCurrentDirectory' (and lifted for convenience)
cd :: MonadIO m => String -> m ()
cd dir = liftIO $ D.setCurrentDirectory dir

-- | Split a filepath into its (directory, filename)
--
-- Please note that the directory name returned (if a directory is given) will have a trailing pathSeparator
--
-- In other words, when sensible (i.e. pathname has a directory and filename,) @pathname = uncurry ListLike.append (dirAndBase pathname)@
dirAndBase :: ListLike full Char => full -> (full, full)
dirAndBase s | LL.null s = (LL.empty, LL.empty)
             | LL.last s == pathSeparator = (s, LL.empty)
             | otherwise = fromMaybe (LL.empty, s) $ breakAtLast pathSeparator s

-- | Longest parent segment of filepath (i.e. from left side), with trailing path separator removed, if any
dirname :: ListLike full Char => full -> full
dirname = fst . dirAndBase

-- | Child-most segment of filepath, with trailing path separator removed, if any
basename :: ListLike full Char => full -> full
basename = snd . dirAndBase

-- | Remove trailing path separator from string
noPathEnd :: ListLike s Char => s -> s
noPathEnd s | LL.null s = LL.empty
            | otherwise = LL.last s == pathSeparator ? LL.init s ↔ s

{-
-- | Compare two directories recursively, for structure and file content; one can safely say that if dirdiff returns mempty on directories containing ONLY FILES AND DIRECTORIES (symlinks and special filesystem objects are not considered!), then these two directories are exactly identical.
 -
-- Perhaps I'll add support for comparing other filesystem objects later
--
-- Returns (files present in a but not b, files present in b but not a, files present in both but with different file contents)
--
-- 'dirsame' is @dirdiff@'s complement
--
-- Also note that @dirdiff@ will crash if either of its arguments are null
dirdiff :: String -> String -> ReaderT (IORef (Seq Text)) _ (Trie Char (), Trie Char (), Trie Char ())
dirdiff (forceTrailingSep -> a) (forceTrailingSep -> b) = do
    ref <- ask
    let pushToLog :: MonadResource m => IOError -> ConduitT i o m ()
        pushToLog e = liftIO $ modifyIORef' ref (|> (T'.pack $ show e)) -- atomicity isn't important here
    handleC pushToLog $ do
        sourceDirectoryDeep False a .| (\x -> whenM doesFileExist (rel a b x))
        sourceDirectoryDeep False b
    -- read from a's directory stream; it'll basically be a condiut version of the set-diff-fold, except that rather than give b as a starting value and checking if x ∈ b, just don't have b at all, and check whether a file exists at x relative to b.
diff a b = S.foldr (\x (ja, jb, ovrlp) -> if x `S.member` b then (ja, S.delete x jb, S.insert x ovrlp) else (S.insert x ja, jb, ovrlp)) (S.empty, b, S.empty) a
    where
        forceTrailingSep x = if last x == pathSeparator then x else x ++ [pathSeparator]
        -- example: rel "/home/nic/Downloads/" "/mnt/backup/nic/Downloads/" "/home/nic/Downloads/tomato.png" --> "/mnt/backup/nic/Downloads/tomato.png"
        rel:: String -> String
        rel x y fp = y ++ drop (length x) fp

-- | The complement to @dirdiff@: returns a set (as a @Trie@) of. Not yet implemented.
dirsame :: String -> String -> Trie Char ()
dirsame = undefined
-}

-- | Test whether files' contents are equal (opens files in binary mode)
--
-- Please note that @fileEq@ does not dereference links; supposing that link1 → file1, then
--
-- >>> fileEq link1 file1
-- False
--
-- However, @System.Posix.readSymbolicLink link1 == file1@, implying too that
--
-- >>> fileEq file1 =<< readSymbolicLink link1
-- True
fileEq :: String -> String -> IO Bool
fileEq a b = do
    h1 <- openBinaryFile a ReadMode
    h2 <- openBinaryFile b ReadMode
    e <- liftA2 (/=) (hGetContents h1) (hGetContents h2)
    seq e (return ())
    hClose h1
    hClose h2
    return e

-- | Dereferences a chain of symbolic links until the actual filesystem object is found, or @Nothing@ if a link is broken
--
-- Given that realPath, given an object name, returns that object name, realPath serves as a sort of "doesPathUltimatelyExist"
--
-- The @[String]@ ExceptT error value is a stack of the links that realPath tried to read, with the one that failed at the head
realPath :: String -> ExceptT [String] IO String
realPath = go . pure -- ignore warning "non-exhaustive pattern not matched []"
    where go ss@(s:_) = liftIO (D.doesPathExist s) >>= \case
            False -> err ss
            True -> liftIO (D.pathIsSymbolicLink s) >>= \case
                False -> return s
                True -> liftIO (readSymbolicLink s) >>= go . (:ss) -- equal to realPath, but I err on the side of recursion rather than mutual recursion, for optimization.
