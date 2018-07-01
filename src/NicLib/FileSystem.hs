{-# LANGUAGE
    LambdaCase
,   ViewPatterns
#-}
module NicLib.FileSystem
( mrDir
, lsDirRelativeTo
, subDirs
, subDirsIO
, renameFile
, nextDuplicateFileName
, pathOrNextDuplicate
, fileExtension
) where

import Control.Monad.IO.Class
import Data.Tree
import NicLib.NStdLib
import Prelude hiding (FilePath)
import System.IO
import qualified System.Directory as D
import System.FilePath (pathSeparator)
import qualified Data.Set as S

lsDirRelativeTo :: MonadIO m => String -> m [String]
lsDirRelativeTo fp@(FilePath -> fp') = liftIO $ fmap (unwrapFilePath . ((fp' <>) . FilePath)) <$> D.listDirectory fp

-- | find filesystem objects matching a given predicate (may generalize to map/reduce in future, to be like GNU find(1))
mrDir :: MonadIO m => (String -> m Bool) -> String -> m (S.Set String)
mrDir p = go where
--  go :: MonadIO m => String -> m (S.Set String) -- because p is used in mrDir and go, GHC supposes that the MonadIO's may be different. Uncomment to help with typechecking, until GHC says that it can't match m with m1, then comment-out again, and it should compile
    go r = do
        this <- bool S.empty (S.singleton r) <$> p r
        isDir <- liftIO (D.doesDirectoryExist r)
        if isDir then lsDirRelativeTo r >>= foldM (\acc -> fmap (mappend acc) . go) this else return this

-- | Get file tree recursively, with map/reduce. The filter and map are applied to the absolute path, for each file.
-- Filter must return monadic value in order to work with doesFileExist &c
-- it's advisable to only ever give absolute paths as arguments to this function!
-- it's recommended to not try to use subDirs as a substitute to find(1); the fact that subDirs returns a tree rather than a list gives it some power and limitations that make it behave simply differently from a map/reduce traversal over a filesystem that returns a list of filepaths.
subDirs :: MonadIO m => (String -> m Bool) -> (String -> a) -> String -> m (Tree a)
subDirs p m = unfoldTreeM f
    where
        f fp = do
            e <- liftIO $ D.doesDirectoryExist fp
            ls <- if e then
                      filterM p =<< lsDirRelativeTo fp
                  else
                      return []
            return (m fp, ls)

-- | Map an IO action breadth-first recursively over a file tree. The filter and map are applied to the absolute path, for each file.
-- Some filters you may want: doesFileExist, doesDirectoryExist, doesPathExist; otherwise, if you want seperate actions depending on type of filesystem object, use a MultiWayIf in the mapping, and use (const True) as your filter
-- Some mappings you may want: (putStrLn . unwrapFilePath)
-- IO action will be applied to filtered elements. Suppose the IO action deletes directories; then those directories are obviously not traversed. Suppose the directories are renamed; the renamed directories are still traversed. Literally, we mapM_ over matching subfiles, then re-evaluate the directory structure to determine which directories we can still traverse recursively, and then continue our traversal. So if your action creates new directories, those new directories will also be mapped over. This means that if you're creating new directories at every current working directory, subDirsIO will never return!
-- only works on directories. If you provide a path to a file object, even if the file satisfies the filter, it will not be mapped-over. Just do that yourself. This function is strictly for recursively acting over a directory. This function still maps actions over subfiles, however.
-- remember to wrap in FilePath for mappend/<>, if filtering for equality of directory paths
subDirsIO :: MonadIO m => (String -> m Bool) -> (String -> m ()) -> String -> m ()
subDirsIO p m fp = do
    e <- liftIO $ D.doesDirectoryExist fp
    when e $ do
        (match, nomatch) <- partitionM p =<< lsDirRelativeTo fp
        mapM_ m match
        mapM_ (subDirsIO p m) nomatch

-- | creates parent directories as necessary. Basically better than System.Directory.renameFile
renameFile :: String -> String -> IO ()
renameFile old new = do
    case breakAtLast pathSeparator new of
        Nothing -> return ()
        Just (dir, _) -> do
            D.createDirectoryIfMissing True dir
            D.renameFile old new

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
            Just (d, "") -> c ++ "1"
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
