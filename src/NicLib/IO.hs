{-# LANGUAGE RankNTypes, MultiWayIf, FlexibleContexts, LambdaCase, ViewPatterns #-}
-- TODO: create methods "safe" and "short" for easy conversion and dualism of WriterT and ExceptT (esp. s vs. [s] auto-conversion in conjunction with iso & dual)
module NicLib.IO
( ErrorMsg
, EIO
, SEIO
, orError
, lefty
, stderrOnFail
, printToStderr
, boolToErr
) where

import qualified Data.ListLike as LL
import Data.ListLike.String hiding (fromString)
import Data.ListLike (StringLike)
import Data.String (IsString, fromString)
import NicLib.NStdLib
import Control.Monad.Catch
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer
import System.IO.Error
import System.IO (stderr, hPutStrLn)

type ErrorMsg = forall s. (StringLike s) => s -- typedef is of limited use until GHC supports impredicative polymorphism...but still, if-want-here-is
type EIO a = forall m str. (MonadCatch m, MonadIO m, StringLike str) => ExceptT str m a
type SEIO s a = forall m str. (MonadCatch m, MonadIO m, StringLike str) => StateT s (ExceptT str m) a

lefty :: (MonadIO m, MonadCatch m) => e -> ExceptT e m a
lefty = ExceptT . return . Left

-- TODO: see <http://hackage.haskell.org/package/base-4.11.1.0/docs/System-IO-Error.html#v:userError> for annotated errors
-- these error descriptions are better than nothing, but are quite unhelpful (e.g. "filesystem object already exists" - which one?)
-- please always include descriptive error messages that specify concrete values (e.g. song.mp3 rather than "file" or "a file") that pertain to the nature of the program you're writing. If you fail to account for something, these extra messages may be helpful
orError :: (MonadIO m, MonadCatch m, LL.ListLike str Char, IsString str) => ExceptT str m a -> str -> ExceptT str m a
action `orError` msg = catch action $ \e ->
    let desc = if | isPermissionError e -> " (permission error)" -- TODO: check UID against owner's UID of resource we're trying to access; find exactly where and what the permission (or owner) mismatch is
                  | isAlreadyExistsError e -> " (filesystem object already exists)"
                  | isDoesNotExistError e -> " (filesystem object doesn't exist)"
                  | isAlreadyInUseError e -> " (filesystem object already in use)"
                  | isFullError e -> " (device we're trying to write to is already full)"
                  | isEOFError e -> " (end of file reached too early (this is a programming error))"
                  | isIllegalOperation e -> " (\"illegal operation\")"
                  | otherwise -> ""
    in lefty (msg `LL.append` fromString desc)

-- | prints a StringLike error message for EIO failure; returns m () on success
stderrOnFail :: (StringLike s, MonadIO m) => ExceptT s m a -> m ()
stderrOnFail = (\case Left err -> liftIO $ hPutStrLn stderr (toString err); _ -> return ()) <=< runExceptT

-- | print all collected errors, if any
printToStderr :: (StringLike s, LL.ListLike s Char, MonadIO m) => WriterT [s] m a -> m a
printToStderr = (\(a, err) -> when (not $ LL.null err) (liftIO $ mapM_ (hPutStrLn stderr . toString) err) >> return a) <=< runWriterT

-- | converts a True to an lefty-ExceptT
boolToErr :: (MonadCatch m, MonadIO m) => Bool -> e -> ExceptT e m ()
boolToErr True e = lefty e
boolToErr False _ = return ()
