{-# LANGUAGE RankNTypes, MultiWayIf, FlexibleContexts #-}
module NicLib.IO
( ErrorMsg
, EIO
, SEIO
, orError
) where

import qualified Data.ByteString.Char8 as BS'
import qualified Data.ListLike as LL
import Data.ListLike (StringLike)
import Data.String
import NicLib.NStdLib
import Control.Monad.Catch
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except
import System.IO.Error

type ErrorMsg = forall s. (StringLike s) => s
type EIO a = forall m str. (MonadCatch m, MonadIO m, StringLike str) => ExceptT str m a
type SEIO s a = forall m str. (MonadCatch m, MonadIO m, StringLike str) => StateT s (ExceptT str m) a

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
    in ExceptT . return . Left $ msg `LL.append` fromString desc
