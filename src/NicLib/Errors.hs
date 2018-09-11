{-# LANGUAGE
   LambdaCase
 , MultiWayIf
 , OverloadedStrings
 , TupleSections
 , ViewPatterns
#-}

-- AccumT's are used mostly as WriterT's herein; you'll see a lot of things in the vein of runAccumT (add ... >> ...) mempty
-- | Handle logging, filtering for, and printing, errors
-- btw, I may use the term "bug-out" to refer to a BugT returning a Left-like value
module NicLib.Errors
( BugT
, bugT
, runBugT
, warn
, err
, orError
, dumpWarn
, getWarning
, stderrOnFail
, stderrOnFailAndExit
, toStderr
, liftME
) where

import Control.Monad.Catch
import Control.Monad.Trans.Accum
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.ListLike (StringLike)
import Data.ListLike.String hiding (fromString)
import Data.String (IsString)
import NicLib.NStdLib
import System.Exit
import System.IO (stderr, hPutStrLn)
import System.IO.Error

-- | Combination of the ExceptT and AccumT monads. Allows logging warning messages, or exiting on errors.
-- Encourages assuming that anything that could generate a warning could, sometime through revisions of the program, produce an error,
-- and conversely that anything that produces a fatal error is error-prone enough that it may output some helpful warning messages.
type BugT e w m a = ExceptT e (AccumT w m) a

runBugT :: Monoid w => BugT e w m a -> m (Either e a, w)
runBugT = flip runAccumT mempty . runExceptT

bugT ::  m (Either e a, w) -> BugT e w m a
bugT = ExceptT . AccumT . const

-- | push warning to log
warn :: (Monoid w, Monad m) => w -> BugT e w m ()
warn = lift . add

-- | short-circuit computation because it cannot continue further
-- like return, but for Left's rather than Right's
-- Compare @err@ with the Alternative instance for ExceptT, which returns a successful value or a monoidal accumulation of failures:
-- @runExcept $ err ["uh-oh!"] <|> err ["nono!"]@ --> Left ["uh-oh!", "nono!"]
-- @runExcept $ err ["uh-oh!"] <|> return 4 <|> err ["nono!"]@ --> Right 4
-- @Data.Foldable.asum@ is a good way of trying @ExceptT@'s until one succeeds, logging errors along the way.
err :: Applicative m => e -> ExceptT e m a
err = ExceptT . pure . Left

-- | @lift look@
getWarning :: (Monoid w, Monad m) => BugT e w m w
getWarning = lift look

-- | error that, along with a user-given description of the error, tries to append a detailed description. I'm not sure whether to use System.IO.Error's @userError@, @mkIOError@, and @annotateIOError@, to return an @IOError@, or keep within @ExceptT@.
-- These error descriptions are better than nothing, but are quite unhelpful (e.g. "filesystem object already exists" - which one?)
-- please always include descriptive error messages that specify concrete values (e.g. song.mp3 rather than "file" or "a file") that pertain to the nature of the program you're writing. If you fail to account for something, these extra messages may be helpful
orError :: (MonadCatch m, Semigroup str, IsString str) => ExceptT str m a -> str -> ExceptT str m a
action `orError` msg = catch action $ \e ->
    let desc = if | isPermissionError e    -> " (permission error)" -- TODO: check UID against owner's UID of resource we're trying to access; find exactly where and what the permission (or owner) mismatch is
                  | isAlreadyExistsError e -> " (filesystem object already exists)"
                  | isDoesNotExistError e  -> " (filesystem object doesn't exist)"
                  | isAlreadyInUseError e  -> " (filesystem object already in use)"
                  | isFullError e          -> " (device we're trying to write to is already full)"
                  | isEOFError e           -> " (end of file reached too early (this is a programming error))"
                  | isIllegalOperation e   -> " (\"illegal operation\")"
                  | otherwise              -> ""
    in err $ msg <> desc

-- | Printing functions. Useful for CLI apps.

-- | Log warning messages. If the logging function is successful, clear the calling monad's buffer and return the output of the logger.
-- If unsuccessful, return the logger's error in the original monad, leaving the accumulation buffer unmodified.
-- Some simple examples: @dumpWarn (liftIO . putStrLn) ⋯@, or @dumpWarn (liftIO . mapM_ hPutStrLn stderr) ⋯@
-- A more complex scenario, in which we're logging to an external database over a network connection:
-- @
-- import qualified Data.Text as T'
-- import Data.Text (Text)
-- import qualified Data.Text.IO as TIO
--
-- example :: Floating n => BugT String (Sum Int) IO n
-- example = do
--     mapM_ (\n -> when (n % 35 == 0) $ warn (Sum n)) [1..100] -- yeah, it's a kinda silly example
--     (result, log) <- dumpWarn (intoDatabase . getSum)
--     when (not $ null log) (liftIO . TIO.putStrLn $ "WARNING: " <> log)
--     either (\e -> liftIO . putStrLn $ "Fatal error in logging warnings to database! " <> e) return result
--     s <- getSum <$> getWarning
--     when (s == 0) (err "Can't divide by zero!")
--     return (fromIntegral s / pi)
--     where
--         intoDatabase :: (Show n, Num n) => n -> BugT Text Text IO ()
--         intoDatabase = do
--             when (n > fromIntegral 100) $ warn "You're pushing a number greater than 100?"
--             maybe (return ()) err $ sql "INSERT INTO numbers (value) VALUES (" <> T'.pack (show n) <> ")" -- hypothetical function sql :: Text -> IO (Maybe Text), where the Text is an error message
-- @
-- dumpWarn is designed to return the results of the logging function in the original monad (instead of @squash@ing them together) so that you can keep each dumpWarn call separate, in case you want to dump to multiple data stores, where there's expectation that some dumps may fail.
-- btw, the above example is hypothetical and maybe wouldn't compile, but it should give a good idea of how to use the BugT monad and dumpWarn
dumpWarn :: (Monoid ω, Monoid w, Monad m) => (w -> BugT η ω m a) -> BugT e w m (Either η a, ω)
dumpWarn f = lift . lift $ runBugT getWarning >>= runBugT . f . snd
-- dumpWarn f = morphism239 (lift . lift) runBugT getWarning (f . snd)
-- TODO: note that this does not work because the first runBugT is not the same type as the latter runBugT, due to monomorphism restriction. We need to identify exactly the most general type signature in order to use morphism239.

{-
-- morphism239 performs a composition of functions on monad transformers that share a common inner monad and constructor but have different parameters, e.g. @State Text m@ and @State String m@
-- @z@ is a "zipping" function, e.g. @lift . lift@
-- @x@ is an "extraction" function, e.g. @runStateT@
-- @f@ and @g@ are just some functions
morphism239 :: (Monad m) => (m a -> t1) -> (t2 -> m a) -> t2 -> (a -> t2) -> t1
morphism239 z x f g = z (x f >>= x . g)
-}

-- | prints a StringLike error message on failure; returns m () on success
stderrOnFail :: (StringLike s, MonadIO m) => ExceptT s m a -> m ()
stderrOnFail = (\case Left e -> liftIO $ hPutStrLn stderr (toString e); _ -> return ()) <=< runExceptT

-- | prints a StringLike error message on failure; then exits with ExitSuccess or ExitFailure
-- useful in definiton of main
stderrOnFailAndExit :: (StringLike s, MonadIO m) => ExceptT s m a -> m ()
stderrOnFailAndExit = (\case Left e -> liftIO $ hPutStrLn stderr (toString e) >> exitFailure; _ -> liftIO exitSuccess) <=< runExceptT

-- | print all collected errors, if any
-- for use with dumpWarn
toStderr :: (StringLike s, MonadIO m) => [s] -> BugT () () m ()
toStderr = liftIO . mapM_ (hPutStrLn stderr . toString)

-- | lift a Maybe into Either
liftME :: l -> Maybe a -> Either l a
liftME l Nothing = Left l
liftME _ (Just x) = Right x
