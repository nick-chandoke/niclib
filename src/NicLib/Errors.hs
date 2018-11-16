{-# OPTIONS_GHC -Wno-orphans #-}

-- TODO: rewrite in terms of Capabilities <https://github.com/tweag/capability> (will push niclib to requiring GHC 8.6, however)
--   or use exceptions instead of error-logging monads. I'll have to consider the roles/uses of this module in order to decide such things.
-- AccumT's are used mostly as WriterT's herein; you'll see a lot of things in the vein of runAccumT (add ... >> ...) mempty
-- | Handle logging, filtering for, and printing, errors.
--
-- Please note that, especially for multi-threaded programs, you should /not/ use error-logging monads to catch exceptions! If you aren't familiar with this, you can easily find material about it online. This being said, I've found use for BugT in server request routing, to handle non-erroroneous exceptional behaviors. (read https://wiki.haskell.org/Error_vs._Exception if that's confusing.) Keep in mind that "errors" are still useful in pure code, and that "error" monads are better called "exit-early" or "short-circuit" monads, since that's their literal behavior, regardless of the purpose!
module NicLib.Errors
( BugT
, bugT
, runBugT
, mapBugAccum
, err
, getWarning
, mtell
, orError
, orLog
, orThrow
, toError
, toWarning
, warn
, with -- for some reason I get a parse error when I tried naming it 'by'
, liftME
, withBugAccum
-- * Printing functions
, dumpWarn
, stderrOnFail
, stderrOnFailAndExit
, toStderr
) where

import Control.Arrow (second)
import Control.Exception.Safe (throwString) -- safe-exceptions
import Control.Monad ((<=<))
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Accum
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer
import Data.ListLike (StringLike)
import Data.ListLike.String hiding (fromString)
import Data.Text (Text) -- text
import NicLib.NStdLib
import System.Exit
import System.IO (stderr, hPutStrLn)
import System.IO.Error
import qualified Data.Text as T' -- text

-- | Applies (\<\>) to monoidal writer values and accumulations:
-- >>> runWriter $ mtell "hello!" <> pure (Sum 4) <> pure (Sum 20) <> mtell " hoho"
-- (Sum {getSum = 24},"hello! hoho")
--
-- I don't remember why I wrote this. I'm leaving this orphan instance here anyway, just in case.
-- I mean I don't even use @WriterT@ anymore; I prefer @AccumT@.
instance (Applicative m, Monoid s, Monoid a) => Semigroup (WriterT s m a) where
    (<>) = morphism46 WriterT runWriterT (<>)

instance (Applicative m, Monoid s, Monoid a) => Monoid (WriterT s m a) where
    mempty = pure mempty
    mappend = (<>)

-- | 'Control.Monad.Trans.Writer.tell', but monoid-compatible (rather than returning @()@, returns @mempty@; thus, dosen't affect value, but in different way)
mtell :: (Monoid s, Monoid a, Monad m) => s -> WriterT s m a
mtell = mempty <=< tell

-- | Combination of the 'ExceptT' and 'AccumT' monads. Allows logging warning messages, or exiting on errors.
--
-- Encourages assuming that anything that could generate a warning could, sometime through revisions of the program, produce an error,
-- and conversely that anything that produces a fatal error is error-prone enough that it may output some helpful warning messages.
type BugT e w m = ExceptT e (AccumT w m)

-- | Unwrap a BugT
runBugT :: Monoid w => BugT e w m a -> m (Either e a, w)
runBugT = flip runAccumT mempty . runExceptT

-- | Create a BugT
bugT ::  m (Either e a, w) -> BugT e w m a
bugT = ExceptT . AccumT . const

-- | Push warning to log
warn :: (Monoid w, Monad m) => w -> BugT e w m ()
warn = lift . add

withBugAccum :: (Monad m, Monoid w, Monoid w') => (w -> w') -> BugT e w m a -> BugT e w' m a
withBugAccum f = mapBugAccum (bugT . pure . second f)

mapBugAccum :: (Monad m, Monoid w, Monoid w') => ((Either e a, w) -> BugT e w' m a) -> BugT e w m a -> BugT e w' m a
mapBugAccum f = bugT . morphism240 runBugT f runBugT

-- | Short-circuit computation because it cannot continue further.
--
-- Like @pure@, but for Left's rather than Right's.
--
-- Remember that BugT is a newtype around ExceptT; this means that err can lift an @e@ into a @BugT e w m a@ as well as an @ExceptT e m a@
--
-- Compare @err@ with the Alternative instance for ExceptT, which returns a successful value or a monoidal accumulation of failures:
--
-- >>> runExcept $ err ["uh-oh!"] <|> err ["nono!"]
-- Left ["uh-oh!", "nono!"]
--
-- >>> runExcept $ err ["uh-oh!"] <|> pure 4 <|> err ["nono!"]
-- Right 4
--
-- 'Data.Foldable.asum' is a good way of trying @ExceptT@'s until one succeeds, logging all failures if whole computation fails.
err :: Applicative m => e -> ExceptT e m a
err = ExceptT . pure . Left

-- | @getWarning = lift look@
getWarning :: (Monoid w, Monad m) => BugT e w m w
getWarning = lift look

-- | Promote a warning to an error. Not sure if this ever makes sense. See 'toWarning''s rationale. I can't see that rationale working backwards.
--
-- Regardless, if you need it, here it is!
toError :: (Monad m, Eq e, Monoid e, Monoid w) => AccumT e m a -> BugT e w m a
toError = bugT . morphism240 (flip runAccumT mempty) (\(a, e) -> if e == mempty then pure a else err e) runBugT

-- | Demote an error to a warning. Consider that although a single ExceptT may fail or succeed, when running multiple different ExceptT's all toward the same purpose, the success of any one of them means success. Thus all the failed attempts could be mere warnings (i.e. logged,) or disregarded.
--
-- Returns Nothing if a warning was added; returns Just the Right value if there was one.
toWarning :: (Monoid w, Monad m) => ExceptT w m a -> BugT e w m (Maybe a)
toWarning = lift . ((\case Left e -> add e >> pure Nothing; Right x -> pure $ Just x) <=< lift . runExceptT)

-- | On exception catch, do something with both a user-given description of the error, and a detailed description of the problem. I'm not sure whether to use 'System.IO.Error.userError', @mkIOError@, and @annotateIOError@, to return an @IOError@.
--
-- Use with the @with@ function (or if you're using @Lucid@ man-up and use @$@ instead.) Example:
--
-- @liftIO (readFile "not here!") `orLog` "Error reading file 'nothere'!" `with` err@
--
-- One may use @err@ or any method of your preference that works. Remember that the function you supply via @with@ must instance @MonadCatch@! Thus, for instance, @warn@ will not work, unless someone instances @MonadCatch m => MonadCatch (AccumT w m)@.
--
-- These error descriptions are better than nothing, but are quite unhelpful (e.g. "filesystem object already exists" - if dealing with multiple files, you don't know which one!)
--
-- Please always include descriptive error messages that specify concrete values (e.g. song.mp3 rather than "file" or "a file") that pertain to the nature of the program you're writing. If you fail to account for something, these extra messages may be helpful
orLog :: (MonadCatch m) => m a -> Text -> (Text -> m a) -> m a
action `orLog` msg = \l -> catch action $ \e ->
    let desc = if | isPermissionError e    -> " (permission error)" -- TODO: check UID against owner's UID of resource we're trying to access; find exactly where and what the permission (or owner) mismatch is
                  | isAlreadyExistsError e -> " (filesystem object already exists)"
                  | isDoesNotExistError e  -> " (filesystem object doesn't exist)"
                  | isAlreadyInUseError e  -> " (filesystem object already in use)"
                  | isFullError e          -> " (device we're trying to write to is already full)"
                  | isEOFError e           -> " (end of file reached too early (this is a programming error))"
                  | isIllegalOperation e   -> " (\"illegal operation\")"
                  | otherwise              -> ""
    in l $ msg <> desc

-- | @orError = orLog with err@
orError :: (MonadCatch m) => ExceptT Text m a -> Text -> ExceptT Text m a
orError = orLog %> ($err)

-- | Calls 'Control.Exception.Safe.throwString' on an error
-- e.g. @mzero \`orThrow\` "mzero error"@
orThrow :: MonadCatch m => m a -> Text -> m a
orThrow = orLog %> flip with (throwString . T'.unpack)

-- | @with = $@.
with :: (a -> b) -> a -> b
with = ($)

-- | Log warning messages. If the logging function is successful, clear the calling monad's buffer and return the output of the logger.
--
-- If unsuccessful, return the logger's error in the original monad, leaving the accumulation buffer unmodified.
--
-- Some simple examples: @dumpWarn (liftIO . putStrLn) ⋯@, or @dumpWarn (liftIO . mapM_ hPutStrLn stderr) ⋯@
-- A more complex scenario, in which we're logging to an external database over a network connection:
--
-- @
-- import qualified Data.Text as T'
-- import Data.Text (Text)
-- import qualified Data.Text.IO as TIO
--
-- example :: Floating n =\> BugT String (Sum Int) IO n
-- example = do
--     mapM_ (\\n -\> when (n % 35 == 0) $ warn (Sum n)) [1..100] -- yeah, it's a kinda silly example
--     (result, log) \<- dumpWarn (intoDatabase . getSum)
--     when (not $ null log) (liftIO . TIO.putStrLn $ "WARNING: " \<\> log)
--     either (\\e ->\ liftIO . putStrLn $ "Fatal error in logging warnings to database! " \<\> e) pure result
--     s \<- getSum \<$\> getWarning
--     when (s == 0) (err "Can't divide by zero!")
--     pure $ fromIntegral s / pi
--     where
--         intoDatabase :: (Show n, Num n) =\> n -\> BugT Text Text IO ()
--         intoDatabase = do
--             when (n \> fromIntegral 100) $ warn "You're pushing a number greater than 100?"
--             maybe (pure ()) err $ sql "INSERT INTO numbers (value) VALUES (" \<\> T'.pack (show n) \<\> ")" -- hypothetical function sql :: Text -\> IO (Maybe Text), where the Text is an error message
-- @
--
-- @dumpWarn@ is designed to return the results of the logging function in the original monad (instead of 'squash'ing them together) so that you can keep each @dumpWarn@ call separate, in case you want to dump to multiple data stores, where there's expectation that some dumps may fail.
dumpWarn :: (Monoid ω, Monoid w, Monad m) => (w -> BugT η ω m a) -> BugT e w m (Either η a, ω)
dumpWarn f = lift . lift $ morphism240 runBugT (f . snd) runBugT getWarning -- the first runBugT is not the same as the second runBugT! They operate in the same monad, but over different types; thus I must specify this general function twice so that it reifies into two different more-specific functions.

-- | prints a StringLike error message on failure; returns m () on success
stderrOnFail :: (StringLike s, MonadIO m) => ExceptT s m a -> m ()
stderrOnFail = (\case Left e -> liftIO $ hPutStrLn stderr (toString e); _ -> pure ()) <=< runExceptT

-- | Prints a StringLike error message on failure; then exits with 'ExitSuccess' or 'ExitFailure'
--
-- Useful in definiton of @main@
stderrOnFailAndExit :: (StringLike s, MonadIO m) => ExceptT s m a -> m ()
stderrOnFailAndExit = (\case Left e -> liftIO $ hPutStrLn stderr (toString e) >> exitFailure; _ -> liftIO exitSuccess) <=< runExceptT

-- | Print all collected errors, if any
--
-- For use with 'dumpWarn'
toStderr :: (StringLike s, MonadIO m) => [s] -> BugT () () m ()
toStderr = liftIO . mapM_ (hPutStrLn stderr . toString)

-- | Lift a Maybe into Either
liftME :: l -> Maybe a -> Either l a
liftME l Nothing = Left l
liftME _ (Just x) = Right x
