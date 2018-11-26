{-# OPTIONS_GHC -Wno-orphans #-}

-- TODO: link to nixys-server-box haddocks when available on nicholaschandoke.me
-- TODO: rewrite in terms of Capabilities? <https://github.com/tweag/capability> (will push niclib to requiring GHC 8.6, however)
-- AccumT's are used mostly as WriterT's herein; you'll see a lot of things in the vein of runAccumT (add ... >> ...) mempty
-- | Handle logging, filtering for, and printing of warnings and short-circuiters (/neither <https://wiki.haskell.org/Error_vs._Exception errors nor exceptions>/). The main object of this module is 'BugT', which collects warnings and short-circuits on errors. BugT can be used for things that aren't strictly errors nor warnings; see nixys-server-box for an example.
--
-- === Exceptions/Disclaimer
--
-- You /can/ use this module for exception handling, e.g. accounting for expected 'IOError''s e.g. file not found or permissions errors. Anything goes in non-production personal scripts. However, for production code, if you aren't already familiar with exception handling, please read <https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell Snoyman's FP Complete blog post> to get started.
-- @Errors@ sometimes blurs the line between exception handling and BugT-like behavior, for instance in its use of @MonadThrow@. Some functions herein can be used for either BugT stuff or exception handling, or both.
module NicLib.Errors
( BugT
, bugT
, runBugT
, mapBugAccum
, err
, getWarning
, mtell
, describeIOError
, toError
, toWarning
, warn
, liftME
, withBugAccum
-- * Printing functions
, dumpWarn
, toStderr
, stderrOnFail
, stderrOnFailAndExit
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

-- | Short-circuit computation because it cannot continue further. Basically 'throwE' but with a better name, and works for Applicatives.
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

-- | Get a synopsis of an @IOError@. For example, converts @isPermissionError@ to "Permission error".
describeIOError :: IOError -> String
describeIOError e = 
    if | isPermissionError e    -> "Permission error"
       | isAlreadyExistsError e -> "Filesystem object already exists"
       | isDoesNotExistError e  -> "Filesystem object doesn't exist"
       | isAlreadyInUseError e  -> "Filesystem object already in use"
       | isFullError e          -> "Device we're trying to write to is already full"
       | isEOFError e           -> "End of file reached too early"
       | isIllegalOperation e   -> "Illegal operation"
       | otherwise              -> "Unknown IO Error"

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
