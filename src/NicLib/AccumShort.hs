{-# OPTIONS_GHC -Wno-orphans #-}

-- TODO: encode via ChronicleT <http://hackage.haskell.org/package/these> instead of ExceptT (AccumT)? Is there benefit (elegance or efficiency, part. in regard to optimizability)?
-- TODO: separate IO-related stuff from AccumShort stuff
-- TODO: link to nixys-server-box haddocks when available on nicholaschandoke.me
-- TODO: rewrite in terms of Capabilities? <https://github.com/tweag/capability> (will push niclib to requiring GHC 8.6, however)
--
-- | Handle logging, filtering for, and printing of warnings and short-circuiters (/neither <https://wiki.haskell.org/Error_vs._Exception errors nor exceptions>/). The main object of this module is 'AccumShort', which collects warnings and short-circuits on errors. @AccumShort@ can be used for things that aren't strictly errors nor warnings; see nixys-server-box for an example.
--
-- *Note*: this module overrides @Control.Monad.Trans.accum@ with a lifted version of that.
--
-- === Comparision with Exceptions
--
-- You should not use this module for exception handling in IO, <https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell if you didn't know>. However, you may wish to use the <https://www.stackage.org/haddock/lts-12.18/safe-exceptions-0.1.7.0/Control-Exception-Safe.html#t:MonadThrow MonadThrow> abstraction, which reifies to @ExceptT@ (therefore @AccumShort@ too,) @IO@, and others.
--
-- Here's an example of using exceptions:
--
-- @
-- {-# language LambdaCase #-}
-- import Control.Monad.Safe (throwString) -- package safe-exceptions
--
-- data T = T String deriving (Show, Typeable)
-- instance Exception T
--
-- example :: Either SomeException -> IO ()
-- example = \case
--     Left e ->
--         putStrLn $ "Caught " <> case fromException e of
--             Nothing -> "non-T msg: " <> show e
--             Just (T msg) -> "T msg: " <> msg
--     _ -> pure ()
-- @
--
-- >>> example $ throwM (T "oh, no!")
-- Caught T msg: oh, no!
--
-- >>> example $ throwString "ho-h, boy!"
-- @
-- Caught non-T msg: Control.Exception.Safe.throwString called with:
--
-- ho-h, boy!
-- Called from:
--   throwString (<interactive>:40:11 in interactive:Ghci14)
-- @
--
-- Notes:
--
-- 1. @Identity@ does not instance @MonadThrow@; thus you must use @Either _ _@ rather than @ExceptT _ Identity _@.
-- 2. @(Either SomeException)@ instances @MonadThrow@; thus the argument to @Left@ must be of @SomeException@ type. That's why we need to use @fromException@ here.
-- 3. The only reason that @example@ accepts an @Either SomeException@ rather than a @MonadThrow m => m a@ is that it pattern-matches its argument to @Either@'s constructors; this implicitly reifies its type. You may wish to have a more general handler, such as one that handles @Alternative@s.
-- 4. @fromException@ is unambiguous only becasue I pattern-matched against @T@'s constructor; often one will need to explicitly specify the type of @fromException e@
-- 5. As you can see, @throwString@, despite how easy it is to use, does not have the prettiest output. Dat callstack tho :3c
module NicLib.AccumShort
( AccumShort
, mkAccumShort
, runAS
, mapAS
, short
, getAccum
, madd
, toChoice
, toAccum
, accum
, liftME
, withAC
-- * IO stuff that shouldn't be in this module
, describeIOError
, stderrOnFail
, stderrOnFailAndExit
-- * Printing functions
, dumpAccum
, toStderr
) where

import Control.Arrow (second)
import Control.Exception.Safe (throwString) -- safe-exceptions
import Control.Monad ((<=<))
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Accum hiding (accum)
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
-- >>> runWriter $ madd "hello!" <> pure (Sum 4) <> pure (Sum 20) <> madd " hoho"
-- (Sum {getSum = 24},"hello! hoho")
--
-- I don't remember why I wrote this. I'm leaving this orphan instance here anyway, just in case.
-- I mean I don't even use @WriterT@ anymore; I prefer @AccumT@.
instance (Applicative m, Monoid s, Monoid a) => Semigroup (WriterT s m a) where
    (<>) = morphism46 WriterT runWriterT (<>)

instance (Applicative m, Monoid s, Monoid a) => Monoid (WriterT s m a) where
    mempty = pure mempty
    mappend = (<>)

-- | Monoid-compatible 'add': returns @mempty@ rather than @()@
madd :: (Monoid s, Monoid a, Monad m) => s -> AccumT s m a
madd = const (pure mempty) <=< add

-- | Combination of the 'ExceptT' and 'AccumT' monads. Allows logging warning messages, or exiting on errors.
--
-- Encourages assuming that anything that could generate a warning could, sometime through revisions of the program, produce an error,
-- and conversely that anything that produces a fatal error is error-prone enough that it may output some helpful warning messages.
type AccumShort e w m = ExceptT e (AccumT w m)

-- | Unwrap an @AccumShort@
runAS :: Monoid w => AccumShort e w m a -> m (Either e a, w)
runAS = flip runAccumT mempty . runExceptT

-- | Create an @AccumShort@
mkAccumShort ::  m (Either e a, w) -> AccumShort e w m a
mkAccumShort = ExceptT . AccumT . const

-- | Push warning to log
accum :: (Monoid w, Monad m) => w -> AccumShort e w m ()
accum = lift . add

withAC :: (Monad m, Monoid w, Monoid w') => (w -> w') -> AccumShort e w m a -> AccumShort e w' m a
withAC f = mapAS (mkAccumShort . pure . second f)

mapAS :: (Monad m, Monoid w, Monoid w') => ((Either e a, w) -> AccumShort e w' m a) -> AccumShort e w m a -> AccumShort e w' m a
mapAS f = mkAccumShort . morphism240 runAS f runAS

-- | Short-circuit computation because it cannot continue further. Basically 'throwE' but with a better name, and works for Applicatives.
--
-- Like @pure@, but for Left's rather than Right's.
--
-- Remember that @AccumShort@ is a newtype around ExceptT; this means that short can lift an @e@ into a @AccumShort e w m a@ as well as an @ExceptT e m a@
--
-- Compare @short@ with the Alternative instance for ExceptT, which returns a successful value or a monoidal accumulation of failures:
--
-- >>> runExcept $ short ["uh-oh!"] <|> short ["nono!"]
-- Left ["uh-oh!", "nono!"]
--
-- >>> runExcept $ short ["uh-oh!"] <|> pure 4 <|> short ["nono!"]
-- Right 4
--
-- 'Data.Foldable.asum' is a good way of trying @ExceptT@'s until one succeeds, logging all failures if whole computation fails.
short :: Applicative m => e -> ExceptT e m a
short = ExceptT . pure . Left

-- | @getAccum = lift look@
getAccum :: (Monoid w, Monad m) => AccumShort e w m w
getAccum = lift look

-- | Promote a warning to an error. Not sure if this ever makes sense. See 'toAccum''s rationale. I can't see that rationale working backwards.
--
-- Regardless, if you need it, here it is!
toChoice :: (Monad m, Eq e, Monoid e, Monoid w) => AccumT e m a -> AccumShort e w m a
toChoice = mkAccumShort . morphism240 (flip runAccumT mempty) (\(a, e) -> if e == mempty then pure a else short e) runAS

-- | Demote an error to a warning. Consider that although a single ExceptT may fail or succeed, when running multiple different ExceptT's all toward the same purpose, the success of any one of them means success. Thus all the failed attempts could be mere warnings (i.e. logged,) or disregarded.
--
-- Returns Nothing if a warning was added; returns Just the Right value if there was one.
toAccum :: (Monoid w, Monad m) => ExceptT w m a -> AccumShort e w m (Maybe a)
toAccum = lift . ((\case Left e -> add e >> pure Nothing; Right x -> pure $ Just x) <=< lift . runExceptT)

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

-- | Try doing something with the accumulation object. If the attempted function is successful, sets the calling monad's accumulation object to @mempty@ and returns the output of the given function.
--
-- If unsuccessful, return the function's error in the original monad, leaving the accumulation buffer object.
--
-- Although the morphism parameter's codomain is @AccumShort@, you'll usually @lift@ a function that'd usually end in @Identity@, @IO@, or something common.
--
-- Some simple examples: @dumpAccum (liftIO . putStrLn) ⋯@, or @dumpAccum (liftIO . mapM_ hPutStrLn stderr) ⋯@
-- A more complex scenario, in which we're logging to an external database over a network connection:
--
-- @
-- import qualified Data.Text as T'
-- import Data.Text (Text)
-- import qualified Data.Text.IO as TIO
--
-- example :: Floating n =\> AccumShort String (Sum Int) IO n
-- example = do
--     mapM_ (\\n -\> when (n % 35 == 0) $ accum (Sum n)) [1..100] -- yeah, it's a kinda silly example
--     (result, log) \<- dumpAccum (intoDatabase . getSum)
--     when (not $ null log) (liftIO . TIO.putStrLn $ "WARNING: " \<\> log)
--     either (\\e ->\ liftIO . putStrLn $ "Fatal error in logging warnings to database! " \<\> e) pure result
--     s \<- getSum \<$\> getAccum
--     when (s == 0) (short "Can't divide by zero!")
--     pure $ fromIntegral s / pi
--     where
--         intoDatabase :: (Show n, Num n) =\> n -\> AccumShort Text Text IO ()
--         intoDatabase = do
--             when (n \> fromIntegral 100) $ accum "You're pushing a number greater than 100?"
--             maybe (pure ()) short $ sql "INSERT INTO numbers (value) VALUES (" \<\> T'.pack (show n) \<\> ")" -- hypothetical function sql :: Text -\> IO (Maybe Text), where the Text is an error message
-- @
--
-- @dumpAccum@ is designed to return the results of the logging function in the original monad (instead of 'squash'ing them together) so that you can keep each @dumpAccum@ call separate, in case you want to dump to multiple data stores, where there's expectation that some dumps may fail.
dumpAccum :: (Monoid ω, Monoid w, Monad m) => (w -> AccumShort η ω m a) -> AccumShort e w m (Either η a, ω)
dumpAccum f = lift . lift $ morphism240 runAS (f . snd) runAS getAccum -- the first runAS is not the same as the second runAS! They operate in the same monad, but over different types; thus I must specify this general function twice so that it reifies into two different more-specific functions.

-- | prints a StringLike error message on failure; returns @pure ()@ on success
stderrOnFail :: (StringLike s, MonadIO m) => ExceptT s m a -> m ()
stderrOnFail = (\case Left e -> liftIO $ hPutStrLn stderr (toString e); _ -> pure ()) <=< runExceptT

-- | Prints a StringLike error message on failure; then exits with 'ExitSuccess' or 'ExitFailure'
--
-- Useful in definiton of @main@
stderrOnFailAndExit :: (StringLike s, MonadIO m) => ExceptT s m a -> m ()
stderrOnFailAndExit = (\case Left e -> liftIO $ hPutStrLn stderr (toString e) >> exitFailure; _ -> liftIO exitSuccess) <=< runExceptT

-- | Print all collected errors, if any
--
-- For use with 'dumpAccum'
toStderr :: (StringLike s, MonadIO m) => [s] -> AccumShort () () m ()
toStderr = liftIO . mapM_ (hPutStrLn stderr . toString)

-- | Lift a Maybe into Either
liftME :: l -> Maybe a -> Either l a
liftME l Nothing = Left l
liftME _ (Just x) = Right x
