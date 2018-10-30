-- | Lookup keys in a lookup table. Collects keys that either aren't present or fail to parse. Useful for its @Applicative@ instance.
-- (A very thorough) example:
-- @
-- import Control.Monad.Trans.Lookup
-- import Data.Functor.Identity
-- import NicLib.Errors (liftME)
-- import NicLib.NStdLib ((<%), readMaybe)
-- import Prelude hiding (lookup)
-- import qualified Prelude as P
-- 
-- data C = C String String String deriving Show
-- data D = D String Int String deriving Show
-- 
-- table1 = [("b", "value")] -- missing keys a and c (used for parsing into C)
-- table2 = [("b", "value"), ("a", "valA"), ("c", "valC")] -- has all keys (used for parsing into C and D)
-- table3 = [("b", "value"), ("a", "35"), ("c", "valC")] -- has all keys, and a is an integer like it should be (used for parsing into D)
-- table4 = [("a", "3i5")] -- missing keys b and c, and a is not even integer like it should be (used for parsing into D)
-- 
-- parseInt v = liftME (ParseError v "not an integer") $ readMaybe v
-- 
-- lkup :: [(String, String)] -> String -> Lookup String
-- lkup table i = lookup id (pure . pure) (Identity <% P.lookup) table i
-- 
-- testFn t = runLookup $ C -- parse into C from table t
--         <$> lkup t "a"
--         <*> lkup t "b"
--         <*> lkup t "c"
-- 
-- testFnWParse t = runLookup $ D
--               <$> lkup t "b"
--               <*> lookup id (pure . parseInt) (Identity <% P.lookup) t "a"
--               <*> lkup t "c" -- parse into D
-- @
-- @testFn table1@ --> Left (fromList [Undefined {varName = "a"},Undefined {varName = "c"}])
-- @testFn table2@ --> Right (C "valA" "value" "valC")
--
-- @testFnWParse table2@ --> Left (fromList [ParseError {varName = "valA", cause = "not an integer"}])
-- @testFnWParse table3@ --> Right (D "value" 35 "valC")
-- @testFnWParse table4@ --> Left (fromList [Undefined {varName = "b"},Undefined {varName = "c"},ParseError {varName = "3i5", cause = "not an integer"}])
module Control.Monad.Trans.Lookup
( LookupT (..)
, runLookup
, Lookup
, lookup
, ParseError (..)
, lookupEnv
, lookupFile
, lookupFileBS
, lookupFileBSL
, lookupFileBSC
, lookupFileBSLC
, lookupFileT
, lookupFileTL
) where

-- base
import Control.Applicative
import Data.Bool (bool)
import Data.Functor.Identity
import Prelude hiding (lookup, Applicative(..))
import System.Directory (doesFileExist)
import qualified Data.Bifunctor as BiF
import qualified Prelude as P

import qualified Data.Set as S -- containers

-- bytestring
import qualified Data.ByteString as BS'
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as BSC'
import qualified Data.ByteString.Lazy.Char8 as BSC

-- text
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Text as T'
import qualified Data.Text.Lazy as T

import NicLib.NStdLib ((<%), morphism239) -- NicLib

import Control.Monad.IO.Class
import Control.Monad.Trans.Class

newtype LookupT m a = LookupT { runLookupT :: m (Either (S.Set ParseError) a) } deriving Functor
type Lookup a = LookupT Identity a

runLookup :: Lookup a -> Either (S.Set ParseError) a
runLookup = runIdentity . runLookupT

-- | For whether an environment variable is not defined, or fails to parse into a desired Config field; String of ParseError is description of parse error
data ParseError = Undefined -- ^ denotes that a variable could not be found
                    { varName :: String }
                | ParseError -- ^ denotes that a variable could not be parsed from a string
                    { varName :: String
                    , cause :: String -- ^ description of why the parsing failed
                    }
                deriving (Eq, Show)

-- | In a set of ParseError's, I want Undefined variables to be mentioned before variables that failed to parse
instance Ord ParseError where
    compare (Undefined x) (Undefined y) = compare x y
    compare (Undefined _) (ParseError _ _) = LT
    compare (ParseError _ _) (Undefined _) = GT
    compare (ParseError x _) (ParseError y _) = compare x y

-- | The primary way to create a @LookupT@ object
lookup :: (Monad m, Show i)
       => (i -> String) -- ^ for collecting identifiers as error output strings
       -> (a -> m (Either ParseError b)) -- ^ function that parses a looked-up value into a final value (e.g. @read \@Int@)
       -> (i -> s -> m (Maybe a)) -- ^ the lookup function (e.g. @\k -> Identity $ Prelude.lookup k@, or @\k -> Identity . Map.lookup k@, or @\f -> doesFileExist f >>= bool (pure Nothing) (readFile f)@)
       -> s -- ^ the structure to search through (e.g. @IO [(String,String)]@ or @Identity (Map String String)@)
       -> i -- ^ identifier to lookup
       -> LookupT m b
lookup toStr p l s i = LookupT $ l i s >>= \case
        Nothing -> pure . Left . S.singleton $ Undefined (toStr i)
        Just a -> BiF.first S.singleton <$> p a

instance Applicative m => Applicative (LookupT m) where
    pure = LookupT . pure . pure
    liftA2 f (LookupT l1) (LookupT l2) = LookupT (liftA2 g l1 l2)
        where
            g (Left set1) (Left set2) = Left (set1 <> set2)
            g (Right _)   (Left set)  = Left set
            g (Left set)  (Right _)   = Left set
            g (Right x)   (Right y)   = Right (f x y)

-- morphism239 :: (LookupT m a -> m (Either (Set ParseError) a))
--             -> (Either (Set ParseError) a -> LookupT m x)
--             -> (LookupT m x -> m (Either (Set ParseError) x))
--             -> LookupT m a
--             -> m (Either (Set ParseError) x)
instance Monad m => Monad (LookupT m) where
    -- | Short-circuiting like @Either@; does not accumulate a set like the Applicative instance does.
    -- So why instance Monad? Only so that I can instance @MonadIO@, which I needed for some functions.
    l >>= f = LookupT $ morphism239 runLookupT g runLookupT l where
        g = \case
            Left  s -> LookupT $ pure (Left s)
            Right a -> f a

instance MonadIO m => MonadIO (LookupT m) where
    liftIO = LookupT . liftIO . fmap pure

instance MonadTrans LookupT where
    lift = LookupT . fmap pure

-- Common Lookup Functions --

lookupEnv :: Monad m => [(String, b)] -> String -> LookupT m b
lookupEnv = lookup id (pure . pure) (pure <% P.lookup)

lookupFileCommon :: (String -> IO b) -> String -> LookupT IO b
lookupFileCommon f = lookup id (pure . pure) (\x _ -> doesFileExist x >>= bool (pure Nothing) (pure <$> f x)) undefined

lookupFile :: String -> LookupT IO String
lookupFile = lookupFileCommon readFile

lookupFileBS :: String -> LookupT IO BS'.ByteString
lookupFileBS = lookupFileCommon BS'.readFile

lookupFileBSL :: String -> LookupT IO BS.ByteString
lookupFileBSL = lookupFileCommon BS.readFile

lookupFileBSC :: String -> LookupT IO BSC'.ByteString
lookupFileBSC = lookupFileCommon BSC'.readFile

lookupFileBSLC :: String -> LookupT IO BSC.ByteString
lookupFileBSLC = lookupFileCommon BSC.readFile

lookupFileT :: String -> LookupT IO T'.Text
lookupFileT = lookupFileCommon TIO.readFile

lookupFileTL :: String -> LookupT IO T.Text
lookupFileTL = lookupFileCommon TLIO.readFile
