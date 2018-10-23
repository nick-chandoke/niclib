-- | Lookup keys in a lookup table. Collects keys that either aren't present or fail to parse. Useful for its @Applicative@ instance.
-- (A very thorough) example:
-- @
-- data C = C String String String deriving Show
-- data D = D String Int String deriving Show
-- 
-- table1 = pure [("b", "value")] -- missing keys a and c (used for parsing into C)
-- table2 = pure [("b", "value"), ("a", "valA"), ("c", "valC")] -- has all keys (used for parsing into C and D)
-- table3 = pure [("b", "value"), ("a", "35"), ("c", "valC")] -- has all keys, and a is an integer like it should be (used for parsing into D)
-- table4 = pure [("a", "3i5")] -- missing keys b and c, and a is not even integer like it should be (used for parsing into D)
-- 
-- parseInt v = liftME (ParseError v "not an integer") $ readMaybe v
-- lkup = Identity <% P.lookup
-- testFn = \table -> runIdentity $ runLookupT (C <$> lookup "a" pure <*> lookup "b" pure <*> lookup "c" pure) table lkup -- parse into C
-- testFnWParse = \table -> runIdentity $ runLookupT (D <$> lookup "b" pure <*> lookup "a" parseInt <*> lookup "c" pure) table lkup -- parse into D
-- @
-- Then, trying to construct C:
-- @testFn table1@ --> Left (fromList [Undefined {varName = "a"},Undefined {varName = "c"}])
-- @testFn table2@ --> Right (C "valA" "value" "valC")
--
-- Trying to construct D:
-- @testFnWParse table2@ --> Left (fromList [ParseError {varName = "valA", cause = "not an integer"}])
-- @testFnWParse table3@ --> Right (D "value" 35 "valC")
-- @testFnWParse table4@ --> Left (fromList [Undefined {varName = "b"},Undefined {varName = "c"},ParseError {varName = "3i5", cause = "not an integer"}])
module Control.Monad.Trans.Lookup
( LookupT (..)
, lookup
, ParseError (..)
) where

-- base
import Control.Applicative
import Data.Either (either)
import Prelude hiding (lookup, Applicative(..))
import qualified Data.Bifunctor as BiF

-- TODO: comment-out after doing example
import qualified Prelude as P
import Data.Functor.Identity
import NicLib.NStdLib (readMaybe)
import NicLib.Errors (liftME)

import qualified Data.Set as S -- containers

-- NicLib
import NicLib.NStdLib ((<%), cT)

newtype LookupT s m a =
    LookupT {runLookupT :: m s -- ^ the structure to search through (e.g. @[(String,String)]@ or @Map String String@)
                      -> (String -> s -> m (Maybe String)) -- ^ the lookup function (e.g. @\k -> Identity $ Prelude.lookup k@, or @\k -> Identity . Map.lookup k@)
                      -> m (Either (S.Set ParseError) a)
           }

-- | For whether an environment variable is not defined, or fails to parse into a desired Config field; String of ParseError is description of parse error
data ParseError = Undefined -- ^ denotes that a variable could not be found
                    {varName :: String}
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
    compare (ParseError x d1) (ParseError y d2) = compare x y

lookup :: Monad m => String -> (String -> Either ParseError a) -> LookupT s m a
lookup k p = LookupT $ \ms l -> ms >>= \s -> l k s >>= \case
        Nothing -> pure . Left . S.singleton $ Undefined k
        Just v -> pure $ BiF.first S.singleton (p v)

instance Functor m => Functor (LookupT s m) where
    fmap f (LookupT {runLookupT}) = LookupT $ (fmap . fmap) f <% runLookupT

instance Applicative m => Applicative (LookupT s m) where
    pure x = LookupT $ \_ _ -> pure (Right x)
    liftA2 f (LookupT {runLookupT = r1}) (LookupT {runLookupT = r2}) = LookupT $ \s l -> cT g (,) <$> r1 s l <*> r2 s l
        where
            g = \case
                (Left set1, Left set2) -> Left $ set1 <> set2
                (Right _, Left set) -> Left set
                (Left set, Right _) -> Left set
                (Right x, Right y) -> Right $ f x y
