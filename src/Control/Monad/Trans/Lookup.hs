-- | Applicative parsing for structures that embed some kind of lookup table. Rather than short-circuiting, @LookupT@ collects keys that either aren't present or fail to parse.
--
-- (A very thorough) example:
--
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
--         \<$\> lkup t "a"
--         \<*\> lkup t "b"
--         \<*\> lkup t "c"
-- 
-- testFnWParse t = runLookup $ D
--               \<$\> lkup t "b"
--               \<*\> lookup id (pure . parseInt) (Identity <% P.lookup) t "a"
--               \<*\> lkup t "c" -- parse into D
-- @
--
-- >>> testFn table1
-- Left (fromList [Undefined {varName = "a"},Undefined {varName = "c"}])
--
-- >>> testFn table2@
-- Right (C "valA" "value" "valC")
--
-- >>> testFnWParse table2
-- Left (fromList [ParseError {varName = "valA", cause = "not an integer"}])
--
-- >>> testFnWParse table3
-- Right (D "value" 35 "valC")
--
-- >>> testFnWParse table4
-- Left (fromList [Undefined {varName = "b"},Undefined {varName = "c"},ParseError {varName = "3i5", cause = "not an integer"}])
module Control.Monad.Trans.Lookup
( LookupT (..)
, runLookup
, Lookup
, lookup
, ParseError (..)
-- * Common Lookup Functions
, lookupFind
, lookupFindDesperate
, lookupEnv
, lookupFile
, lookupFileBS
, lookupFileBSL
, lookupFileBSC
, lookupFileBSLC
, lookupFileT
, lookupFileTL
-- * Common Lookup Functions With Defaults
, lookupEnvWithDef
, lookupEnvWithReadDef
, lookupFileWithDef
, lookupFileBSWithDef
, lookupFileBSLWithDef
, lookupFileBSCWithDef
, lookupFileBSLCWithDef
, lookupFileTWithDef
, lookupFileTLWithDef
) where

-- base
import Control.Applicative
import Data.Bool (bool)
import Data.Foldable (find)
import Data.Functor.Identity
import Data.Monoid (Alt(..))
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

-- NicLib
import NicLib.NStdLib ((<%), morphism239, readMaybe, findM)
import NicLib.Errors (liftME)

-- transformers
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe

newtype LookupT m a = LookupT { runLookupT :: m (Either (S.Set ParseError) a) } deriving Functor
type Lookup a = LookupT Identity a

runLookup :: Lookup a -> Either (S.Set ParseError) a
runLookup = runIdentity . runLookupT

-- | For whether an environment variable is not defined, or fails to parse into a desired Config field
data ParseError
    -- | denotes that a variable could not be found
    = Undefined { varName :: String }
    -- | denotes that a variable could not be parsed from a string
    | ParseError
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
-- Remember that you can provide default values by using @\<|\> defaultValue@ in the lookup function (the 3rd parameter)
lookup :: Monad m
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
    l >>= f = LookupT $ morphism239 runLookupT g runLookupT l where
        g = \case
            Left  s -> LookupT $ (Left . either (s<>) (const s)) <$> runLookupT (f undefined)
            Right a -> f a

instance MonadIO m => MonadIO (LookupT m) where
    liftIO = LookupT . liftIO . fmap pure

instance MonadTrans LookupT where
    lift = LookupT . fmap pure

-- | 'lookup' based around 'findM'
--
-- Find the first item in a Foldable whose 2nd letter is 'o'; then try to parse its 3rd character to an int:
--
-- @
-- let var = "somevar"
--     lkupFn = runLookup . lookupFind
--         var
--         (pure . liftME (ParseError var "Couldn't parse to int") . readMaybe @Int . pure . (!!2))
--         (pure . (=='o') . (!!1))
--         . words
-- @
--
-- >>> lkupFn "Here are many wo4ds"
-- Right 4
--
-- >>> lkupFn "Here are some wo4ds"
-- Left (fromList [ParseError {varName = "somevar", cause = "Couldn't parse to int"}])
--
-- >>> lkupFn "Here are many things"
-- Left (fromList [Undefined {varName = "somevar"}])
--
-- Note that inserting "some" causes failure, despite "wo4ds" being a valid candidate. To continue searching until parse matches, use 'lookupFindDesperate'.
lookupFind :: (Monad m, Foldable t)
            => String -- ^ variable name to be displayed in error messages
            -> (i -> m (Either ParseError b)) -- ^ parsing function
            -> (i -> m Bool) -- ^ predicate to pass to find
            -> t i -- ^ list to search through
            -> LookupT m b
lookupFind str parse predicate xs = lookup (const str) parse (\i s -> findM predicate s) xs undefined

-- God only knows why, but using NicLib.foldMapM kept getting the following error in GHCi:
-- "No instance for (Control.Monad.Trans.Error.Error ParseError) arising from a use of ‘foldMapM'"
-- So it's MaybeT to the rescue!?
-- | Like 'lookupFind', but searches through failed-parsing-elements, returning on the first element matching both predicate /and/ successfully parsing, if any such element exists.
--
-- Thus if @lookupFindDesperate@ returns a @ParseError@, it must be of the @Undefined@ constructor. Be careful when using this in your logic, as saying that "no satisfactory & parsable object could be found" means the same as "undefined" can be misleading.
--
-- Continuing where the example from 'lookupFind' left-off, if, in the definition of lkupFn, we replace lookupFind with lookupFindDesperate,
--
-- >>> lkupFn "Here are some wo4ds"
-- Right 4
lookupFindDesperate :: (Monad m, Foldable t)
                    => String -- ^ variable name to be displayed in error messages
                    -> (i -> m (Either ParseError b)) -- ^ parsing function
                    -> (i -> m Bool) -- ^ predicate to pass to 'findM'
                    -> t i -- ^ list to search through
                    -> LookupT m b
lookupFindDesperate str parse predicate = LookupT . fmap (maybe (Left . S.singleton $ Undefined str) Right) . runMaybeT . getAlt
    . foldMap (\i -> Alt $ lift (predicate i) >>= MaybeT . bool (pure Nothing) (either (const Nothing) Just <$> parse i))

lookupEnv :: Monad m => [(String, b)] -> String -> LookupT m b
lookupEnv = lookup id (pure . pure) (pure <% P.lookup)

lookupFileCommon :: (String -> IO b) -> String -> LookupT IO b
lookupFileCommon f = lookup id (pure . pure) (\x _ -> doesFileExist x >>= bool (pure Nothing) (pure <$> f x)) undefined

lookupFile :: FilePath -> LookupT IO String
lookupFile = lookupFileCommon readFile

lookupFileBS :: FilePath -> LookupT IO BS'.ByteString
lookupFileBS = lookupFileCommon BS'.readFile

lookupFileBSL :: FilePath -> LookupT IO BS.ByteString
lookupFileBSL = lookupFileCommon BS.readFile

lookupFileBSC :: FilePath -> LookupT IO BSC'.ByteString
lookupFileBSC = lookupFileCommon BSC'.readFile

lookupFileBSLC :: FilePath -> LookupT IO BSC.ByteString
lookupFileBSLC = lookupFileCommon BSC.readFile

lookupFileT :: FilePath -> LookupT IO T'.Text
lookupFileT = lookupFileCommon TIO.readFile

lookupFileTL :: FilePath -> LookupT IO T.Text
lookupFileTL = lookupFileCommon TLIO.readFile

-- there's probably some confusing way to express this in terms of @lookup@, involving bind or join
-- | e.g.
--
-- @
-- do
--     env <- getEnvironment
--     let shellLevel = runLookup $ lookupEnvWithDef (\x -> pure . liftME (ParseError x "Couldn't parse into integer") $ readMaybe @Int x) 30 env \"SHLVL\"
--     pure shellLevel
-- @
--
-- The type of this expression is @IO (Either (S.Set ParseError) Int)@. If you made a typo and put \"SHELVL\", then it'd return @Right 30@ in IO.
--
-- Remember to include @pure@ in your parsing function if you're using the @Identity@ category!
lookupEnvWithDef :: Monad m
                 => (b -> m (Either ParseError c))
                 -> c
                 -> [(String, b)]
                 -> String
                 -> LookupT m c
-- maybe (Just def)
lookupEnvWithDef p def s i = LookupT $ (pure <% P.lookup) i s >>= \case
    Nothing -> pure (Right def)
    Just a -> BiF.first S.singleton <$> p a

-- TODO: abstract-away read and env, for easy combination of read (env, noread), (env, read), (notenv, read), (notenv, notread)
-- | @lookupEnvWithDef@, but parses to some @Read@
lookupEnvWithReadDef :: (Read i, Monad m) => i -> [(String, String)] -> String -> LookupT m i
lookupEnvWithReadDef = lookupEnvWithDef (\s -> pure . liftME (ParseError s "Can't parse into an integer.") $ readMaybe s)

lookupFileCommonWithDef :: (String -> IO b) -> b -> String -> LookupT IO b
lookupFileCommonWithDef f def = lookup id (pure . Right) (\x _ -> doesFileExist x >>= bool (pure $ Just def) (pure <$> f x)) undefined

lookupFileWithDef :: String -> FilePath -> LookupT IO String
lookupFileWithDef = lookupFileCommonWithDef readFile

lookupFileBSWithDef :: BS'.ByteString -> FilePath -> LookupT IO BS'.ByteString
lookupFileBSWithDef = lookupFileCommonWithDef BS'.readFile

lookupFileBSLWithDef :: BS.ByteString -> FilePath -> LookupT IO BS.ByteString
lookupFileBSLWithDef = lookupFileCommonWithDef BS.readFile

lookupFileBSCWithDef :: BSC'.ByteString -> FilePath -> LookupT IO BSC'.ByteString
lookupFileBSCWithDef = lookupFileCommonWithDef BSC'.readFile

lookupFileBSLCWithDef :: BSC.ByteString -> FilePath -> LookupT IO BSC.ByteString
lookupFileBSLCWithDef = lookupFileCommonWithDef BSC.readFile

lookupFileTWithDef :: T'.Text -> FilePath -> LookupT IO T'.Text
lookupFileTWithDef = lookupFileCommonWithDef TIO.readFile

lookupFileTLWithDef :: T.Text -> FilePath -> LookupT IO T.Text
lookupFileTLWithDef = lookupFileCommonWithDef TLIO.readFile
