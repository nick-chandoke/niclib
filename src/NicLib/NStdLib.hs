{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE
  CPP
, DeriveFunctor
, FlexibleContexts
, FlexibleInstances
, LambdaCase
, MultiParamTypeClasses
, MultiWayIf
, NamedFieldPuns
, OverloadedStrings
, StandaloneDeriving
, TupleSections
, UndecidableInstances
, ViewPatterns
#-}

-- | Miscellaneous data structures and functions that make everyday coding easier. My own Prelude, plus my own functionality.
module NicLib.NStdLib
( module Data.Semigroup
, module Control.Arrow
, module Control.Monad
, module Control.Applicative
, module Data.Traversable
, module Data.Maybe
, module Data.Bool
, module Data.Functor.Identity
, module Control.Monad.IO.Class
, mtell
, (&)
, on
, (>*>)
, OrderBy(..)
, uncons
, bimap
, both
, (<&&>)
, (<||>)
, (?)
, (!!?)
, (↔)
, sxor
, pxor
, xor
, ffilter
, (<&>)
, readMaybe
, replaceAll
, bool'
, Isomorphism(..)
, cT
, foldBins
, foldBins'
, (<%)
, (%>)
, replicateM'
, foldMapM
, partitionM
, findM
, showBin
) where

import Numeric (showIntAtBase)
import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer
import Control.Monad.IO.Class
import Data.Bifunctor as BiF
import Data.Bool
import Data.Char
import Data.Either (either)
import Data.Function ((&), on, fix) -- importing everything messes with the ArrowCurry definition
import Data.Functor.Identity
import Data.List
import Data.Maybe
import Data.Monoid (Alt(..))
import Data.Semigroup
import Data.Set (Set)
import Data.String
import Data.Traversable
import Data.Tree
import Data.Word
import Prelude hiding (GT, LT, EQ, (.), id, curry, uncurry)
-- import Text.Taggy (Node, nodeChildren) -- for its Isomorphism instance
import qualified Data.ByteString.Char8 as BS'
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ListLike as LL
import qualified Data.Set as S
import qualified Data.Text as T'
import qualified Data.Text.Lazy as T
import qualified Prelude

-- | OrderBy inherits Eq & Ord instances from left tuple argument, ignoring right tuple argument
newtype OrderBy a b = OrderBy {unOrderBy :: (a,b)}
instance (Show a, Show b) => Show (OrderBy a b) where show = show . unOrderBy
instance Eq a => Eq (OrderBy a b) where (unOrderBy -> (a,_)) == (unOrderBy -> (c,_)) = a == c
instance Ord a => Ord (OrderBy a b) where compare (unOrderBy -> (a,_)) (unOrderBy -> (c,_)) = compare a c

infixl 3 <&&>
-- | Convenient particularly in (->), for composition of unary functions, e.g. filter (odd <&&> (`divides` 5))
(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)

infixl 3 <||>
-- | Convenient particularly in (->), for composition of unary functions, e.g. filter (isDigit <||> (=='.'))
(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)

-- | ternary if-then-else operators
-- so Java/C[++]'s bool ? x : y becomes bool ? x ↔ y in Haskell
-- (I assume most Haskellers would ask why create such operators. Well, I just love the ternary operator, and after switching to Haskell, I felt something lacking...so don't judge please.)
infixr 1 ?
(?) :: Bool -> (a, a) -> a
p ? x = if p then fst x else snd x

infixr 2 ↔ -- ^K <> in vim
(↔) :: a -> a -> (a, a)
(↔) = (,)

-- deriving instance Functor ((,,) a b)

both :: Bifunctor p => (a -> d) -> p a a -> p d d
both f = bimap f f

xor :: Bool -> Bool -> Bool
xor x y = (x || y) && (not (x && y))

-- | xor for sets
sxor :: Ord a => Set a -> Set a -> Set a
sxor r s = S.union (r S.\\ s) (s S.\\ r)

-- | Partitioned xor, or disjoint union, or coproduct of sets; r `pxor` s = partition (`elem` r) (r `sxor` s). Formally, r `pxor` s ==> (r', s') : r' < r, s' < s, r' `sxor` s' = r `sxor` s. This is an instance of coproduct over the category of sets.
pxor :: Ord a => Set a -> Set a -> (Set a, Set a)
pxor r s = (r S.\\ s, s S.\\ r)

-- | flip fmap; <&>:<$>::&:$
infixl 4 <&>
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

-- | filter but with codomain generalized to ListLike's
ffilter :: (LL.ListLike list item, Foldable t) => (item -> Bool) -> t item -> list
ffilter p = foldr (\a b -> if p a then LL.cons a b else b) LL.empty

-- | f &&& g >*> h is shorthand for (f &&& g) >>> uncurry h
infixr 2 >*>
(>*>) :: ArrowCurry cat => cat a1 (a2, b) -> cat a2 (cat b c) -> cat a1 c
x >*> h = x >>> uncurry h

-- | replace all occurences of old with new. Using Traversable because we're replacing elements "in-place"; we're preserving structure and type.
-- cf. Foldable, which does not necessarily preserve structure.
replaceAll :: (Eq b, Traversable t) => b -> b -> t b -> t b
replaceAll old new = fmapDefault (bool' id (const new) (==old))

readMaybe :: Read a => String -> Maybe a
readMaybe str = case reads str of
    [(x,"")] -> Just x
    _ -> Nothing

-- | like bool in Data.Bool, but allows pointfree manipulation like Data.Maybe.maybe (for (->) Applicative, its most common use; or, sometimes one may use it in other Applicatives). Example: @bool' ((-) 10) (+1) (>3)@ equals (in the (->) monoidal functor) @\n -> if n < 3 then n + 1 else n - 10@
bool' :: Applicative f => f d -> f d -> f Bool -> f d
bool' = liftA3 bool

-- | "if index in bounds then pure (item at index) else empty," but most efficient
(!!?) :: (Num n, Eq n, Ord n, LL.ListLike list item, Alternative f) => list -> n -> f item
xs !!? n
    | n < 0 = empty -- still works if negative, but faster to quit immediately if negative, rather than traverse a whole list just to return Nothing
    | otherwise = fix (\f i -> LL.uncons >>> \case Nothing -> empty; Just (h,t) -> if i == n then pure h else f (i + 1) t) 0 xs

{- | zip any two foldables (a quick-to-write but slow to perform equivalent is zipFolds f = zipWith f (toList x) (toList y)
zipFolds :: (Foldable t1, Foldable t2) => (a -> b -> c) -> t1 a -> t2 b -> [c]
zipFolds f x y = undefined -}

-- | invertible information-preserving morphism between two types; must satisfy dual . iso = id = iso . dual
-- dual is defined in this class mostly to force you to check that your definition of iso is indeed structure-preserving!
-- TODO: generalize to Homomorphism type family, so that we can do things like, e.g. Text -> Query -> ByteString (this particular example cannot be Isomorphism Query (ListLike a), because a is different in iso vs. in dual! (Right?))
-- ALSO: create structure that allows automatic calculation (again, prob. impl. via higher kinds) quickest conversion between types (i.e. shortest path in graph where nodes are types and directed edges are homomorphisms)
-- Notice how, for (Integral a, Num a, Integral b, Num b) => we can convert between members of the same typeclass. Perhaps, rather than Isomorphism a b, I should do Homomorphism a b | a -> b where (~=>) :: a -> b....
class Isomorphism a b where
    {-# MINIMAL iso, dual #-}
    -- | an isomorphism; typically there's only one isomorphism between any two structures
    iso :: a -> b
    -- | categorical dual of an isomorphism
    dual :: b -> a

-- instance {-# OVERLAPPABLE #-} Isomorphism a String => Show a where show = iso
-- instance {-# OVERLAPPABLE #-} Isomorphism a String => IsString a where fromString = dual

instance Isomorphism T'.Text BS'.ByteString where
    iso = BS'.pack . T'.unpack
    dual = T'.pack . BS'.unpack

instance Isomorphism T.Text String where
    iso = T.unpack
    dual = T.pack

instance Isomorphism BS.ByteString String where
    iso = BS.unpack
    dual = BS.pack

instance Isomorphism T'.Text String where
    iso = T'.unpack
    dual = T'.pack

instance Isomorphism BS'.ByteString String where
    iso = BS'.unpack
    dual = BS'.pack

{- taggy isomorphism; to be replaced by html-conduit
instance Isomorphism Node (Tree Node) where
    iso = unfoldTree (id &&& nodeChildren)
    dual = rootLabel
-}

instance Isomorphism (ExceptT e m a) (m (Either e a)) where
    iso = runExceptT
    dual = ExceptT

instance ArrowCurry arr => Isomorphism (arr a (arr b c)) (arr (a, b) c) where
    iso = uncurry
    dual = curry

instance Isomorphism Char Word8 where
    iso = fromIntegral . ord
    dual = chr . fromIntegral

-- With Backpack <https://ghc.haskell.org/trac/ghc/wiki/Backpack> coming into use,...I'm a bit unsure of how to use polymorphic strings and lists....
-- Fortunately I don't yet have a use for this isomorphism, so I can defer worries to the future.
-- Rather than Monoid, I could have used Default, Alternative, MonadPlus,...I mean jeez how many null-supporting ADTs are there? I have to choose the single most ubiquitous one, or declare a buzzillion instances
-- #whydoesnthaskellusestructuraltyping
-- I suppose it's nice that this operation preserves fold
-- TODO: this should make able to transform a list of URLs into a tree of them?
instance Monoid m => Isomorphism [m] (Tree m) where
    iso = Node mempty . fmap (flip Node [])
    dual = fmap rootLabel . subForest

-- THIS INSTANCE PENDING DEPRECATION IN FAVOR OF ExceptT's Alternative instance:
--     (Monad m, Monoid e) => Alternative (ExceptT e m); (<|>) concats the monoid while failing. Thus use asum instead of mconcat, and (<|>) rather than (<>).
-- | created in conjunction with iso :: ExceptT e m a -> WriterT e m a: in an ExceptT, an error is fatal to that operation. However, if one runs many ExecptT's, some may succeed and others fail; thus, for this operation, the ExceptT's errors are no longer fatal!
-- So the idea here is converting a group of ExecptT's into a group of WriterT's, collecting results and errors.
-- Remember that if a is not a monoid, but you still want to return a and collect states/errors, just wrap a in Const (in Data.Monoid)
instance (Monoid s, Monoid a, Monad m) => Semigroup (WriterT s m a) where
    (<>) = cT WriterT (on (liftM2 (<>)) runWriterT)

instance (Monoid s, Monoid a, Monad m) => Monoid (WriterT s m a) where
    mempty = return mempty
    mappend = (<>)

-- | Control.Monad.Trans.Writer.tell, but monoid-compatible (rather than returning (), returns mempty; thus, dosen't affect value, but in different way)
mtell :: (Monoid s, Monoid a, Monad m) => s -> WriterT s m a
mtell = return mempty <=< tell

-- Given two isomorphic f, g :: MonadTrans (i.e. their ADT representations have the same number of variables,) ∃ isomorphism iso : f → g:
-- iso = lift . isoF . runMonadTrans where runMonadTrans represents e.g. runStateT, runWriterT and isoF is an isomorphism that maps a set of ADT variables to another (isoF is a total function)
-- I distinguish between isoF and iso because they aren't the same function in Haskell, though they're basically equal because lift and runMonadTrans are just automorphisms
-- we need Eq s to check if it equals mempty in the dual
instance (Monoid s, Monoid a, Monad m, Eq s) => Isomorphism (ExceptT s m a) (WriterT s m a) where
    iso = either mtell return <=< lift . runExceptT
    dual = ExceptT . fmap (\(a, e) -> if e == mempty then Left e else Right a) . runWriterT

class Arrow arr => ArrowCurry arr where
    curry :: (arr (a, b) c) -> arr a (arr b c)
    uncurry :: arr a (arr b c) -> (arr (a, b) c)

instance ArrowCurry (->) where
    curry = Prelude.curry
    uncurry = Prelude.uncurry

-- | "Curry transform." Compose a binary function with a unary function. Handy for point-free definition of binary functions!
-- Data.Function.on can be defined in terms of a unary function, a binary function, curry, and uncurry; this definition is similar to that of cT.
-- If you have nothing better to do, playing code golf with on, cT, curry, uncurry, and arrows is pretty freaky and cool, especially when you assign binary operators to each of them and treat the duals curry and uncurry as adjoint functors
-- Via currying, one can use cT to compose any n-ary and n+1-ary functions
-- Examples: notMember = cT not Set.member :: a -> Set a -> Bool
--           mappend = cT WriterT (on (liftM2 mappend) runWriterT) -- used in NStdLib's Monoid instance for WriterT s m a
--           also one can use the <% operator (looks like two circles being shrunk into one going to the left):
--           notMember = not <% Set.member
cT :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
cT u b = curry (u . uncurry b)

infixl 2 <%
(<%) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(<%) = cT

infixr 2 %>
(%>) :: (a -> b -> c) -> (c -> d) -> (a -> b -> d)
(%>) = flip cT

-- | Fold a bunch of binary functions. As an example, here's an Ord instance for @Network.Wai.Request@:
-- @(==) = foldBins (&&) (==) True [requestMethod, rawPathInfo, rawQueryString] :: Request -> Request -> Bool@
foldBins :: Foldable t => (c -> b1 -> c) -> (b2 -> b2 -> b1) -> c -> t (a -> b2) -> a -> a -> c
foldBins f1 f2 x = curry . foldr (\f t -> liftA2 f1 t . uncurry . on f2 $ f) (pure x)

-- | Strict left fold version of @foldBins@. Note that, despite being a left fold, it has a function signature identical to foldBins.
foldBins' :: Foldable t => (c -> b1 -> c) -> (b2 -> b2 -> b1) -> c -> t (a -> b2) -> a -> a -> c
foldBins' f1 f2 x = curry . foldl' (\t -> liftA2 f1 t . uncurry . on f2) (pure x)

-- | Numeric has showHex and showOct, but not showBin? What.
showBin :: (Show a, Integral a) => a -> ShowS
showBin = showIntAtBase 2 intToDigit

{- | writes state to file, database, &c (MonadCatch m => s -> m ()), every n actions, state updates, or upon predicate satisfaction (predicate is checked after every monadic action)
-- should I have a new data type for this, though? It's so simple: it's just a StateT with (>>=) = (>>=) >=> (bool' (return ()) writeState p =<< get)
newtype SaveStateT = SaveStateT {runWST :: StateT s m a}
instance Monad SaveStateT where
    m >>= f = 
instance MonadTrans SaveStateT 
-}

-- | replicateM generalized to monoids
-- modified from Control.Monad
replicateM' :: (Applicative f, Monoid m) => Int -> f m -> f m
replicateM' cnt0 f = g cnt0
    where
        g cnt
            | cnt <= 0  = pure mempty
            | otherwise = liftA2 mappend f (g (cnt - 1))

foldMapM :: (Foldable t, Monad m, Monoid b) => (a -> m b) -> t a -> m b
foldMapM f = foldM (\acc -> fmap (mappend acc) . f) mempty

partitionM :: (Foldable t, Monad m) => (a -> m Bool) -> t a -> m ([a], [a])
partitionM p = foldMapM (\a -> bool (mempty, pure a) (pure a, mempty) <$> p a)

findM :: (Foldable t, Monad m) => (a -> m Bool) -> t a -> m (Maybe a)
findM p = liftM getAlt . foldMapM (\x -> Alt . bool Nothing (Just x) <$> p x)

#ifdef EXPERIMENTAL
        Catamorphism: a unique homomorphism from an initial F-algebra with given endofunctor into some other algebra. Here in Haskell ADTs are re-expression of initial algebras.
-- | Allows composing functions in a catamorphism, e.g.
-- Catamorphism's are right-associative; use flipCata to convert to left-associative
-- f is the composition function (mappend for monoids); otherwise specifiable 
newtype Catamorphism f a b = Cata {runCata :: (\a b -> b)} -- TODO: rewrite in terms of f
map = Catamorphism (:) (f) -- rather than (:), parameterize insertion/composition function (or require semigroup with initial element, or foldMap-like function for monoids
-- of course, these catamorphisms are made to work for folds; foldr (cata₂ . cata₁)
-- implementation of the category of catamorphisms may obsolete fold*n functions declared earlier in this NStdLib module
instance Category (Catamorphism compose) where
    id = Catamorphism (\a _ -> compose a)
    f . g = cT...? -- :: (b -> c -> c) -> (a -> b -> b) ->

Example: tree fold: for a fixed type a, consider functor mapping type b to a type that contains a copy of each term of a, as well as all pairs of b's (terms of the product type of two instances of the type b). An algebra consists of a function to b, which either acts on an a term of two b terms, i.e. a -> b and b -> b -> b, i.e. suppose a terminus t reachable by either an f : a -> t, or g : b -> b -> t, t = b.
type TreeAlgebra a b = (a -> b, b -> b -> b) -- i.e. type TreeAlrebraFunctors a b = (a -> b) | (b -> b -> b) -- but that's not legal Haskell; see below:
data Tree a {- terminus t -} = Leaf a {- a functor f : a -> t -} | Branch (Tree a) (Tree a) {- g : b -> b -> t -} -- where, clearly we can see, t = b

#endif
