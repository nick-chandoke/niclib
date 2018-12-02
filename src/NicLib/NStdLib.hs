-- | Generally useful functions
module NicLib.NStdLib
( (!!?)
, (%>)
, (<%)
, (<&&>)
, (<&>)
, (<||>)
, (>*>)
, (?)
, (↔)
, As(..)
, OrderBy(..)
, bool'
, both
, cT
, findM
, foldBins
, foldBins'
, foldMapM
, morphism239
, morphism240
, morphism46
, partitionM
, readMaybe
, replicateM'
, showBin
, sxor
, whenM
, xor
) where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Control.Monad.Trans.Accum (AccumT, add, runAccumT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Data.Bifunctor as BiF
import Data.Bool
import Data.Char
import Data.Function (on, fix) -- importing everything messes with the ArrowCurry definition
import Data.Foldable
import Data.Maybe
import Data.Monoid (Alt(..))
import Data.Set (Set)
import Data.String
import Data.Tree
import Numeric (showIntAtBase)
import Prelude hiding (GT, LT, EQ, (.), id, curry, uncurry)
import qualified Data.ListLike as LL
import qualified Data.Set as S
import qualified Prelude

-- | OrderBy inherits Eq & Ord instances from left tuple argument, ignoring right tuple argument
newtype OrderBy a b = OrderBy {unOrderBy :: (a,b)}
instance (Show a, Show b) => Show (OrderBy a b) where show = show . unOrderBy
instance Eq a => Eq (OrderBy a b) where (unOrderBy -> (a,_)) == (unOrderBy -> (c,_)) = a == c
instance Ord a => Ord (OrderBy a b) where compare (unOrderBy -> (a,_)) (unOrderBy -> (c,_)) = compare a c

infixl 3 <&&>
-- | Convenient particularly in @(->)@, for composition of unary functions, e.g. @filter (odd <&&> (`divides` 5))@
(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)

infixl 3 <||>
-- | Convenient particularly in @(->)@, for composition of unary functions, e.g. @filter (isDigit <||> (=='.'))@
(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)

-- | Ternary if-then-else operators
--
-- So Java/C[++]'s @bool ? x : y@ becomes @bool ? x ↔ y@ in Haskell
--
-- (I assume most Haskellers would ask why create such operators. Well, I just love the ternary operator, and after switching to Haskell, I felt something lacking...so don't judge please.)
infixr 1 ?
(?) :: Bool -> (a, a) -> a
p ? x = if p then fst x else snd x

infixr 2 ↔
(↔) :: a -> a -> (a, a)
(↔) = (,)

-- | So useful when doing, e.g. @when (directory is empty) ⋯@!
--
-- btw, whenM cannot exist for Applicatives. Also @whenM@ cannot be expressed in terms of @bool'@.
whenM :: Monad m => m Bool -> m () -> m ()
whenM c v = flip when v =<< c

-- | Apply an operation to both objects of a Bifunctor
both :: Bifunctor p => (a -> d) -> p a a -> p d d
both f = bimap f f

-- | Exclusive-or for Booleans
xor :: Bool -> Bool -> Bool
xor x y = (x || y) && (not (x && y))

-- TODO: probably can be done better by using NicLib.Set.diff
-- | Exclusive-or for sets
sxor :: Ord a => Set a -> Set a -> Set a
sxor r s = S.union (r S.\\ s) (s S.\\ r)

-- | flip fmap; <&>:<$>::&:$
infixl 4 <&>
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

-- | @f &&& g >*> h@ is shorthand for @(f &&& g) >>> 'uncurry' h@
infixr 2 >*>
(>*>) :: ArrowCurry cat => cat a1 (a2, b) -> cat a2 (cat b c) -> cat a1 c
x >*> h = x >>> uncurry h

readMaybe :: Read a => String -> Maybe a
readMaybe str = case reads str of
    [(x,[])] -> Just x
    _ -> Nothing

-- | Like 'Data.Bool.bool', but allows pointfree manipulation like 'Data.Maybe.maybe' (for (->) Applicative, its most common use; or, sometimes one may use it in other Applicatives).
--
-- @bool' ((-) 10) (+1) (\>3)@ equals (in the (->) monoidal functor) @\\n -> if n < 3 then n + 1 else n - 10@@
bool' :: Applicative f => f d -> f d -> f Bool -> f d
bool' = liftA3 bool

-- | An efficient version of "if index in bounds then pure (item at index) else empty."
(!!?) :: (Num n, Ord n, LL.ListLike list item, Alternative f) => list -> n -> f item
xs !!? n
    | n < 0 = empty -- still works if negative, but faster to quit immediately if negative, rather than traverse a whole list just to return Nothing
    | otherwise = fix (\f i -> LL.uncons >>> \case Nothing -> empty; Just (h,t) -> if i == n then pure h else f (i + 1) t) 0 xs

-- | @morphism239@ performs a composition of functions on monad transformers that share a common inner monad and constructor but have different parameters, e.g. @State Text m@ and @State String m@, or @ExceptT e m@ and @AccumT w m@. It's almost a generalization of mapAccumT, mapExceptT, etc., or like an inverse of @hoist@. It doesn't allow the inner monad to change, but does allow the outer monad to change.
--
-- Here's an example from NicLib.Errors:
--
-- @
-- toError :: (Monad m, Monoid e, Eq e) =\> AccumT e m a -\> ExceptT e m a
-- toError = ExceptT . morphism239 (flip runAccumT mempty) (\\(a, w) -\> if w == mempty then pure a else err w) runExceptT
-- @
--
-- It seems pretty goofy to have @ExceptT@ and @runExceptT@ in the above example, but we need them both in order to typecheck @>>=@. This is conceptually like lifting the inner monad, then fmapping over it.
--
-- By the way, you'll typically want to run some form of @lift . morphism239 ⋯@
--
-- Note that f and f' must be the same level of monad transformer! For instance, the following does not work (see @NicLib.Errors.BugT@):
--
-- @morphism239 (flip runAccumT mempty) undefined runBugT@.
--
-- GHC will give an infinite type error because @runBugT@ acts on a 2-transformer stack (a @BugT@) but runAccumT runs on a 1-transformer stack (namely @AccumT@). What one could like to concieve is:
--
-- @
-- t m a ~ AccumT e m a
-- s m x ~ BugT e w m x
-- @
--
-- suggesting that they share the common monad @m@. However, this is incorrect; in the expression @s m x,@
--
-- * @s ~ ExceptT e@,
-- * @m ~ AccumT w m2@, and
-- * @x ~ x@.
-- 
-- The "nice" couple of types above are suggested by @lift :: m a -> t m a@. However, this does not imply @lift . lift :: m a -> t1 t2 m a@! The reality is that @lift . lift :: m a -> t1 (t2 m) a@! This is a bit of a double-edged sword. I'm sure if you've spent much time with transformers, you're familiar with the clash of Haskell's type system vs. monads associative composition in category theory. Yet I have not found mtl style or freer-monads quite nice enough to use. Feel free to convince me otherwise.
--
-- Use 'morphism240' to lift this restriction, so that you can mix degrees of monad transformers. (morphism240 is just morphism239 but with a less specific type signature.) I chose to have both 239 and 240 because 239's signature is easy to understand, whereas 240's is so general that it's difficult to determine its use cases, and it's good to be aware/careful about manipulating monad transformers.
--
-- PS. I'm sure this could be better implemented using comonad and mtl (or Eff or <https://www.tweag.io/posts/2018-10-04-capability.html capabilities> or <https://ghc.haskell.org/trac/ghc/wiki/Backpack backback>,) but I'm not interested in (orphan) instancing common monad transformers as comonads or trying to learn up-and-coming-new frameworks, so meh. (morphism239 could be written in terms of @lower@ (http://hackage.haskell.org/package/comonad-5.0.4/docs/Control-Comonad-Trans-Class.html).)
--
-- Oh, wait. Actually this is just like @embed@, but doesn't require one to instance MMonad. <http://hackage.haskell.org/package/mmorph-1.1.2/docs/Control-Monad-Morph.html>
morphism239 :: (Monad m)
            => (t m a -> m b) -- ^ unwrap the first transformer. Usually run*, e.g. @runStateT@, @runBugT@
            -> (b -> s m x) -- ^ a morphism that relates the prior two unwrapping functions
            -> (s m x -> m y) -- ^ unwrap the second transformer. Usually the prior run* function
            -> t m a
            -> m y
morphism239 = morphism240

-- | So 'morphism239' didn't cut it for ya? As you can see, 240 strings-together two Kleisli morphisms via a static morphism.
-- You may be wondering why have both @f@ and @f'@ (look at the source code); when I created 240, they *were the same function!* (see the code for @NicLib.Errors.dumpWarn@). They remain here for the time being; I'm considering making morphism240 :: m b -> (b -> c) -> (c -> m d) -> m d. I'm unsure how that'll affect clarity of code - especially that that uses 239.
morphism240 :: Monad m => (a -> m b) -> (b -> c) -> (c -> m d) -> a -> m d
morphism240 f t f' x = f x >>= f' . t

-- | Originally written for a @Monoid@ instance for @WriterT@
morphism46 :: Applicative f => (f c -> d) -> (a -> f b) -> (b -> b -> c) -> a -> a -> d
morphism46 wrap unwrap g = wrap <% on (liftA2 g) unwrap

-- TODO: can use Ed Kmett's semigroupoid library's Iso type <http://hackage.haskell.org/package/semigroupoids-5.3.1/docs/Data-Isomorphism.html> for this?
-- | Homomorphism/isomorphism type family. Use with @-XTypeApplications@, e.g.
--
-- >>> as @Char 65
-- 'A'
--
-- For any two instances A and B
--
-- @
-- instance As B where type To B = A
-- instance As A where type To A = B
-- @
--
-- should satisfy the identity
--
-- prop> (as \@B) . (as \@A) = (as \@A) . (as \@B) = id
--
-- Which properties the homomorphism will preserve will depend on the objects you're working with, so the instances here are conservatively few.
--
-- Note that this class is limited because GHC allows no overlap of type family instances. For example, I can't instance both a to [a] and [a] to a, since a is a type variable and can match a whole lot of instances (remember that GHC does not consider the context; it just matches the RHS of (=>) in a signature.)
--
-- I'm not sure how useful this structure is, for either general or specific purposes.
--
-- Perhaps using @Category@ is useful (instead)? Then homomorphisms would be composed applicatively. I have yet to dabble with Backpack <https://ghc.haskell.org/trac/ghc/wiki/Backpack> (I'm assuming by the isomorphism classes about string-like types that backpack has applications in isomorphisms or embeddings.)
--
class As b where
    type To b
    as :: To b -> b

instance (Monad m, Eq e, Monoid e) => As (ExceptT e m a) where
    type To (ExceptT e m a) = AccumT e m a
    as = ExceptT . morphism239 (flip runAccumT mempty) (\(a, e) -> if e == mempty then pure a else (ExceptT . pure . Left) e) runExceptT

instance (Monoid e, Monad m) => As (AccumT e m (Maybe a)) where
    type To (AccumT e m (Maybe a)) = ExceptT e m a
    as = (\case Left e -> add e >> return Nothing; Right x -> return $ Just x) <=< lift . runExceptT

-- Rather than Monoid, I could have used Default, Alternative, MonadPlus,...I mean jeez how many null-supporting ADTs are there? I have to choose the single most ubiquitous one, or declare a buzzillion instances
-- #whydoesnthaskellusestructuraltyping
-- I suppose it's nice that this operation preserves fold
-- TODO: this should make able to transform a list of URLs into a tree of them?
instance Monoid m => As (Tree m) where
    type To (Tree m) = [m]
    as = Node mempty . fmap (flip Node [])

instance As [m] where
    type To [m] = (Tree m)
    as = fmap rootLabel . subForest

-- | I haven't actually used this class for anything yet, but at some time I considered vaguely yet enthusiastically that one can do some impressive programming using arrows, currying, and categories. It may even be the case that @ArrowCurry@ doesn't add any functionality beyond Control.Arrow.
class Arrow arr => ArrowCurry arr where
    curry :: (arr (a, b) c) -> arr a (arr b c)
    uncurry :: arr a (arr b c) -> (arr (a, b) c)

instance ArrowCurry (->) where
    curry = Prelude.curry
    uncurry = Prelude.uncurry

-- | "Curry transform." Compose a binary function with a unary function. Handy for point-free definition of binary functions!
--
-- 'Data.Function.on' can be defined in terms of a unary function, a binary function, curry, and uncurry; this definition is similar to that of cT.
--
-- If you have nothing better to do, playing code golf with on, cT, curry, uncurry, and arrows is pretty freaky and cool, especially when you assign binary operators to each of them and treat the duals curry and uncurry as adjoint functors
--
-- Via currying, one can use cT to compose any n-ary and n+1-ary functions
--
-- Examples:
--
-- @
-- notMember = cT not Set.member :: a -> Set a -> Bool
-- mappend = cT WriterT (on (liftM2 mappend) runWriterT) -- a Monoid instance for WriterT
-- @
--
-- also one can use the '<%' operator (looks like two circles being shrunk into one going to the left):
--
-- @notMember = not <% Set.member@
cT :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
cT u b = curry (u . uncurry b)

infixl 2 <%
(<%) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(<%) = cT

infixr 2 %>
(%>) :: (a -> b -> c) -> (c -> d) -> (a -> b -> d)
(%>) = flip cT

-- | Fold a bunch of binary functions. As an example, here's an Ord instance for @Network.Wai.Request@:
--
-- @(==) = foldBins (&&) (==) True [requestMethod, rawPathInfo, rawQueryString] :: Request -> Request -> Bool@
--
-- To demonstrate short-circuiting:
--
-- >>> foldBins (||) (==) False [Left . fst, undefined] (4, undefined) (4, undefined)
-- ⊥
--
-- If any of the @undefined@'s are there, the whole computation bottoms-out.
foldBins :: Foldable t => (c -> b1 -> c) -> (b2 -> b2 -> b1) -> c -> t (a -> b2) -> a -> a -> c
foldBins f1 f2 x = curry . foldr (flip $ \t -> liftA2 f1 t . uncurry . on f2) (pure x)

-- | Strict left fold version of @foldBins@. Note that, despite being a left fold, it has a function signature identical to foldBins.
--
-- Does not short-circuit as easily as @foldBins@:
--
-- >>> foldBins' (||) (==) False [Left . fst, undefined] (4, undefined) (4, undefined)
-- True
--
-- >>> foldBins' (||) (==) False [Left . fst, undefined] (4, undefined) (3, undefined)
-- ⊥
foldBins' :: Foldable t => (c -> b1 -> c) -> (b2 -> b2 -> b1) -> c -> t (a -> b2) -> a -> a -> c
foldBins' f1 f2 x = curry . foldl' (\t -> liftA2 f1 t . uncurry . on f2) (pure x)

-- | Numeric has showHex and showOct, but not showBin? What.
showBin :: (Show a, Integral a) => a -> ShowS
showBin = showIntAtBase 2 intToDigit

{-
-- | writes state to file, database, &c (MonadCatch m => s -> m ()), every n actions, state updates, or upon predicate satisfaction (predicate is checked after every monadic action)
-- should I have a new data type for this, though? It's so simple: it's just a StateT with (>>=) = (>>=) >=> (bool' (return ()) writeState p =<< get)
newtype SaveStateT = SaveStateT {runWST :: StateT s m a}
instance Monad SaveStateT where
    m >>= f = 
instance MonadTrans SaveStateT 
-}

-- | 'Control.Monad.replicateM' generalized to monoids
replicateM' :: (Applicative f, Monoid m) => Int -> f m -> f m
replicateM' cnt0 f = g cnt0
    where
        g cnt
            | cnt <= 0  = pure mempty
            | otherwise = liftA2 mappend f (g (cnt - 1))

-- | 'Data.Foldable.foldMap' generalized to monads
foldMapM :: (Foldable t, Monad m, Monoid b) => (a -> m b) -> t a -> m b
foldMapM f = foldM (\acc -> fmap (mappend acc) . f) mempty

-- | 'Data.List.partition' generalized to monads and Foldables
partitionM :: (Foldable t, Monad m) => (a -> m Bool) -> t a -> m ([a], [a])
partitionM p = foldMapM (\a -> bool (mempty, pure a) (pure a, mempty) <$> p a)

-- | 'Data.Foldable.find' generalized to monads
findM :: (Foldable t, Monad m) => (a -> m Bool) -> t a -> m (Maybe a)
findM p = liftM getAlt . foldMapM (\x -> Alt . bool Nothing (Just x) <$> p x)
