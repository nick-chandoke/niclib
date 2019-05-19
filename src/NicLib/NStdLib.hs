-- | Generally useful functions
module NicLib.NStdLib
( -- * Operators
  (!!?)
, (%>)
, (<%)
, (?)
, (↔)
-- * Convenience Functions
, bool'
, maybeC
, boolC
, both
, cT
, liftME
, describeIOError
, showBin
, displayBS
-- * Monad Transformer Morphisms
, morphism239
, morphism240
, morphism46
-- * Sets & Sequences
, sxor
, findM
, foldBins
, foldBins'
, partitionM
, replicateM'
-- * Ordering Comonad
, OrderBy(..)
, orderBy
) where

-- rio & base
import RIO hiding (GT, LT, EQ, (.), id)
import RIO.Set (Set)
import Control.Applicative
import Control.Category
import Data.Bifunctor as BiF
import Data.ByteString.Builder (byteString)
import Data.Char (intToDigit)
import Data.Function (on, fix) -- importing everything messes with the ArrowCurry definition
import Data.Monoid (Alt(..))
import Numeric (showIntAtBase)
import Prelude (ShowS)
import System.IO.Error
import qualified RIO.Set as S

-- comonad
import Control.Comonad
import Control.Comonad.Env

-- mono-traversable
import Data.Sequences
import Data.MonoTraversable

-- | @OrderBy@ inherits @Eq@ & @Ord@ instances from left parameter, ignoring right parameter
--
-- Before using @OrderBy@, consider that @Set (OrderBy ord a)@ is isomorphic with @Map ord a@. Prefer the latter.
--
-- Behaves similarly to (,a) but notably lacks @Applicative@ and @ComonadApply@ or more specific instances (e.g. @Monad@).
data OrderBy ord a = OrderBy ord a deriving (Show, Functor)
instance Eq ord => Eq (OrderBy ord a) where OrderBy ord _ == OrderBy c _ = ord == c
instance Ord ord => Ord (OrderBy ord a) where compare (OrderBy ord _) (OrderBy c _) = compare ord c
instance Comonad (OrderBy ord) where
    extract (OrderBy _ a) = a
    duplicate x@(OrderBy ord _) = OrderBy ord x
    extend cok x@(OrderBy ord _) = OrderBy ord (cok x)
instance ComonadEnv ord (OrderBy ord) where
    ask (OrderBy ord _) = ord

orderBy :: (a -> ord) -> a -> OrderBy ord a
orderBy f = OrderBy <$> f <*> id 

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

-- | Apply an operation to both objects of a Bifunctor
both :: Bifunctor p => (a -> d) -> p a a -> p d d
both f = bimap f f

-- TODO: probably can be done better by using NicLib.Set.diff
-- | Exclusive-or for sets
sxor :: Ord a => Set a -> Set a -> Set a
sxor r s = S.union (r S.\\ s) (s S.\\ r)

-- | Like 'Data.Bool.bool', but allows pointfree manipulation like 'Data.Maybe.maybe' (for (->) Applicative, its most common use; or, sometimes one may use it in other Applicatives).
--
-- @bool' ((-) 10) (+1) (\>3)@ equals (in the (->) monoidal functor) @\\n -> if n < 3 then n + 1 else n - 10@@
bool' :: Applicative f => f d -> f d -> f Bool -> f d
bool' = liftA3 bool

-- | "Continuation(-like) maybe" 'maybe' that looks better when nesting non-monadic @Maybe@s, /e.g./ for these types of situations:
--
-- @
-- maybeC (readMaybe \@Int val)
--     (throw InvalidRead)
--     $ \\ival -\> boolC (ival /= 0)
--         (putStrLn "bad input: 0")
--         $ do
--             c \<- readFile "file"
--             ⋮
-- @
maybeC :: Maybe a -> b -> (a -> b) -> b
maybeC m n j = maybe n j m

-- | "Continuation(-like) bool" 'bool' that looks like CPS style. See 'maybeC' for example.
boolC :: Bool
      -> a -- ^ on False
      -> a -- ^ on True
      -> a
boolC b f t = bool f t b

-- | An efficient version of "if index in bounds then pure (item at index) else empty."
(!!?) :: (Num n, Ord n, IsSequence seq, item ~ Element seq, Alternative f) => seq -> n -> f item
xs !!? n
    | n < 0 = empty -- still works if negative, but faster to quit immediately if negative, rather than traverse a whole list just to return Nothing
    | otherwise = fix (\f i -> uncons >>> \case Nothing -> empty; Just (h,t) -> if i == n then pure h else f (i + 1) t) 0 xs

-- | @morphism239@ performs a composition of functions on monad transformers that share a common inner monad and constructor but have different parameters, e.g. @State Text m@ and @State String m@, or @ExceptT e m@ and @AccumT w m@. It's almost a generalization of mapAccumT, mapExceptT, etc., or like an inverse of @hoist@. It doesn't allow the inner monad to change, but does allow the outer monad to change.
--
-- Here's an example:
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
-- Note that f and f' must be the same level of monad transformer! For instance, the following does not work:
--
-- @
-- runStack = flip runAccumT mempty . runExceptT
-- morphism239 (flip runAccumT mempty) undefined runStack
-- @
--
-- GHC will give an infinite type error because @runStack@ acts on a 2-transformer stack (a @BugT@) but runAccumT runs on a 1-transformer stack (namely @AccumT@). What one could like to concieve is:
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
            => (t m a -> m b) -- ^ unwrap the first transformer. Usually run*, e.g. @runStateT@
            -> (b -> s m x) -- ^ a morphism that relates the prior two unwrapping functions
            -> (s m x -> m y) -- ^ unwrap the second transformer. Usually the prior run* function
            -> t m a
            -> m y
morphism239 = morphism240

-- You may be wondering why have both @f@ and @f'@; when I created 240, they were the same function! They remain here for the time being; I'm considering making morphism240 :: m b -> (b -> c) -> (c -> m d) -> m d. I'm unsure how that'll affect clarity of code - especially that that uses 239.
-- | So 'morphism239' didn't cut it for ya? As you can see, 240 strings-together two Kleisli morphisms via a static morphism.
morphism240 :: Monad m => (a -> m b) -> (b -> c) -> (c -> m d) -> a -> m d
morphism240 f t f' x = f x >>= f' . t

-- | Originally written for a @Monoid@ instance for @WriterT@
morphism46 :: Applicative f => (f c -> d) -> (a -> f b) -> (b -> b -> c) -> a -> a -> d
morphism46 wrap unwrap g = wrap <% on (liftA2 g) unwrap

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
--
-- see also [https://gist.github.com/i-am-tom/8ce5fd5dbce2a71fe604934d774a08f8](https://gist.github.com/i-am-tom/8ce5fd5dbce2a71fe604934d774a08f8)
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

-- | Somewhy @ByteString@ doesn't instance @Display@. Rather than orphan one, here's a function.
displayBS :: ByteString -> Utf8Builder
displayBS = Utf8Builder . byteString

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

-- | 'Data.List.partition' generalized to monads and Foldables
partitionM :: (Foldable t, Monad m) => (a -> m Bool) -> t a -> m ([a], [a])
partitionM p = foldMapM (\a -> bool (mempty, pure a) (pure a, mempty) <$> p a)

-- | 'Data.Foldable.find' generalized to monads
findM :: (Foldable t, Monad m) => (a -> m Bool) -> t a -> m (Maybe a)
findM p = liftM getAlt . foldMapM (\x -> Alt . bool Nothing (Just x) <$> p x)

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

-- | Lift a Maybe into Either
liftME :: l -> Maybe a -> Either l a
liftME l Nothing = Left l
liftME _ (Just x) = Right x
