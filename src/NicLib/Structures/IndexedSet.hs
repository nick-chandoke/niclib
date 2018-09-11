{-# LANGUAGE NamedFieldPuns, ViewPatterns, RecordWildCards, TypeFamilies #-}
-- | structures that reduce memory use by not storing redundant data
module NicLib.Structures.IndexedSet
( IndexedSet
, pop
, (!)
, lookup
, delete
, deleteIfExists
, insert
, adjust
, empty
, singleton
, null
, size
, member
, notMember
) where
import Data.Function ((&))
import Data.IntMap.Strict (IntMap)
import Data.Map.Strict (Map)
import Prelude hiding (lookup, null)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import GHC.Exts
import Data.Binary

-- | The idea here is that you can insert an item into a map, and you'll get back an index by which you can retrieve it later. The motivation for this is that you can store many Int references to a value, but only one copy of the value is held in memory (well, by current implementation, actually two.)
-- Thus, for storing lots of data with common attributes (e.g. strings with common prefixes, URLs with common schemes or domains), memory usage is greatly reduced.
-- Fortunately, because there's a bijection between indicies and values, you can compare equality of two values simply by comparing their numbers. However, remember that the indicies are unordered! The Ordering of two indicies tells which one was inserted into the map first - nothing else!
data IndexedSet a = IndexedSet { nextIndex :: Int, nums :: IntMap a, vals :: Map a Int } deriving Eq

-- TODO: replace with store instance
instance Binary a => Binary (IndexedSet a) where
    put (IndexedSet {..}) = do
        put nextIndex
        put nums
        put vals
    get = IndexedSet <$> get <*> get <*> get

instance Show a => Show (IndexedSet a) where
    show = show . M.toList . vals

infixl 4 !
(!) :: Ord a => IndexedSet a -> Int -> a
(IndexedSet {nums}) ! i = nums IM.! i

lookup :: Ord a => Int -> IndexedSet a -> Maybe a
lookup i (IndexedSet {nums}) = IM.lookup i nums

-- | deletes an item at a given index, or calls error if nothing at that index
delete :: Ord a => Int -> IndexedSet a -> IndexedSet a
delete i s@(IndexedSet {nums, vals}) = maybe
    (error "IndexedSet: Trying to delete at a non-existant index.")
    (\v -> s {nums = IM.delete i nums, vals = M.delete v vals})
    (IM.lookup i nums)

-- | get an element from the set
pop :: Ord a => IndexedSet a -> Maybe a
pop (null -> False) = Nothing
pop (IndexedSet {nums, vals}) = Just . fst $ M.elemAt 0 vals

empty :: IndexedSet a
empty = IndexedSet 0 IM.empty M.empty

singleton :: Ord a => a -> (Int, IndexedSet a)
singleton a = (0, IndexedSet 1 (IM.singleton 0 a) (M.singleton a 0))

-- | deletes an item at a given index, if an item exists at that index
deleteIfExists :: Ord a => Int -> IndexedSet a -> IndexedSet a
deleteIfExists i s@(IndexedSet {nums, vals}) = maybe
    s
    (\v -> s {nums = IM.delete i nums, vals = M.delete v vals})
    (IM.lookup i nums)

-- | Change value at a given index
adjust :: Ord a => (a -> a) -> Int -> IndexedSet a -> IndexedSet a
adjust endo i s@(IndexedSet {nums, vals}) = case IM.lookup i nums of
    Just a -> s {nums = IM.adjust endo i nums, vals = M.insert (endo a) i $ M.delete a vals}
    Nothing -> s

-- | inserts an element into the set if set doesn't already contain it. Returns the index of that element.
insert :: Ord a => a -> IndexedSet a -> (Int, IndexedSet a)
insert a s@(IndexedSet {nextIndex, nums, vals}) = case M.lookup a vals of
    Just i -> (i, s)
    Nothing -> (nextIndex, IndexedSet {nextIndex = succ nextIndex, nums = IM.insert nextIndex a nums, vals = M.insert a nextIndex vals})

null :: Ord a => IndexedSet a -> Bool
null = M.null . vals

size :: Ord a => IndexedSet a -> Int
size = M.size . vals

member :: Ord a => a -> IndexedSet a -> Bool
member a (IndexedSet {vals}) = M.member a vals

notMember :: Ord a => a -> IndexedSet a -> Bool
notMember a (IndexedSet {vals}) = M.notMember a vals

instance Ord a => IsList (IndexedSet a) where
    type Item (IndexedSet a) = a
    fromList xs = IndexedSet (length xs) (IM.fromList (zip [0..] xs)) (M.fromList (zip xs [0..]))
    toList = M.keys . vals
