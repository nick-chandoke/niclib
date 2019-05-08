{-# language NoBangPatterns #-}

-- TODO: replace stateful stuff with IORef? Is that any better?
-- | Structures that reduce memory use by not storing redundant data
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

import Data.IntMap.Strict (IntMap)
import Data.Map.Strict (Map)
import Prelude hiding (lookup, null)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import GHC.Exts
import Data.Store (Store)
import GHC.Generics (Generic)

-- | The idea here is that you can insert an item into a map, and you'll get back an index by which you can retrieve it later. The motivation for this is that you can store many Int references to a value, but only one copy of the value is held in memory (well, by current implementation, actually two.)
--
-- In other words, it's like putting a row into a database, and generating & returning a primary key for it.
--
-- Thus, for storing lots of data with common attributes (e.g. URLs with common schemes or domains, or songs with common album names), memory usage is greatly reduced.
data IndexedSet a = IndexedSet
    { nextIndex :: Int
    , nums :: IntMap a
    , vals :: Map a Int
    } deriving (Generic, Eq)

instance (Ord a, Store a) => Store (IndexedSet a)

instance Show a => Show (IndexedSet a) where
    show = show . M.toList . vals

infixl 4 !
(!) :: Ord a => IndexedSet a -> Int -> a
IndexedSet {nums} ! i = nums IM.! i

lookup :: Ord a => Int -> IndexedSet a -> Maybe a
lookup i (IndexedSet {nums}) = IM.lookup i nums

-- | Deletes an item at a given index, or calls error if nothing at that index
delete :: Ord a => Int -> IndexedSet a -> IndexedSet a
delete i s@(IndexedSet {nums, vals}) = maybe
    (error "IndexedSet: Trying to delete at a non-existant index.")
    (\v -> s {nums = IM.delete i nums, vals = M.delete v vals})
    (IM.lookup i nums)

-- | Get an element from the set
pop :: Ord a => IndexedSet a -> Maybe a
pop (null -> False) = Nothing
pop (IndexedSet {vals}) = Just . fst $ M.elemAt 0 vals

empty :: IndexedSet a
empty = IndexedSet 0 IM.empty M.empty

singleton :: Ord a => a -> (Int, IndexedSet a)
singleton a = (0, IndexedSet 1 (IM.singleton 0 a) (M.singleton a 0))

-- | Deletes an item at a given index, if an item exists at that index
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

-- | Inserts an element into the set if set doesn't already contain it. Returns the index of that element.
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
