module Heap
  ( Heap (..),
    insert,
    isEmpty,
    minElement,
    deleteMinElement,
    fromList,
    toList,
    HeapTag,
    MaxiphobicHeap,
    RoundRobinHeap,
    SkewHeap,
  )
where

import Data.List (sortOn)

data Heap t a = Null | Fork t a (Heap t a) (Heap t a) deriving (Show)

class HeapTag t where
  makeHeapTag :: t

  -- | Join two heaps. Both heaps are not empty and the first heap min element is not larger than the second heap.
  joinHeaps :: (Ord a) => Heap t a -> Heap t a -> Heap t a

newtype MaxiphobicHeapTag = MaxiphobicHeapTag Int deriving (Eq, Ord, Show)

instance Semigroup MaxiphobicHeapTag where
  MaxiphobicHeapTag x <> MaxiphobicHeapTag y = MaxiphobicHeapTag $ x + y

instance HeapTag MaxiphobicHeapTag where
  makeHeapTag = MaxiphobicHeapTag 1

  joinHeaps (Fork t1 x a b) c@(Fork t2 _ _ _) = Fork (t1 <> t2) x a' (b' <> c')
    where
      -- Invert so Nothings will be merged first.
      (c', b', a') = case sortOn getTag [a, b, c] of
        [e0, e1, e2] -> (e0, e1, e2)
        _ -> error "impossible: expected exactly three heaps"
  joinHeaps h1 h2 = h1 <> h2

type MaxiphobicHeap a = Heap MaxiphobicHeapTag a

newtype RoundRobinHeapTag = RoundRobinHeapTag Bool deriving (Eq, Ord, Show)

instance HeapTag RoundRobinHeapTag where
  makeHeapTag = RoundRobinHeapTag False

  joinHeaps (Fork (RoundRobinHeapTag False) x a b) c = Fork (RoundRobinHeapTag True) x (a <> c) b
  joinHeaps (Fork (RoundRobinHeapTag True) x a b) c = Fork (RoundRobinHeapTag False) x a (b <> c)
  joinHeaps h1 h2 = h1 <> h2

type RoundRobinHeap a = Heap RoundRobinHeapTag a

newtype SkewHeapTag = SkewHeapTag () deriving (Eq, Ord, Show)

instance HeapTag SkewHeapTag where
  makeHeapTag = SkewHeapTag ()

  joinHeaps (Fork t x a b) c = Fork t x b (a <> c)
  joinHeaps h1 h2 = h1 <> h2

type SkewHeap a = Heap SkewHeapTag a

instance (Ord a, HeapTag t) => Semigroup (Heap t a) where
  h <> Null = h
  Null <> h = h
  h1 <> h2
    | minElement h1 <= minElement h2 = joinHeaps h1 h2
    | otherwise = joinHeaps h2 h1

instance (Ord a, HeapTag t) => Monoid (Heap t a) where
  mempty = Null

-- |
-- >>> isEmpty Null
-- True
isEmpty :: Heap t a -> Bool
isEmpty Null = True
isEmpty (Fork _ _ _ _) = True

-- | >>> minElement $ Fork 1 10 Null Null
-- Just 10
minElement :: Heap t a -> Maybe a
minElement (Fork _ x _ _) = Just x
minElement _ = Nothing

getTag :: Heap t a -> Maybe t
getTag (Fork t _ _ _) = Just t
getTag _ = Nothing

-- |
-- >>> deleteMinElement $ Fork (MaxiphobicHeapTag 1) 10 Null Null
-- Null
deleteMinElement :: (Ord a, HeapTag t) => Heap t a -> Heap t a
deleteMinElement Null = Null
deleteMinElement (Fork _ _ a b) = a <> b

-- |
-- >>> insert 10 . insert 5 . insert 15 $ Null :: MaxiphobicHeap Int
-- Fork (MaxiphobicHeapTag 3) 5 (Fork (MaxiphobicHeapTag 1) 10 Null Null) (Fork (MaxiphobicHeapTag 1) 15 Null Null)
-- >>> insert 10 . insert 5 . insert 15 $ Null :: RoundRobinHeap Int
-- Fork (RoundRobinHeapTag False) 5 (Fork (RoundRobinHeapTag False) 15 Null Null) (Fork (RoundRobinHeapTag False) 10 Null Null)
-- >>> insert 10 . insert 5 . insert 15 $ Null :: SkewHeap Int
-- Fork (SkewHeapTag ()) 5 (Fork (SkewHeapTag ()) 15 Null Null) (Fork (SkewHeapTag ()) 10 Null Null)
insert :: (Ord a, HeapTag t) => a -> Heap t a -> Heap t a
insert x h = Fork makeHeapTag x Null Null <> h

fromList :: (Ord a, HeapTag t) => [a] -> Heap t a
fromList = foldr insert Null

toList :: (Ord a, HeapTag t) => Heap t a -> [a]
toList Null = []
toList (Fork _ x a b) = x : (toList $ a <> b)

instance Functor (Heap t) where
  fmap _ Null = Null
  fmap f (Fork t x a b) = Fork t (f x) (fmap f a) (fmap f b)
