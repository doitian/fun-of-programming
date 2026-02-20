module MaxiphobicHeap where

type Size = Int

data (Ord a) => MaxiphobicHeap a = Null | Fork Size a (MaxiphobicHeap a) (MaxiphobicHeap a)

-- |
-- >>> isEmpty Null
isEmpty :: MaxiphobicHeap a -> Bool
isEmpty Null = True
isEmpty (Fork _ _ _ _) = False
