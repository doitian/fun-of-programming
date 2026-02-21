module HeapSpec where

import Data.List (sort)
import Heap
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  heapSpec "MaxiphobicHeap" (fromList :: [Int] -> MaxiphobicHeap Int)
  heapSpec "RoundRobinHeap" (fromList :: [Int] -> RoundRobinHeap Int)
  heapSpec "SkewHeap" (fromList :: [Int] -> SkewHeap Int)

heapSpec :: (HeapTag t) => String -> ([Int] -> Heap t Int) -> Spec
heapSpec name factory = do
  describe name $ do
    prop "sorts elements" $ \xs ->
      toList (factory xs) `shouldBe` sort xs
