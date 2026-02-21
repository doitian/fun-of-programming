module HeapSpec where

import Data.List (sort)
import Heap
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "MaxiphobicHeap" $ do
    prop "sorts elements" $ \xs ->
      let xs' = toList $ (fromList xs :: MaxiphobicHeap Int)
       in xs' `shouldBe` sort xs
