module HeapSpec where

import Data.List (sort)
import Heap
import Safe (atMay, minimumMay)
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "MaxiphobicHeap" $ do
    prop "gets minimum element" $ \xs ->
      let heap = fromList xs :: MaxiphobicHeap Int
       in minElement heap `shouldBe` minimumMay xs

    prop "gets 2nd minimum element" $ \xs ->
      let heap = deleteMinElement $ fromList xs :: MaxiphobicHeap Int
       in minElement heap `shouldBe` atMay (sort xs) 1
