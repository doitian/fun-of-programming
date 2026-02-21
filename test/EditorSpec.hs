module EditorSpec where

import Editor
import Test.Hspec
import Test.Hspec.QuickCheck

type Buffer = (Int, String)

-- | The empty buffer
empty :: Buffer
empty = (0, "")

-- | Inserts character before cursor
insert :: Char -> Buffer -> Buffer
insert c (pos, text) = (pos + 1, take pos text ++ [c] ++ drop pos text)

-- | Deletes character before cursor
delete :: Buffer -> Buffer
delete (pos, text)
  | pos > 0 = (pos - 1, take (pos - 1) text ++ drop pos text)
  | otherwise = (pos, text)

-- | Moves cursor left one character
left :: Buffer -> Buffer
left (pos, text)
  | pos > 0 = (pos - 1, text)
  | otherwise = (pos, text)

-- | Moves cursor right one character
right :: Buffer -> Buffer
right (pos, text)
  | pos < length text = (pos + 1, text)
  | otherwise = (pos, text)

-- | Tells if cursor at left end
atLeft :: Buffer -> Bool
atLeft (0, _) = True
atLeft _ = False

-- | Tells if cursor at left end
atRight :: Buffer -> Bool
atRight (pos, text) = pos == length text

retrieve :: BufferI -> Buffer
retrieve buf = (getCursor buf, getText buf)

spec :: Spec
spec = do
  describe "BufferI" $ do
    it "insert consecutive text" $ do
      (getText $ foldl' (flip insertI) emptyI "hello") `shouldBe` "hello"

    it "emptyI" $ do
      retrieve emptyI `shouldBe` empty

    prop "insertI" $ \c buf ->
      retrieve (insertI c buf) `shouldBe` insert c (retrieve buf)

    prop "deleteI" $ \buf ->
      retrieve (deleteI buf) `shouldBe` delete (retrieve buf)

    prop "leftI" $ \buf ->
      retrieve (leftI buf) `shouldBe` left (retrieve buf)

    prop "rightI" $ \buf ->
      retrieve (rightI buf) `shouldBe` right (retrieve buf)

    prop "atLeftI" $ \buf ->
      atLeftI buf `shouldBe` atLeft (retrieve buf)

    prop "atRightI" $ \buf ->
      atRightI buf `shouldBe` atRight (retrieve buf)

    prop "insertI then atLeftI" $ \buf ->
      (atLeftI $ insertI 'x' buf) `shouldBe` False

    prop "insertI then atRightI" $ \buf ->
      (atRightI $ insertI 'x' buf) `shouldBe` atRightI buf

    prop "deleteI then atLeftI" $ \buf ->
      (atLeftI $ deleteI buf) `shouldBe` (getCursor buf <= 1)

    prop "deleteI then atRightI" $ \buf ->
      (atRightI $ deleteI buf) `shouldBe` atRightI buf

    prop "insertI then deleteI" $ \buf ->
      (deleteI $ insertI 'x' buf) `shouldBe` buf
