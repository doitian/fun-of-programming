module Editor (module Editor) where

type BufferI = (String, String)

-- | The empty buffer
emptyI :: BufferI
emptyI = ("", "")

-- | Inserts character before cursor
insertI :: Char -> BufferI -> BufferI
insertI c (before, after) = (c : before, after)

-- | Deletes character before cursor
deleteI :: BufferI -> BufferI
deleteI (before, after) = (drop 1 before, after)

-- | Moves cursor left one character
leftI :: BufferI -> BufferI
leftI (c : before, after) = (before, c : after)
leftI buf = buf

-- | Moves cursor right one character
rightI :: BufferI -> BufferI
rightI (before, c : after) = (c : before, after)
rightI buf = buf

-- | Tells if cursor at left end
atLeftI :: BufferI -> Bool
atLeftI (before, _) = null before

-- | Tells if cursor at left end
atRightI :: BufferI -> Bool
atRightI (_, after) = null after

getText :: BufferI -> String
getText (before, after) = reverse before ++ after

getCursor :: BufferI -> Int
getCursor (before, _) = length before
