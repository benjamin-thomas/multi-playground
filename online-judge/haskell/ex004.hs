import Data.Char (digitToInt)

{-
>>> solution ["123", "456", "789"]
14

>>> solution ["123456789"]
4

>>> solution ["14329", "7568"]
10
 -}

solution :: [String] -> Int
solution xs =
    length $
        filter
            (even . sum . fmap digitToInt)
            (sequence xs)
