import Prelude hiding (last)

{-

Source: https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems

Run the tests
echo ./lists.hs | entr -c doctest /_

Play with the REPL
ghci ./lists.hs
ghci> :cmd return $ unlines [":reload", "my_func"]

 -}

-------------------------------------------------------------------------------

{- | Problem 1

(*) Find the last element of a list.

>>> last []
Nothing

>>> last [1]
Just 1

>>> last [1,2,3]
Just 3
-}
last :: [a] -> Maybe a
last [] = Nothing
last [x] = Just x
last (_ : xs) = last xs

{- |
>>> last' []
Nothing

>>> last' [1]
Just 1

>>> last' [1,2,3]
Just 3
-}
last' :: [a] -> Maybe a
last' lst = case lst of
    [] -> Nothing
    [x] -> Just x
    _ : xs -> last' xs

-------------------------------------------------------------------------------

{- | Problem 2
(*) Find the last-but-one (or second-last) element of a list.

>>> penultimate []
Nothing

>>> penultimate [1]
Nothing

>>> penultimate [1,2]
Just 1

>>> penultimate [1,2,3]
Just 2

>>> penultimate  ['a'..'z']
Just 'y'
-}
penultimate :: [a] -> Maybe a
penultimate [] = Nothing
penultimate [x, _] = Just x
penultimate (x : xs) = penultimate xs

-------------------------------------------------------------------------------

{- | Problem 3
(*) Find the K'th element of a list.

>>> nth 0 []
Nothing

>>> nth 0 [1,2,3]
Just 1

>>> nth 1 [1,2,3]
Just 2

>>> nth 2 [1,2,3]
Just 3

>>> nth 99 [1,2,3]
Nothing
-}
nth :: (Eq n, Num n) => n -> [a] -> Maybe a
nth _ [] = Nothing
nth 0 (x : xs) = Just x
nth n (_ : xs) = nth (n - 1) xs

-------------------------------------------------------------------------------