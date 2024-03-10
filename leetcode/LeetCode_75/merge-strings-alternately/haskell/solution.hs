{-

To just compile/type check:
    echo ./solution.hs | entr -c ghc -fno-code /_

To compile and run tests:
    echo ./solution.hs | entr -c doctest /_

Or use this dedicated tool:
    ghcid --lint --test=':!doctest ./solution.hs' ./solution.hs

 -}
module Solution where

import Control.Monad (when)
import Data.Text (Text)
import Data.Text qualified as T

{- |

>>> merge "" ""
""

>>> merge "ABC" "abc"
"AaBbCc"

>>> merge "AB" "ab_cde"
"AaBb_cde"

>>> merge "AB_CDE" "ab"
"AaBb_CDE"
-}
merge :: String -> String -> String
merge [] [] = []
merge [] y = y
merge x [] = x
merge (x : xs) (y : ys) = x : y : merge xs ys

{- |

>>> mergeAlt (T.pack"") (T.pack"")
""

>>> mergeAlt (T.pack "ABC") (T.pack "abc")
"AaBbCc"

>>> mergeAlt (T.pack "AB") (T.pack "ab_cde")
"AaBb_cde"

>>> mergeAlt (T.pack "AB_CDE") (T.pack "ab")
"AaBb_CDE"
-}

-- mergeAlt :: String -> String -> Text
-- mergeAlt a b =
--     aux (T.uncons $ T.pack a, T.uncons $ T.pack b)
--   where
--     aux :: (Maybe (Char, Text), Maybe (Char, Text)) -> Text
--     aux (Nothing, Nothing) = T.empty
--     aux (Just (x, xs), Nothing) = T.singleton x <> aux (T.uncons xs, Nothing)
--     aux (Nothing, Just (y, ys)) = T.singleton y <> aux (Nothing, T.uncons ys)
--     aux (Just (x, xs), Just (y, ys)) = T.singleton x <> T.singleton y <> aux (T.uncons xs, T.uncons ys)

mergeAlt :: Text -> Text -> Text
mergeAlt a b = T.concat $ aux (T.uncons a, T.uncons b)
  where
    aux :: (Maybe (Char, Text), Maybe (Char, Text)) -> [Text]
    aux (Nothing, Nothing) = []
    aux (Just (x, xs), Nothing) = T.singleton x : aux (T.uncons xs, Nothing)
    aux (Nothing, Just (y, ys)) = T.singleton y : aux (Nothing, T.uncons ys)
    aux (Just (x, xs), Just (y, ys)) = T.singleton x : T.singleton y : aux (T.uncons xs, T.uncons ys)

test1 :: Int -> String
test1 n = merge (replicate n 'a') (replicate n 'B')

test2 :: Int -> Text
test2 n = mergeAlt (T.pack $ replicate n 'a') (T.pack $ replicate n 'B')

{-

Haskell is super slow in comparaison to OCaml.
I'm not sure if it's related to actually printing the result though, I couldn't find a way to stop it.
If I override the return value with unit, then the expression doesn't get evaluated at all.

NOTE: using Text doesn't seem to provide any noticeable improvement, I'm probably doing something wrong.

ghci> test1 99_999
(1.64 secs, 188,071,224 bytes)

ghci> test2 99_999
(1.82 secs, 300,795,016 bytes)

I should probably use something like this next time:
    http://www.serpentine.com/criterion/tutorial.html

 -}