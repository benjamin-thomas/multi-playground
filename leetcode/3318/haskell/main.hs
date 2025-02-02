import Data.List qualified as List
import Data.Map qualified as Map

{- |

>>> takeExactly 3 [1,2,3,4]
[1,2,3]

>>> takeExactly 3 [1,2,3]
[1,2,3]

>>> takeExactly 3 [1,2]
[]
-}
takeExactly :: (Eq n, Num n) => n -> [a] -> [a]
takeExactly =
  aux []
 where
  aux acc 0 _ = reverse acc
  aux acc n [] = []
  aux acc n (x : xs) = aux (x : acc) (n - 1) xs

{- |
>>> makeWindow 3 [1,2,3,4,5]
[[1,2,3],[2,3,4],[3,4,5]]

>>> makeWindow 6 [1,1,2,2,3,4,2,3]
[[1,1,2,2,3,4],[1,2,2,3,4,2],[2,2,3,4,2,3]]
-}
makeWindow :: (Eq n, Num n) => n -> [a] -> [[a]]
makeWindow n lst =
  aux [] n lst
 where
  aux acc n lst =
    case takeExactly n lst of
      [] -> reverse acc
      window -> aux (window : acc) n (drop 1 lst)

{- |
>>> occurrences [1,1,2,2,3,4]
[(2,2),(1,2),(4,1),(3,1)]
-}
occurrencesDesc :: (Ord k, Ord v, Num v) => [k] -> [(k, v)]
occurrencesDesc lst =
  List.sortBy
    ( \(ka, va) (kb, vb) ->
        case compare vb va of
          EQ -> compare kb ka
          x -> x
    )
    $ Map.toList
    $ Map.fromListWith (+) [(x, 1) | x <- lst]

{- |
2*3 + 4*5 = 26
>>> sumOccurrences [(2,3),(4,5)]
26
-}
sumOccurrences :: (Num a) => [(a, a)] -> a
sumOccurrences lst = sum $ map (uncurry (*)) lst

{- |
>>> solve 6 2 [1,1,2,2,3,4,2,3]
28

>>> solve 2 2 [3,8,7,8,7,5]
68
-}
solve :: (Ord a, Num a, Num k, Eq k) => k -> Int -> [a] -> a
solve k x lst =
  sum $
    sumOccurrences . take x . occurrencesDesc <$> makeWindow k lst
