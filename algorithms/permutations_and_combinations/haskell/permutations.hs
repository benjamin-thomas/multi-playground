-- echo ./permutations.hs | entr -c doctest /_

{- |

=== SUBSETS ===

If a set contains `n` elements, then the number of subsets of the set is 2^n.

So, if we have an input of ['A', 'B', 'C'], then the number of subsets will be:

2^3 = 8

For the subsets:
  [[], ['A'], ['B'], ['C'], ['A', 'B'], ['A', 'C'], ['B', 'C'], ['A', 'B', 'C']]


To visualize things, refer the file `subsets.dot`.

>>> subsets ['A', 'B', 'C']
["","C","B","BC","A","AC","AB","ABC"]

>>> subsets [1, 2, 3]
[[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]
-}
subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x : xs) =
    let left = subsets xs
        right = map (x :) (subsets xs)
     in left ++ right

{- |

=== PERMUTATIONS ===

Question: in how many different ways can you arrange x items?

For a list of 3 items, there are   3*2*1 = 3! =  6 permutations (aka arrangements).
For a list of 4 items, there are 4*3*2*1 = 4! = 24 permutations.

>>> permutations ['A', 'B', 'C']
["ABC","ACB","BAC","BCA","CAB","CBA"]

>>> permutations [1, 2, 3]
[[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
-}
permutations :: (Eq a) => [a] -> [[a]]
permutations [] = [[]]
permutations rest =
    concatMap
        ( \x ->
            map
                (x :)
                (permutations (filter (/= x) rest))
        )
        rest