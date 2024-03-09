-- echo ./solution.hs | entr -c doctest /_

{- |

>>> mergeAlternately "" ""
""

>>> mergeAlternately "ABC" "abc"
"AaBbCc"

>>> mergeAlternately "AB" "ab_cde"
"AaBb_cde"

>>> mergeAlternately "AB_CDE" "ab"
"AaBb_CDE"
-}
mergeAlternately :: String -> String -> String
mergeAlternately [] [] = []
mergeAlternately [] b = b
mergeAlternately a [] = a
mergeAlternately (a : as) (b : bs) = a : b : mergeAlternately as bs
