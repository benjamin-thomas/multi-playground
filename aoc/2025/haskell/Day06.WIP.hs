{- OPTIONS_GHC -Wall #-}

import Data.Maybe
    ( Maybe(..)
    ,
    )

hello =
    [ [ 1, 5,  9 ]
    , [ 2, 6, 10 ]
    , [ 3, 7, 11 ]
    , [ 4, 8, 12 ]
    ]

transpose1 [] = []
transpose1 xss =
    let
        plucked = map (\xs -> (head xs,tail xs)) xss
        heads   = map fst plucked
        tails   = map snd plucked
    in
    heads : (if concat tails == [] then [] else transpose1 tails)


-- Data.List.uncons
uncons [] = Nothing
uncons (x:xs) = Just (x, xs)

-- Data.Maybe.mapMaybe
mapMaybe f =
    foldr
        (\mx xs ->
            case f mx of
                Nothing -> xs
                Just x  -> x:xs
        )
    []

transpose xss =
    case mapMaybe uncons xss of
        []    -> []
        pairs -> map fst pairs : transpose (map snd pairs)
