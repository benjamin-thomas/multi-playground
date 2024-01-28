module Check exposing (positiveNumber)

-- We can keep the same API, whether validing in sequence or in "parallel"


positiveNumber : Int -> Result String Int
positiveNumber n =
    if n > 0 then
        Ok n

    else
        Err ("Not positive: " ++ String.fromInt n)
