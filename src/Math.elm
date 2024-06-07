module Math exposing (average, ordinalize)


average : List Int -> Int
average ints =
    let
        sum =
            List.foldl (+) 0 ints

        count =
            List.length ints
    in
    sum // count


ordinalize : Int -> String
ordinalize int =
    let
        remainder =
            int // 10

        joinWith suffix =
            String.fromInt int ++ suffix
    in
    if int == 11 || int == 12 || int == 13 then
        joinWith "th"

    else if remainder == 1 then
        joinWith "st"

    else if remainder == 2 then
        joinWith "nd"

    else if remainder == 3 then
        joinWith "rd"

    else
        joinWith "th"
