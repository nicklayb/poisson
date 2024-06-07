module DateTime exposing (DateTime, clockToString, fromPosix, monthToString)

import Time exposing (Month(..))


type alias DateTime =
    { raw : Time.Posix
    , second : Int
    , minute : Int
    , hour : Int
    , day : Int
    , month : Month
    , year : Int
    }


fromPosix : Time.Zone -> Time.Posix -> DateTime
fromPosix timeZone posix =
    let
        second =
            Time.toSecond timeZone posix

        minute =
            Time.toMinute timeZone posix

        hour =
            Time.toHour timeZone posix

        day =
            Time.toDay timeZone posix

        month =
            Time.toMonth timeZone posix

        year =
            Time.toYear timeZone posix
    in
    { raw = posix
    , second = second
    , minute = minute
    , hour = hour
    , day = day
    , month = month
    , year = year
    }


monthToString : Month -> String
monthToString month =
    case month of
        Jan ->
            "Jan"

        Feb ->
            "Feb"

        Mar ->
            "Mar"

        Apr ->
            "Apr"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Aug"

        Sep ->
            "Sep"

        Oct ->
            "Oct"

        Nov ->
            "Nov"

        Dec ->
            "Dec"


clockToString : DateTime -> String
clockToString dateTime =
    [ dateTime.hour
    , dateTime.minute
    , dateTime.second
    ]
        |> List.map (String.fromInt >> String.padLeft 2 '0')
        |> String.join ":"
