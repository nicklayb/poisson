module Duration exposing (Duration, diffToString, fromDataPoints, fromInt, toInt)

import DataPoint exposing (DataPoint)
import Time
import Unit exposing (Unit(..))


type alias Duration =
    { start : Maybe Time.Posix
    , end : Maybe Time.Posix
    , raw : Int
    , milli : Int
    , seconds : Int
    , minutes : Int
    , hours : Int
    , days : Int
    }


fromDataPoints : DataPoint -> DataPoint -> Duration
fromDataPoints left right =
    let
        end =
            left.time

        start =
            right.time

        diff =
            DataPoint.diff left right

        duration =
            fromInt diff
    in
    { duration | start = Just start, end = Just end }


fromInt : Int -> Duration
fromInt diff =
    let
        days =
            diff // Unit.dayInMs

        hours =
            (diff - days * Unit.dayInMs) // Unit.hourInMs

        minutes =
            (diff - days * Unit.dayInMs - hours * Unit.hourInMs) // Unit.minuteInMs

        seconds =
            (diff - days * Unit.dayInMs - hours * Unit.hourInMs - minutes * Unit.minuteInMs) // Unit.secondInMs

        milli =
            diff - days * Unit.dayInMs - hours * Unit.hourInMs - minutes * Unit.minuteInMs - seconds * Unit.secondInMs
    in
    { start = Nothing
    , end = Nothing
    , raw = diff
    , milli = milli
    , seconds = seconds
    , minutes = minutes
    , hours = hours
    , days = days
    }


toInt : Duration -> Int
toInt { milli, seconds, minutes, hours, days } =
    milli + seconds * Unit.secondInMs + minutes * Unit.minuteInMs + hours * Unit.hourInMs + days * Unit.dayInMs


type alias DiffConfig =
    { simple : Bool
    }


diffToString : DiffConfig -> (Int -> Unit -> a) -> Duration -> List a
diffToString diffConfig function duration =
    let
        parts =
            [ ( duration.days, Day )
            , ( duration.hours, Hour )
            , ( duration.minutes, Minute )
            , ( duration.seconds, Second )
            , ( duration.milli, Millisecond )
            ]

        highest units =
            case units of
                ( value, unit ) :: rest ->
                    if value > 0 then
                        [ function value unit ]

                    else
                        highest rest

                _ ->
                    []

        allParts =
            parts
                |> List.filter (\( value, _ ) -> value > 0)
                |> List.map (\( value, unit ) -> function value unit)
    in
    if diffConfig.simple then
        highest parts

    else
        allParts
