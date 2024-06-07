module DataPoint exposing (DataPoint, decoder, diff, toDateTime)

import DateTime exposing (DateTime)
import Json.Decode as Decode exposing (Decoder)
import Time exposing (..)


type alias DataPoint =
    { time : Time.Posix
    }


diff : DataPoint -> DataPoint -> Int
diff left right =
    let
        leftInt =
            Time.posixToMillis left.time

        rightInt =
            Time.posixToMillis right.time
    in
    leftInt - rightInt


decoder : Decoder DataPoint
decoder =
    let
        decodeMillis millis =
            Decode.map DataPoint (Decode.succeed (Time.millisToPosix millis))
    in
    Decode.int
        |> Decode.andThen decodeMillis


toDateTime : Time.Zone -> DataPoint -> DateTime
toDateTime timeZone dataPoint =
    DateTime.fromPosix timeZone dataPoint.time
