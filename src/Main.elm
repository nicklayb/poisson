port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick)
import Http exposing (Error(..))
import Json.Decode as Decode exposing (Value)
import Task
import Time



-- ---------------------------
-- PORTS
-- ---------------------------


port storeDataPoints : List Int -> Cmd msg



-- ---------------------------
-- MODEL
-- ---------------------------


type alias Flags =
    Value


type alias DataPoint =
    { time : Time.Posix
    }


type alias Model =
    { dataPoints : List DataPoint
    , timeZone : Maybe Time.Zone
    , allVisible : Bool
    , simple : Bool
    }


type alias Duration =
    { raw : Int
    , milli : Int
    , seconds : Int
    , minutes : Int
    , hours : Int
    , days : Int
    }


type Unit
    = Millisecond
    | Second
    | Minute
    | Hour
    | Day


dayInMs : Int
dayInMs =
    hourInMs * 24


hourInMs : Int
hourInMs =
    minuteInMs * 60


minuteInMs : Int
minuteInMs =
    secondInMs * 60


secondInMs : Int
secondInMs =
    1000


unitToAbbreviation : Unit -> String
unitToAbbreviation unit =
    case unit of
        Millisecond ->
            "ms"

        Second ->
            "sec"

        Minute ->
            "min"

        Hour ->
            "hrs"

        Day ->
            "days"


toDuration : Int -> Duration
toDuration diff =
    let
        days =
            diff // dayInMs

        hours =
            (diff - days * dayInMs) // hourInMs

        minutes =
            (diff - days * dayInMs - hours * hourInMs) // minuteInMs

        seconds =
            (diff - days * dayInMs - hours * hourInMs - minutes * minuteInMs) // secondInMs

        milli =
            diff - days * dayInMs - hours * hourInMs - minutes * minuteInMs - seconds * secondInMs
    in
    { raw = diff
    , milli = milli
    , seconds = seconds
    , minutes = minutes
    , hours = hours
    , days = days
    }


durationToInt : Duration -> Int
durationToInt { milli, seconds, minutes, hours, days } =
    milli + seconds * secondInMs + minutes * minuteInMs + hours * hourInMs + days * dayInMs


dataPointDiff : DataPoint -> DataPoint -> Int
dataPointDiff left right =
    let
        leftInt =
            Time.posixToMillis left.time

        rightInt =
            Time.posixToMillis right.time
    in
    leftInt - rightInt


diffToString : Model -> (Int -> Unit -> a) -> Duration -> List a
diffToString model function duration =
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
    if model.simple then
        highest parts

    else
        allParts


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        dataPoints =
            decodeFlags flags
    in
    ( { dataPoints = dataPoints
      , timeZone = Nothing
      , allVisible = False
      , simple = True
      }
    , Task.perform GotTimeZone Time.here
    )


decodeFlags : Flags -> List DataPoint
decodeFlags flags =
    let
        decodeMillis millis =
            Decode.map DataPoint (Decode.succeed (Time.millisToPosix millis))

        dataPointDecoder =
            Decode.int
                |> Decode.andThen decodeMillis

        decoder =
            Decode.list dataPointDecoder
    in
    case Decode.decodeValue decoder flags of
        Ok dataPoints ->
            dataPoints

        _ ->
            []



-- ---------------------------
-- UPDATE
-- ---------------------------


type Msg
    = Record
    | DataPointRecorded Time.Posix
    | GotTimeZone Time.Zone
    | Reset
    | ToggleSimple Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Record ->
            ( model, Task.perform DataPointRecorded Time.now )

        DataPointRecorded time ->
            let
                newModel =
                    putDataPoint { time = time } model

                dataPoints =
                    newModel.dataPoints
                        |> List.map (.time >> Time.posixToMillis)
            in
            ( newModel, storeDataPoints dataPoints )

        GotTimeZone timeZone ->
            ( putTimeZone timeZone model, Cmd.none )

        Reset ->
            ( resetDataPoints model, storeDataPoints [] )

        ToggleSimple simple ->
            ( toggleSimple simple model, Cmd.none )


toggleSimple : Bool -> Model -> Model
toggleSimple simple model =
    { model | simple = simple }


resetDataPoints : Model -> Model
resetDataPoints model =
    { model | dataPoints = [] }


putDataPoint : DataPoint -> Model -> Model
putDataPoint dataPoint model =
    { model | dataPoints = dataPoint :: model.dataPoints }


putTimeZone : Time.Zone -> Model -> Model
putTimeZone timeZone model =
    { model | timeZone = Just timeZone }



-- ---------------------------
-- VIEW
-- ---------------------------


view : Model -> Html Msg
view model =
    case model.timeZone of
        Just timeZone ->
            div [ class "container px-1" ]
                [ div [ class "text-right" ]
                    [ label [ class "mr-2" ] [ text "Simple", input [ class "ml-1", type_ "checkbox", checked model.simple, onCheck ToggleSimple ] [] ]
                    , button [ class "bg-white text-purple-800 px-2 py-1 rounded-md", onClick Reset ] [ text "reset" ]
                    ]
                , div [ class "flex flex-col items-center" ]
                    [ div [ class "py-8" ] [ button [ onClick Record, class "w-36 h-36 bg-purple-800 rounded-full text-white text-2xl" ] [ text "Tap" ] ]
                    ]
                , viewDataPoints model model.dataPoints
                ]

        _ ->
            div [] [ text "Loading" ]


viewBar : Int -> Int -> Html Msg
viewBar maximum current =
    let
        percent =
            current // maximum
    in
    div [ class "w-full bg-gray-200 rounded-full h-2.5 mb-4 dark:bg-gray-700" ]
        [ div [ class "bg-blue-600 h-2.5 rounded-full dark:bg-blue-500", style "width" (String.fromInt percent ++ "%") ] [] ]


reduceDurations : (Duration -> a -> a) -> a -> List DataPoint -> a
reduceDurations function initial records =
    let
        reduceFunction next acc =
            case next of
                left :: (right :: rest) ->
                    reduceFunction (right :: rest) (function (toDuration (dataPointDiff left right)) acc)

                _ ->
                    acc
    in
    case records of
        [] ->
            initial

        [ _ ] ->
            initial

        _ ->
            reduceFunction records initial


viewDataPoints : Model -> List DataPoint -> Html Msg
viewDataPoints model dataPoints =
    let
        viewDataPointDiff records acc =
            reduceDurations (\duration innerAcc -> viewDataPoint model duration :: innerAcc) acc records

        average records =
            let
                durations =
                    reduceDurations (\duration acc -> durationToInt duration :: acc) [] records
            in
            durations
                |> List.foldl (+) 0
                |> (\sum -> sum // List.length durations)
                |> toDuration
                |> viewDataPoint model

        statBlock title value =
            div [ class "flex flex-1 flex-col text-center" ]
                [ div [ class "text-gray-500" ] [ text title ]
                , div [ class "text-lg" ] value
                ]
    in
    case dataPoints of
        [] ->
            div [] [ text "No data points" ]

        [ _ ] ->
            div [] [ text "One point" ]

        first :: (second :: _) ->
            div []
                [ div [ class "flex space-evenly" ]
                    [ statBlock
                        ("Last (total "
                            ++ String.fromInt (List.length dataPoints)
                            ++ ")"
                        )
                        (viewDataPointDiff [ first, second ] [])
                    , statBlock "10 avg." [ dataPoints |> List.take 10 |> average ]
                    , statBlock "50 avg." [ dataPoints |> List.take 50 |> average ]
                    ]
                , div [ class "text-right" ] (viewDataPointDiff (List.take 100 dataPoints) [] |> List.reverse)
                ]


viewDataPoint : Model -> Duration -> Html Msg
viewDataPoint model duration =
    let
        viewDataPointText value unit =
            span [ class "mr-2" ]
                [ span [ class "text-md mr-0.5" ] [ text (String.fromInt value) ]
                , span [ class "text-sm" ] [ text (unitToAbbreviation unit) ]
                ]
    in
    div [ class "mb-2" ] <| diffToString model viewDataPointText duration



-- ---------------------------
-- MAIN
-- ---------------------------


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view =
            \model ->
                { title = "Poisson"
                , body = [ view model ]
                }
        , subscriptions = \_ -> Sub.none
        }
