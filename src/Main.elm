port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import DataPoint exposing (DataPoint)
import DateTime
import Duration exposing (Duration)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick)
import Http exposing (Error(..))
import Json.Decode as Decode exposing (Value)
import Math
import Task
import Time
import Unit



-- ---------------------------
-- PORTS
-- ---------------------------


port storeDataPoints : List Int -> Cmd msg



-- ---------------------------
-- MODEL
-- ---------------------------


type alias Flags =
    Value


type alias Model =
    { dataPoints : List DataPoint
    , timeZone : Maybe Time.Zone
    , allVisible : Bool
    , simple : Bool
    }


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
    case Decode.decodeValue (Decode.list DataPoint.decoder) flags of
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
                [ div [ class "flex space-between" ]
                    [ div [ class "flex flex-1" ] [ label [ class "mr-2" ] [ text "Simple", input [ class "ml-1", type_ "checkbox", checked model.simple, onCheck ToggleSimple ] [] ] ]
                    , div [ class "flex flex-1 justify-end" ] [ button [ class "bg-white text-purple-800 px-2 py-1 rounded-md", onClick Reset ] [ text "reset" ] ]
                    ]
                , div [ class "flex flex-col items-center" ]
                    [ div [ class "py-8" ] [ button [ onClick Record, class "w-36 h-36 bg-purple-800 rounded-full text-white text-2xl" ] [ text "Tap" ] ]
                    ]
                , viewDataPoints model model.dataPoints
                ]

        _ ->
            div [] [ text "Loading" ]


reduceDurations : (Duration -> a -> a) -> a -> List DataPoint -> a
reduceDurations function initial records =
    let
        reduceFunction next acc =
            case next of
                left :: (right :: rest) ->
                    reduceFunction (right :: rest) (function (Duration.fromDataPoints left right) acc)

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
            reduceDurations (\duration innerAcc -> viewDurationRow duration :: innerAcc) acc records

        average records =
            records
                |> reduceDurations (\duration acc -> Duration.toInt duration :: acc) []
                |> Math.average
                |> Duration.fromInt
                |> viewDataPoint model

        statBlock title value =
            div [ class "flex flex-1 flex-col text-center py-3 bg-white m-2 rounded-md shadow-md" ]
                [ div [ class "text-gray-500" ] [ text title ]
                , div [ class "text-lg" ] value
                ]

        viewDurationRow duration =
            div [ class "py-1 pl-1 mb-1 shadow-sm rounded-md flex space-between odd:bg-white even:bg-gray-100 border border-gray-400" ]
                [ div [ class "flex flex-1 text-gray-500 italic" ] [ viewDataPointDate model duration ]
                , div [ class "flex flex-1 justify-end" ] [ viewDataPoint model duration ]
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
                        (reduceDurations (\duration innerAcc -> viewDataPoint model duration :: innerAcc) [] [ first, second ])
                    , statBlock "10 avg." [ dataPoints |> List.take 10 |> average ]
                    , statBlock "50 avg." [ dataPoints |> List.take 50 |> average ]
                    ]
                , div [ class "text-right" ] (viewDataPointDiff (List.take 100 dataPoints) [] |> List.reverse)
                ]


viewDataPointDate : Model -> Duration -> Html Msg
viewDataPointDate model duration =
    let
        timeZone =
            Maybe.withDefault Time.utc model.timeZone

        formatDateTime dateTime =
            Math.ordinalize dateTime.day ++ " " ++ DateTime.monthToString dateTime.month ++ " " ++ DateTime.clockToString dateTime

        formattedDateTime =
            duration.end
                |> Maybe.map (DateTime.fromPosix timeZone >> formatDateTime)
                |> Maybe.withDefault ""
    in
    div [] [ text formattedDateTime ]


viewDataPoint : Model -> Duration -> Html Msg
viewDataPoint model duration =
    let
        viewDataPointText value unit =
            span [ class "mr-2" ]
                [ span [ class "text-md mr-0.5" ] [ text (String.fromInt value) ]
                , span [ class "text-sm text-gray-600" ] [ text (Unit.toAbbreviation unit) ]
                ]

        viewDiff =
            Duration.diffToString { simple = model.simple } viewDataPointText duration
    in
    div [] viewDiff



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
