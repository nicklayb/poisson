module Unit exposing (Unit(..), dayInMs, hourInMs, minuteInMs, secondInMs, toAbbreviation)


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


toAbbreviation : Unit -> String
toAbbreviation unit =
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
