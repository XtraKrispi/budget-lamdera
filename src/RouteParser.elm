module RouteParser exposing (..)

import Date exposing (Date, fromIsoString)
import Types exposing (Route(..))
import Url.Parser exposing (..)
import Url.Parser.Query as Query


endDateParser : Query.Parser (Maybe Date)
endDateParser =
    Query.string "endDate"
        |> Query.map
            (\s_ ->
                s_ |> Maybe.andThen (Result.toMaybe << fromIsoString)
            )


routeParser : Parser (Route -> Route) Route
routeParser =
    oneOf
        [ map Home (top <?> endDateParser)
        , map Admin (s "admin")
        , map Archive (s "archive")
        ]
