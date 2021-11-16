module Backend exposing (..)

import Date exposing (Date, Unit(..), add, today)
import Lamdera exposing (ClientId, SessionId, sendToFrontend)
import Task exposing (perform)
import TestData
import Time exposing (Month(..))
import Types exposing (..)


type alias Model =
    BackendModel


app :
    { init : ( Model, Cmd BackendMsg )
    , update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
    , updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
    , subscriptions : Model -> Sub BackendMsg
    }
app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { definitions = []
      , archive = []
      }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        GotDateForAction clientId action item date ->
            let
                cmd =
                    case action of
                        Paid ->
                            ItemPaid item

                        Skipped ->
                            ItemSkipped item
            in
            ( { model
                | archive =
                    model.archive
                        ++ [ { description = item.description
                             , amount = item.amount
                             , paymentType = item.paymentType
                             , date = item.date
                             , action = action
                             , actionedDate = date
                             }
                           ]
              }
            , sendToFrontend clientId cmd
            )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend _ clientId msg model =
    case msg of
        GetItems endDate ->
            let
                itemsFromDefs =
                    (model.definitions ++ [ TestData.testDefinition, TestData.testCreditDefinition ])
                        |> List.map (extractItems endDate)
                        |> List.concat
                        |> List.filter
                            (\item ->
                                not <|
                                    List.any
                                        (\a ->
                                            a.description == item.description && a.date == item.date
                                        )
                                        model.archive
                            )
                        |> Items
            in
            ( model, sendToFrontend clientId itemsFromDefs )

        GetArchive ->
            ( model, sendToFrontend clientId (ArchiveItems model.archive) )

        PayItem item ->
            ( model, perform (GotDateForAction clientId Paid item) today )

        SkipItem item ->
            ( model, perform (GotDateForAction clientId Skipped item) today )

        Unarchive item ->
            let
                newArchive =
                    List.filter (\a -> a /= item) model.archive
            in
            ( { model | archive = newArchive }, sendToFrontend clientId (ArchiveItems newArchive) )


extractItems : Date -> Definition -> List Item
extractItems endDate def =
    let
        newItem date =
            { description = def.description
            , amount = def.amount
            , date = date
            , paymentType = def.paymentType
            }

        go : (Date -> Date) -> Date -> List Item -> List Item
        go fn d items =
            if Date.compare d endDate == GT then
                items

            else
                go fn (fn d) (items ++ [ newItem d ])
    in
    case def.frequency of
        OneTime ->
            [ { description = def.description
              , amount = def.amount
              , date = def.startDate
              , paymentType = def.paymentType
              }
            ]

        Weekly ->
            go (add Weeks 1) def.startDate []

        BiWeekly ->
            go (add Weeks 2) def.startDate []

        Monthly ->
            go (add Months 1) def.startDate []
