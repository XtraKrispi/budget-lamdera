module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Css exposing (..)
import Date exposing (Unit(..), fromIsoString, toIsoString, today)
import FormatNumber
import FormatNumber.Locales exposing (usLocale)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr exposing (css, href, rel, type_, value)
import Html.Styled.Events exposing (onInput)
import Lamdera exposing (Key, sendToBackend)
import RouteParser exposing (routeParser)
import Task
import Theme
import Types exposing (..)
import Url
import Url.Parser exposing (parse)
import Views exposing (ButtonState(..), itemView)


type alias Model =
    FrontendModel


getUrl : Route -> String
getUrl route =
    case route of
        Home Nothing ->
            "/"

        Home (Just endDate) ->
            "/?endDate=" ++ toIsoString endDate

        Admin ->
            "/admin"

        Archive ->
            "/archive"

        NotFound ->
            "/not-found"


app :
    { init : Lamdera.Url -> Key -> ( Model, Cmd FrontendMsg )
    , view : Model -> Browser.Document FrontendMsg
    , update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
    , updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
    , subscriptions : Model -> Sub FrontendMsg
    , onUrlRequest : UrlRequest -> FrontendMsg
    , onUrlChange : Url.Url -> FrontendMsg
    }
app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \_ -> Sub.none
        , view = view
        }


getRoute : Url.Url -> Route
getRoute url =
    Maybe.withDefault NotFound <| parse routeParser url


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( Initializing
        { key = key
        , initialRoute = getRoute url
        }
    , Task.perform GotToday today
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl (key model) (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            case model of
                Initializing _ ->
                    ( model, Cmd.none )

                Ready mdl ->
                    let
                        route =
                            getRoute url

                        ( endDate, routeCmd ) =
                            case route of
                                Home Nothing ->
                                    ( mdl.endDate, sendToBackend (GetItems mdl.endDate) )

                                Home (Just ed) ->
                                    ( ed, sendToBackend (GetItems ed) )

                                Admin ->
                                    ( mdl.endDate, Cmd.none )

                                Archive ->
                                    ( mdl.endDate, sendToBackend GetArchive )

                                NotFound ->
                                    ( mdl.endDate, Cmd.none )
                    in
                    ( Ready
                        { mdl
                            | route = route
                            , endDate = endDate
                        }
                    , routeCmd
                    )

        GotToday date ->
            case model of
                Initializing { key, initialRoute } ->
                    let
                        endDate =
                            case initialRoute of
                                Home (Just ed) ->
                                    ed

                                _ ->
                                    Date.add Weeks 2 date
                    in
                    ( Ready
                        { key = key
                        , items = []
                        , endDate = endDate
                        , route = initialRoute
                        , definitions = []
                        , archive = []
                        }
                    , sendToBackend (GetItems endDate)
                    )

                _ ->
                    ( model, Cmd.none )

        ChangeEndDate dateStr ->
            case fromIsoString dateStr of
                Ok date ->
                    ( model, Nav.pushUrl (key model) (getUrl (Home (Just date))) )

                _ ->
                    ( model, Cmd.none )

        Pay item ->
            ( model, sendToBackend (PayItem item) )

        Skip item ->
            ( model, sendToBackend (SkipItem item) )

        Undo item ->
            ( model, sendToBackend (Unarchive item) )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case ( msg, model ) of
        ( Items items, Ready mdl ) ->
            ( Ready { mdl | items = items |> List.sortWith (\a b -> Date.compare a.date b.date) }, Cmd.none )

        ( ArchiveItems items, Ready mdl ) ->
            ( Ready { mdl | archive = items |> List.sortWith (\a b -> Date.compare b.date a.date) }, Cmd.none )

        ( ItemPaid _, Ready mdl ) ->
            ( model, sendToBackend (GetItems mdl.endDate) )

        ( ItemSkipped _, Ready mdl ) ->
            ( model, sendToBackend (GetItems mdl.endDate) )

        _ ->
            ( model, Cmd.none )


navbar : ReadyModel -> Html FrontendMsg
navbar model =
    let
        home =
            ( Home Nothing, "Home" )

        routes =
            [ home, ( Admin, "Admin" ), ( Archive, "Archive" ) ]

        isMatch : Route -> Route -> Bool
        isMatch modelRoute route =
            case ( modelRoute, route ) of
                ( Home _, Home _ ) ->
                    True

                ( Admin, Admin ) ->
                    True

                ( Archive, Archive ) ->
                    True

                _ ->
                    False

        menuItem ( route, txt ) =
            div
                [ css
                    ([ marginRight (rem 1)
                     , displayFlex
                     , alignItems center
                     ]
                        ++ (if isMatch model.route route then
                                [ borderBottom3 (px 2) solid (rgb 0 0 255) ]

                            else
                                []
                           )
                    )
                ]
                [ a
                    [ href <| getUrl route
                    , css
                        [ height (pct 100)
                        , display inlineBlock
                        , verticalAlign middle
                        , textDecoration none
                        , color (rgb 0 0 0)
                        ]
                    ]
                    [ text txt ]
                ]
    in
    div
        [ css
            [ width (pct 100)
            , displayFlex
            , justifyContent flexStart
            , lineHeight (rem 3)
            , boxShadow5 (px 0) (px 5) (px 20) (px -9) (rgb 200 200 200)
            , backgroundColor (rgb 255 255 255)
            ]
        ]
        (div
            [ css
                [ marginRight (rem 1)
                , paddingLeft (rem 1)
                , fontSize (rem 1.5)
                ]
            ]
            [ a
                [ home |> Tuple.first |> getUrl |> href
                , css
                    [ textDecoration none
                    , color (rgb 0 0 0)
                    ]
                ]
                [ text "Budget" ]
            ]
            :: List.map menuItem routes
        )


homeView : ReadyModel -> Html FrontendMsg
homeView model =
    let
        leftPane =
            div [ css [ flexGrow (int 1) ] ]
                [ div
                    [ css
                        [ marginBottom (rem 1)
                        , displayFlex
                        , alignItems center
                        , justifyContent flexStart
                        ]
                    ]
                    [ span
                        [ css
                            [ marginRight (rem 0.5)
                            ]
                        ]
                        [ text "End Date: "
                        ]
                    , input
                        [ css
                            [ padding4 (rem 0.5) zero (rem 0.5) (rem 0.5)
                            , flexGrow (int 2)
                            , borderRadius (rem 0.5)
                            , border3 (px 1) solid Theme.colors.gray
                            , fontFamilies [ "Open Sans" ]
                            ]
                        , type_ "date"
                        , value <| toIsoString model.endDate
                        , onInput ChangeEndDate
                        ]
                        []
                    ]
                , div []
                    (List.map
                        (\item ->
                            itemView
                                { leftPane = [ text item.description ]
                                , rightPane =
                                    [ div [] [ currencyView item.amount ]
                                    , div [] [ text (toIsoString item.date) ]
                                    ]
                                , actions =
                                    [ ( Neutral, Pay item, text "Pay" )
                                    , ( Neutral, Skip item, text "Skip" )
                                    ]
                                }
                        )
                        model.items
                    )
                ]

        rightPane =
            div
                [ css
                    [ flexGrow (int 3)
                    , marginLeft (rem 1)
                    ]
                ]
                [ div
                    [ css
                        [ fontSize (rem 1.5)
                        , fontWeight bold
                        ]
                    ]
                    [ text "Scratch" ]
                ]
    in
    div
        [ css
            [ padding (rem 2)
            , paddingTop (rem 1)
            , backgroundColor (rgb 255 255 255)
            , marginTop (rem 0.5)
            , displayFlex
            , justifyContent spaceBetween
            , alignItems flexStart
            ]
        ]
        [ leftPane
        , rightPane
        ]


archiveView : ReadyModel -> Html FrontendMsg
archiveView model =
    div
        [ css
            [ padding (rem 2)
            , paddingTop (rem 1)
            , backgroundColor (rgb 255 255 255)
            , marginTop (rem 0.5)
            , displayFlex
            , justifyContent spaceBetween
            , alignItems flexStart
            ]
        ]
        [ div [ css [ flexGrow (int 1) ] ]
            (List.map
                (\item ->
                    itemView
                        { leftPane = [ text item.description ]
                        , rightPane =
                            [ div [] [ currencyView item.amount ]
                            , div [] [ text (toIsoString item.date) ]
                            ]
                        , actions = [ ( Neutral, Undo item, text "Undo" ) ]
                        }
                )
                model.archive
            )
        , div [ css [ flexGrow (int 3) ] ] []
        ]


readyView : ReadyModel -> Html FrontendMsg
readyView model =
    div
        [ css
            [ displayFlex
            , flexDirection column
            , padding (rem 0.5)
            , boxSizing borderBox
            ]
        ]
        [ navbar model
        , case model.route of
            Home _ ->
                homeView model

            Archive ->
                archiveView model

            _ ->
                div [] []
        ]


view : Model -> Browser.Document FrontendMsg
view model =
    let
        ( title, page ) =
            case model of
                Initializing _ ->
                    ( "Loading...", div [] [] )

                Ready readyModel ->
                    ( case readyModel.route of
                        Home _ ->
                            "Home"

                        Admin ->
                            "Admin"

                        Archive ->
                            "Archive"

                        NotFound ->
                            "Ruh Roh"
                    , div [] [ readyView readyModel ]
                    )
    in
    { title = title
    , body =
        [ node "link" [ rel "preconnect", href "https://fonts.googleapis.com" ] []
        , node "link" [ rel "preconnect", href "https://fonts.gstatic.com", Attr.attribute "crossorigin" "" ] []
        , node "link" [ rel "stylesheet", href "https://fonts.googleapis.com/css2?family=Open+Sans&display=swap" ] []
        , node "link" [ rel "apple-touch-icon", Attr.attribute "sizes" "180x180", href "/apple-touch-icon.png" ] []
        , node "link" [ rel "icon", type_ "image/png", Attr.attribute "sizes" "32x32", href "/favicon-32x32.png" ] []
        , node "link" [ rel "icon", type_ "image/png", Attr.attribute "sizes" "16x16", href "/favicon-16x16.png" ] []
        , node "link" [ rel "manifest", href "/site.webmanifest" ] []
        , node "link" [ rel "mask-icon", href "/safari-pinned-tab.svg", Attr.attribute "color" "#5bbad5" ] []
        , node "meta" [ Attr.name "msapplication-TileColor", Attr.attribute "content" "#da532c" ] []
        , node "meta" [ Attr.name "theme-color", Attr.attribute "content" "#ffffff" ] []
        , div
            [ css
                [ fontFamilies [ "Open Sans", "sans-serif" ]
                , height (vh 100)
                , backgroundColor (rgb 255 255 255)
                ]
            ]
            [ page ]
        ]
            |> List.map toUnstyled
    }


currencyView : Amount -> Html FrontendMsg
currencyView amt =
    let
        ( isDebit, f ) =
            case amt of
                Debit a ->
                    ( True, a )

                Credit a ->
                    ( False, a )

        formatted =
            "$"
                ++ FormatNumber.format
                    { usLocale
                        | decimals = FormatNumber.Locales.Exact 2
                    }
                    f
    in
    span
        [ css
            [ color
                (if isDebit then
                    Theme.colors.red

                 else
                    Theme.colors.red
                )
            ]
        ]
        [ text formatted ]
