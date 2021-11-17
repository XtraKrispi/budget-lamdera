module Views exposing (..)

import Css exposing (..)
import Css.Transitions as T exposing (transition)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css)
import Html.Styled.Events exposing (onClick)
import Theme


type ButtonState
    = Positive
    | Negative
    | Neutral


appButton : ButtonState -> Html msg -> msg -> Html msg
appButton state content click =
    let
        ( fg, bg ) =
            case state of
                Positive ->
                    ( Theme.colors.white, Theme.colors.green )

                Negative ->
                    ( Theme.colors.white, Theme.colors.red )

                Neutral ->
                    ( Theme.colors.white, Theme.colors.gray )

        ( hfg, hbg ) =
            case state of
                Positive ->
                    ( Theme.colors.white, Theme.colors.darkGreen )

                Negative ->
                    ( Theme.colors.white, Theme.colors.darkRed )

                Neutral ->
                    ( Theme.colors.white, Theme.colors.darkGray )
    in
    button
        [ css
            [ border zero
            , padding2 (rem 0.5) (rem 2)
            , borderRadius (rem 0.3)
            , color fg
            , backgroundColor bg
            , cursor pointer
            , hover
                [ color hfg
                , backgroundColor hbg
                ]
            , transition
                [ T.backgroundColor 100
                , T.color 100
                ]
            ]
        , onClick click
        ]
        [ content ]


itemView :
    { leftPane : List (Html msg)
    , rightPane : List (Html msg)
    , actions : List ( ButtonState, msg, Html msg )
    }
    -> Html msg
itemView cfg =
    div
        [ css
            [ backgroundColor (rgb 255 255 255)
            , boxShadow5 (px 0) (px 5) (px 20) (px -5) (rgb 200 200 200)
            , padding (rem 2)
            , marginBottom (rem 1)
            , position relative
            , displayFlex
            , flexDirection column
            , borderRadius (rem 0.5)
            ]
        ]
        [ div
            [ css
                [ displayFlex
                , justifyContent spaceBetween
                ]
            ]
            [ div
                [ class "left-pane"
                , css
                    [ fontSize (rem 1.2)
                    , fontWeight bold
                    ]
                ]
                cfg.leftPane
            , div
                [ class "right-pane"
                , css
                    [ displayFlex
                    , flexDirection column
                    ]
                ]
                cfg.rightPane
            ]
        , div
            [ class "actions"
            , css
                [ displayFlex
                , justifyContent spaceAround
                , marginTop (rem 1)
                ]
            ]
            (List.map (\( s, m, c ) -> div [] [ appButton s c m ]) cfg.actions)
        ]
