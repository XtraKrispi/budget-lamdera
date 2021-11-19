module Views exposing (..)

import Css exposing (..)
import Css.Transitions as T exposing (transition)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr exposing (class, css, placeholder, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import Theme


type ButtonState
    = Positive
    | Negative
    | Neutral


formElementCss : List Style
formElementCss =
    [ padding4 (rem 0.5) zero (rem 0.5) (rem 0.5)
    , flexGrow (int 1)
    , borderRadius (rem 0.5)
    , border3 (px 1) solid Theme.colors.gray
    , fontFamilies [ "Open Sans" ]
    ]


card : List (Attribute msg) -> List (Html msg) -> Html msg
card attrs content =
    div
        (css
            [ backgroundColor (rgb 255 255 255)
            , boxShadow5 (px 0) (px 5) (px 20) (px -5) (rgb 200 200 200)
            , padding (rem 2)
            , marginBottom (rem 1)
            , position relative
            , borderRadius (rem 0.5)
            ]
            :: attrs
        )
        content


appInput : (String -> msg) -> String -> List (Attribute msg) -> Html msg
appInput msg val attrs =
    input
        ([ value val
         , onInput msg
         , css formElementCss
         ]
            ++ attrs
        )
        []


appDropdown :
    (String -> msg)
    -> String
    -> List String
    -> List (Attribute msg)
    -> Html msg
appDropdown onSelected selected options attrs =
    options
        |> List.map
            (\o ->
                option
                    [ value o
                    , Attr.selected (o == selected)
                    ]
                    [ text o ]
            )
        |> select
            ([ onInput onSelected
             , css formElementCss
             ]
                ++ attrs
            )


appButton : msg -> ButtonState -> Html msg -> List (Attribute msg) -> Html msg
appButton click state content attrs =
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
        ([ css
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
         , type_ "button"
         ]
            ++ attrs
        )
        [ content ]


itemView :
    { leftPane : List (Html msg)
    , rightPane : List (Html msg)
    , actions : List ( ButtonState, msg, Html msg )
    }
    -> Html msg
itemView cfg =
    card
        [ css
            [ displayFlex
            , flexDirection column
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
            (List.map (\( s, m, c ) -> div [] [ appButton m s c [] ]) cfg.actions)
        ]
