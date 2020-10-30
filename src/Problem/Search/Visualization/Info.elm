module Problem.Search.Visualization.Info exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes exposing (style)
import Problem exposing (Problem)
import Problem.Search as Search exposing (Result(..))



-- INFO BOX ABOUT THE SEARCH STATE


info :
    Problem a
    -> (a -> Html msg)
    -> ( Float, a )
    -> Element msg
info problem visualizeState ( pathCost, state ) =
    column
        [ spacing 20
        , centerX
        , padding 10
        ]
        [ el
            [ centerX ]
            (Element.html (visualizeState state))
        , column [ spacing 2, Font.size 12 ]
            [ infoRow "Path cost" pathCost
            , infoRow "Heuristic" (problem.heuristic state)
            , infoRow "Sum" (pathCost + problem.heuristic state)
            ]
        ]


infoRow : String -> Float -> Element msg
infoRow description number =
    row [ width fill, spacing 20 ]
        [ el [ alignLeft ] (text description)
        , el [ alignRight ] (text <| String.fromFloat number)
        ]



-- TOOLTIP


type alias Tooltip a =
    { node : Maybe ( Float, a )
    , position :
        { x : Float
        , y : Float
        }
    }


tooltip : Problem a -> (a -> Html msg) -> Tooltip a -> Element msg
tooltip problem visualizeState { node, position } =
    case node of
        Just n ->
            Element.el
                (List.map htmlAttribute
                    [ style "left" (String.fromFloat (position.x + 10) ++ "px")
                    , style "top" (String.fromFloat (position.y + 10) ++ "px")
                    , style "z-index" "1"
                    ]
                    ++ [ width shrink
                       , height (maximum 0 shrink)
                       ]
                )
                (Element.el
                    [ Background.color (rgb 1 1 1)
                    , Border.rounded 5
                    , Border.glow (rgb 0.4 0.4 0.4) 2
                    , padding 10
                    ]
                    (info problem visualizeState n)
                )

        Nothing ->
            none

