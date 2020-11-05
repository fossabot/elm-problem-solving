module Problem.Search.Visualization.Tooltip exposing (..)

import Browser.Events
import Color exposing (red)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onMouseOut, onMouseOver)
import Json.Decode
import Problem exposing (Problem)
import Problem.Search as Search
import TypedSvg.Attributes exposing (fill)
import TypedSvg.Core
import TypedSvg.Types exposing (Paint(..))



-- INFO BOX ABOUT THE SEARCH STATE


info :
    Problem a
    -> (a -> Html msg)
    -> Search.Node a
    -> Html msg
info problem visualizeState { pathCost, state } =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "padding" "10px"
        , style "font-family" "sans"
        ]
        [ div
            [ style "display" "flex"
            , style "flex-direction" "row"
            , style "justify-content" "center"
            , style "margin-top" "10px"
            , style "margin-bottom" "20px"
            ]
            [ visualizeState state ]
        , infoRow "Path cost" pathCost
        , infoRow "Heuristic" (problem.heuristic state)
        , infoRow "Sum" (pathCost + problem.heuristic state)
        ]


infoRow : String -> Float -> Html msg
infoRow description number =
    div
        [ style "display" "flex"
        , style "flex-direction" "row"
        , style "justify-content" "space-between"
        , style "margin-bottom" "2px"
        , style "font-size" "12pt"
        ]
        [ div [ style "margin-right" "10px" ] [ Html.text description ]
        , div [ style "margin-left" "10px" ] [ Html.text (String.fromFloat number) ]
        ]



-- TOOLTIP


type alias Model msg a =
    { node : Maybe (Search.Node a)
    , position :
        { x : Float
        , y : Float
        }
    , msg : Maybe (Search.Node a) -> msg
    , vis : a -> Html msg
    , problem : Problem a
    }


init : Problem a -> (Maybe (Search.Node a) -> msg) -> Maybe (a -> Html msg) -> Model msg a
init problem msg vis =
    { node = Nothing
    , position = { x = 0, y = 0 }
    , msg = msg
    , vis = Maybe.withDefault (Html.text << problem.stateToString) vis
    , problem = problem
    }


view : Model msg a -> Html msg
view { problem, node, position, vis } =
    case node of
        Just n ->
            div
                [ style "position" "absolute"
                , style "left" (String.fromFloat (position.x + 10) ++ "px")
                , style "top" (String.fromFloat (position.y + 10) ++ "px")
                , style "z-index" "1"
                ]
                [ div
                    [ style "background-color" "white"
                    , style "border-radius" "5px"
                    , style "box-shadow" "0 0 10px 2px rgba(0,0,0,0.8)"

                    --, Border.rounded 5
                    --, Border.glow (rgb 0.4 0.4 0.4) 2
                    --, padding 10
                    ]
                    [ info problem vis n ]
                ]

        Nothing ->
            Html.text ""


sub : ({ x : Float, y : Float } -> msg) -> Sub msg
sub msg =
    Browser.Events.onMouseMove (Json.Decode.map msg decodeMove)


decodeMove : Json.Decode.Decoder { x : Float, y : Float }
decodeMove =
    Json.Decode.map2 (\a b -> { x = a, y = b })
        (Json.Decode.field "pageX" Json.Decode.float)
        (Json.Decode.field "pageY" Json.Decode.float)



-- INTERNAL


properties : Maybe (Model msg a) -> List (Search.Node a) -> List (TypedSvg.Core.Attribute msg)
properties tooltip nodes =
    case ( tooltip, nodes ) of
        ( Just t, h :: _ ) ->
            [ onMouseOver (t.msg (Just h))
            , onMouseOut (t.msg Nothing)
            ]
                ++ (case t.node of
                        Just n ->
                            if List.any (\node -> node.state == n.state) nodes then
                                [ TypedSvg.Attributes.fill (Paint red) ]

                            else
                                []

                        _ ->
                            []
                   )

        _ ->
            []
