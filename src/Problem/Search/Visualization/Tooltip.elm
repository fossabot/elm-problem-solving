module Problem.Search.Visualization.Tooltip exposing (..)

import Browser.Events
import Color exposing (red)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes exposing (style)
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
    -> Element msg
info problem visualizeState { pathCost, state } =
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
    row [ width Element.fill, spacing 20 ]
        [ el [ alignLeft ] (text description)
        , el [ alignRight ] (text <| String.fromFloat number)
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
    layout [ width shrink, height shrink ]
        (case node of
            Just n ->
                Element.el
                    (List.map htmlAttribute
                        [ style "left" (String.fromFloat (position.x + 10) ++ "px")
                        , style "top" (String.fromFloat (position.y + 10) ++ "px")
                        , style "z-index" "1"
                        ]
                        ++ [-- width shrink
                            --, height (maximum 0 shrink)
                           ]
                    )
                    (Element.el
                        [ Background.color (rgb 1 1 1)
                        , Border.rounded 5
                        , Border.glow (rgb 0.4 0.4 0.4) 2
                        , padding 10
                        ]
                        (info problem vis n)
                    )

            Nothing ->
                none
        )


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
