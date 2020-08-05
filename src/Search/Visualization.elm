module Search.Visualization exposing (..)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Search exposing (Model, Problem, Solution(..))


visualization :
    Model comparable
    -> (Maybe ( Float, comparable ) -> msg)
    -> (comparable -> Html msg)
    -> Maybe ( Float, comparable )
    -> ( Float, Float )
    -> Html msg
visualization model msg visualizeState shown ( x, y ) =
    layout
        [ width fill, height fill, Events.onMouseLeave (msg Nothing), pointer ]
        (column
            [ width fill, height fill ]
            [ tooltip model.problem visualizeState x y shown
            , boxify model.explored msg 0 model.maxPathCost [ ( 0, model.problem.initialState ) ]
            ]
        )


boxify :
    Dict (List ( Float, comparable )) (List ( Float, comparable ))
    -> (Maybe ( Float, comparable ) -> msg)
    -> Int
    -> Float
    -> List ( Float, comparable )
    -> Element msg
boxify explored msg level maxPathCost path =
    case path of
        (( pathCost, _ ) as h) :: _ ->
            let
                children =
                    Maybe.withDefault [] (Dict.get path explored)
            in
            columnOrRow
                level
                (properties pathCost maxPathCost)
                (el
                    [ width (fillPortion 20)
                    , height (fillPortion 20)
                    , Events.onMouseEnter (msg (Just h))
                    ]
                    (el [] none)
                    :: (if List.length children > 0 then
                            [ rowOrColumn
                                level
                                [ width (fillPortion 80)
                                , height (fillPortion 80)
                                , spacing 2
                                , padding 2
                                ]
                                (children
                                    |> List.map
                                        (\child ->
                                            boxify
                                                explored
                                                msg
                                                (level + 1)
                                                maxPathCost
                                                (child :: path)
                                        )
                                )
                            ]

                        else
                            []
                       )
                )

        _ ->
            none


columnOrRow : Int -> (List (Attribute msg) -> List (Element msg) -> Element msg)
columnOrRow level =
    if modBy 2 level == 0 then
        column

    else
        row


rowOrColumn : Int -> (List (Attribute msg) -> List (Element msg) -> Element msg)
rowOrColumn level =
    if modBy 2 level == 0 then
        row

    else
        column


properties : Float -> Float -> List (Attribute msg)
properties pathCost maxPathCost =
    let
        c =
            color pathCost maxPathCost
        b = c - 0.2

    in
    [ width fill
    , height fill
    , Border.rounded 20
    , Background.color (rgb c c c)
    , Border.color (rgb b b b)
    , Border.width 1
    ]


color : Float -> Float -> Float
color pathCost maxPathCost =
    1 - (pathCost / max maxPathCost 5 * 0.8)


tooltip :
    Problem state
    -> (state -> Html msg)
    -> Float
    -> Float
    -> Maybe ( Float, state )
    -> Element msg
tooltip problem visualizeState x y shown =
    case shown of
        Just ( pathCost, state ) ->
            el
                [ width shrink
                , height (maximum 0 shrink)
                ]
                (column
                    (List.map htmlAttribute
                        [ style "left" (String.fromFloat (x + 10) ++ "px")
                        , style "top" (String.fromFloat (y + 10) ++ "px")
                        , style "z-index" "1"
                        ]
                        ++ [ Background.color (rgb 1 1 1)
                           , padding 20
                           , spacing 20
                           , Border.rounded 20
                           , Border.glow (rgb 0.5 0.5 0.5) 2
                           ]
                    )
                    [ el [ centerX ] (html (visualizeState state))
                    , column [ spacing 2, Font.size 12 ]
                        [ row [ width fill, spacing 20 ]
                            [ el [ alignLeft ] (text "Path cost")
                            , el [ alignRight ] (text <| String.fromFloat pathCost)
                            ]
                        , row [ width fill, spacing 20 ]
                            [ el [ alignLeft ] (text "Heuristic")
                            , el [ alignRight ] (text <| String.fromFloat (problem.heuristic state))
                            ]
                        , row [ width fill, spacing 20 ]
                            [ el [ alignLeft ] (text "Sum")
                            , el [ alignRight ] (text <| String.fromFloat (pathCost + problem.heuristic state))
                            ]
                        ]
                    ]
                )

        Nothing ->
            none
