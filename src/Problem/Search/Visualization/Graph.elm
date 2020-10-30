module Problem.Search.Visualization.Graph exposing (Model, init, update, view)

import Color exposing (black)
import Dict
import Force
import Html exposing (Html)
import List.Extra as List
import Maybe.Extra as Maybe
import Problem exposing (Problem)
import Problem.Search as Search
import Set exposing (Set)
import Svg.PathD exposing (..)
import TypedSvg exposing (..)
import TypedSvg.Attributes as Attributes exposing (..)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (..)


type alias Edge =
    { source : String
    , target : String
    , distance : Float
    , strength : Maybe Float
    }


type alias Model =
    { existingStates : Set String
    , nodes : List (Force.Entity String {})
    , edges : List Edge
    }


init : Problem a -> Model
init problem =
    let
        initialState =
            problem.stateToString problem.initialState
    in
    { existingStates = Set.fromList [ initialState ]
    , nodes =
        [ { id = initialState
          , x = 0
          , y = 0
          , vx = 0
          , vy = 0
          }
        ]
    , edges = []
    }


update : Model -> Search.Model a -> Model
update model (Search.Model searchModel) =
    let
        newNodes : List ( String, Search.Node a )
        newNodes =
            searchModel.explored
                |> Dict.filter (\k _ -> not <| Set.member k model.existingStates)
                |> Dict.toList

        nodes : List (Force.Entity String {})
        nodes =
            (newNodes
                |> List.indexedMap
                    (\i ( state, Search.Node { parent } ) ->
                        let
                            parentNode =
                                parent
                                    |> Maybe.map
                                        (\parent_ ->
                                            model.nodes
                                                |> List.find
                                                    (\{ id } ->
                                                        id == searchModel.problem.stateToString parent_
                                                    )
                                        )
                                    |> Maybe.join
                        in
                        { id = state
                        , x =
                            (parentNode
                                |> Maybe.map .x
                                |> Maybe.withDefault 0
                            )
                                + 0.01
                                * toFloat i
                        , y =
                            (parentNode
                                |> Maybe.map .y
                                |> Maybe.withDefault 0
                            )
                                + 0.01
                        , vx = 0
                        , vy = 0
                        }
                    )
            )
                ++ model.nodes

        edges =
            searchModel.explored
                |> Dict.toList
                |> List.map
                    (\( state, Search.Node { children, pathCost } ) ->
                        Maybe.map
                            (List.map
                                (\child ->
                                    let
                                        childPathCost =
                                            child.pathCost

                                        childState =
                                            child.state
                                    in
                                    { source = state
                                    , target = searchModel.problem.stateToString childState
                                    , distance = childPathCost - pathCost
                                    , strength = Nothing
                                    }
                                )
                            )
                            children
                    )
                |> Maybe.values
                |> List.concat

        graphState =
            Force.simulation
                [ Force.center 0 0
                , Force.customLinks 1 model.edges
                , Force.manyBodyStrength -0.002 (List.map .id nodes)
                ]

        -- |> iterations 300
        simulatedNodes : List (Force.Entity String {})
        simulatedNodes =
            Force.computeSimulation
                graphState
                nodes
                |> List.map (\({ x } as a) -> { a | x = x })
    in
    { model
        | nodes = simulatedNodes
        , edges = edges
        , existingStates =
            Set.union
                (newNodes
                    |> List.map Tuple.first
                    |> Set.fromList
                )
                model.existingStates
    }


view : Model -> Html msg
view model =
    let
        minX =
            List.minimumBy .x model.nodes |> Maybe.map .x |> Maybe.withDefault 0

        minY =
            List.minimumBy .y model.nodes |> Maybe.map .y |> Maybe.withDefault 0

        maxX =
            List.maximumBy .x model.nodes |> Maybe.map .x |> Maybe.withDefault 0

        maxY =
            List.maximumBy .y model.nodes |> Maybe.map .y |> Maybe.withDefault 0

        c =
            1 / Basics.max (maxX - minX) (maxY - minY)

        d =
            Basics.min minX minY

        pos a =
            c * (a - d)
    in
    svg
        [ width (px 900)
        , height (px 900)
        , viewBox 0 0 1 1
        , Attributes.style "border: 1px dotted black"
        ]
        ((model.nodes
            |> List.map
                (\entity ->
                    g
                        [ transform [ Translate (pos entity.x) (pos entity.y) ] ]
                        [ circle
                            [ r (px 0.002)
                            ]
                            []

                        {--
                        , Svg.text_
                            [ Svg.Attributes.stroke "black"
                            , Svg.Attributes.strokeWidth "0.1"
                            , Svg.Attributes.transform "translate(0.005 0) scale(0.001) "
                            , Svg.Attributes.fontSize "10"
                            ]
                            [ Svg.text (Debug.toString entity.id) ]

                        --}
                        ]
                )
         )
            ++ (model.edges
                    |> List.map
                        (\{ source, target } ->
                            Maybe.map2 (path_ pos)
                                (model.nodes |> List.find (\{ id } -> id == source))
                                (model.nodes |> List.find (\{ id } -> id == target))
                        )
                    |> Maybe.values
               )
        )


path_ pos e1 e2 =
    TypedSvg.path
        [ d <|
            pathD
                [ M ( pos e1.x, pos e1.y )
                , L ( pos e2.x, pos e2.y )
                ]
        , stroke (Paint black)
        , strokeWidth (px 0.0002)
        ]
        []
