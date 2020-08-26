module Search.Visualization.Graph exposing (Model, init, update, view)

import Dict
import Force
import Html exposing (Html)
import List.Extra as List
import Maybe.Extra as Maybe
import Search exposing (Problem)
import Search.Problem
import Set exposing (Set)
import Svg
import Svg.Attributes


type alias Node a =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , id : a
    }


type alias Edge a =
    { source : a
    , target : a
    , distance : Float
    , strength : Maybe Float
    }


type alias Model a =
    { existingStates : Set a
    , nodes : List (Force.Entity a {})
    , edges : List (Edge a)
    }


init : Problem a comparable -> Model comparable
init problem =
    let
        initialState =
            problem.stateToComparable problem.initialState
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


update : Model comparable -> Search.Model a comparable -> Model comparable
update model searchModel =
    let
        newNodes : List ( comparable, Search.Problem.Node a )
        newNodes =
            searchModel.explored
                |> Dict.filter (\k _ -> not <| Set.member k model.existingStates)
                |> Dict.toList

        nodes : List (Force.Entity comparable {})
        nodes =
            (newNodes
                |> List.indexedMap
                    (\i ( state, { parent } ) ->
                        let
                            parentNode =
                                parent
                                    |> Maybe.map
                                        (\parent_ ->
                                            model.nodes
                                                |> List.find
                                                    (\{ id } ->
                                                        id == searchModel.problem.stateToComparable parent_
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
                    (\( state, { children, pathCost } ) ->
                        Maybe.map
                            (List.map
                                (\( childPathCost, childState ) ->
                                    { source = state
                                    , target = searchModel.problem.stateToComparable childState
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
        simulatedNodes : List (Force.Entity comparable {})
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


view : Model a -> Html msg
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

        pos ( x, y ) =
            ( c * (x - d)
            , c * (y - d)
            )

        posString =
            pos
                >> (\( x, y ) ->
                        String.fromFloat x
                            ++ " "
                            ++ String.fromFloat y
                   )
    in
    Svg.svg
        [ Svg.Attributes.width "900"
        , Svg.Attributes.height "900"
        , Svg.Attributes.viewBox "0 0 1 1"
        , Svg.Attributes.style "border: 1px dotted black"
        ]
        ((model.nodes
            |> List.map
                (\entity ->
                    Svg.g
                        [ Svg.Attributes.transform
                            ("translate ("
                                ++ posString ( entity.x, entity.y )
                                ++ ")"
                            )
                        ]
                        [ Svg.circle
                            [ Svg.Attributes.r "0.002"
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
                            Maybe.map2 (path posString)
                                (model.nodes |> List.find (\{ id } -> id == source))
                                (model.nodes |> List.find (\{ id } -> id == target))
                        )
                    |> Maybe.values
               )
        )


path posString e1 e2 =
    Svg.path
        [ Svg.Attributes.d
            ("M "
                ++ posString ( e1.x, e1.y )
                ++ "L "
                ++ posString ( e2.x, e2.y )
            )
        , Svg.Attributes.stroke "black"
        , Svg.Attributes.strokeWidth "0.0002"
        ]
        []
