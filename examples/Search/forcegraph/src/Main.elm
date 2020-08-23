module Main exposing (..)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Force exposing (..)
import Html exposing (Html, p, text)
import List.Extra as List
import Maybe.Extra as Maybe
import Process
import Search
import Search.Problem exposing (Node)
import Search.Problem.Graph exposing (routeFinding)
import Search.Problem.NPuzzle as NPuzzle exposing (complexEightPuzzle, mediumEightPuzzle, simpleEightPuzzle, visualize)
import Search.Problem.Romania as Romania
import Search.Result exposing (Result(..))
import Search.Visualization.TreeMap as TreeMap
import Search.Visualization.TreeMap2 as TreeMap2
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Tuple


type alias State =
    List Int


type Msg
    = NewModel (Search.Model State State)
    | Show (Maybe ( Float, State ))
    | Move { x : Float, y : Float }


type alias Model =
    { searchModel : Search.Model State State
    , tooltip : TreeMap2.Tooltip State
    }


main =
    Browser.document
        { view =
            \{ tooltip, searchModel } ->
                { title = "Search of 8-Puzzle"
                , body = [ forceMap searchModel ]
                }
        , init = init
        , update = update
        , subscriptions =
            \_ -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init =
    \_ ->
        let
            initialModel =
                Search.breadthFirst complexEightPuzzle
        in
        ( { searchModel = initialModel
          , tooltip = { node = Nothing, position = { x = 0, y = 0 } }
          }
        , searchTask initialModel
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ tooltip } as model) =
    case msg of
        NewModel m ->
            ( { model | searchModel = m }
            , searchTask m
            )

        Show s ->
            ( { model | tooltip = { tooltip | node = s } }, Cmd.none )

        Move p ->
            ( { model | tooltip = { tooltip | position = p } }, Cmd.none )


searchTask : Search.Model State State -> Cmd Msg
searchTask model =
    case model.solution of
        Pending ->
            Task.perform
                NewModel
                (Process.sleep 100
                    |> Task.andThen
                        (\_ ->
                            Task.succeed
                                (Search.nextN 50 model)
                        )
                )

        _ ->
            Cmd.none


firstState : Search.Model a comparable -> List ( Int, ( comparable, Node a ) ) -> Force.State Int
firstState model list =
    simulation
        [ center 0 0
        , customLinks 1
            (list
                |> List.map
                    (\( i, ( state, node ) ) ->
                        node.parent
                            |> Maybe.map
                                (\parent ->
                                    { source =
                                        List.find
                                            (\( _, ( state_, _ ) ) ->
                                                state_ == model.problem.stateToComparable parent
                                            )
                                            list
                                            |> Maybe.map Tuple.first
                                            |> Maybe.withDefault 0
                                    , target = i
                                    , distance =
                                        node.pathCost
                                            - (model.explored
                                                |> Dict.get (model.problem.stateToComparable parent)
                                                |> Maybe.map .pathCost
                                                |> Maybe.withDefault 0
                                              )
                                    , strength = Just 1
                                    }
                                )
                    )
                |> Maybe.values
            )
        ]


sim model list =
    computeSimulation (firstState model list)
        (list
            |> List.map (\( i, ( state, _ ) ) -> entity i state)
        )


forceMap : Search.Model a comparable -> Html Msg
forceMap model =
    let
        list =
            List.indexedMap Tuple.pair (Dict.toList model.explored)

        entities =
            sim model list

        c =
            1 / 25
    in
    svg
        [ width "1000"
        , height "1000"
        , viewBox "0 0 1 1"
        , Svg.Attributes.style "border: 1px dotted black"
        ]
        ((entities
            |> List.map
                (\entity ->
                    circle
                        [ cx (String.fromFloat (entity.x * c + 0.5))
                        , cy (String.fromFloat (entity.y * c + 0.5))
                        , r "0.002"
                        ]
                        []
                )
         )
            ++ -- maybe hell
               (list
                    |> List.map
                        (\( id1, ( _, node ) ) ->
                            node.parent
                                |> Maybe.map
                                    (\parent ->
                                        List.find (\( _, ( state, _ ) ) -> state == model.problem.stateToComparable parent) list
                                            |> Maybe.map
                                                (\( id2, ( _, _ ) ) ->
                                                    Maybe.map2
                                                        (\e1 e2 ->
                                                            Svg.path
                                                                [ d
                                                                    ("M "
                                                                        ++ String.fromFloat (e1.x * c + 0.5)
                                                                        ++ " "
                                                                        ++ String.fromFloat (e1.y * c + 0.5)
                                                                        ++ "L "
                                                                        ++ String.fromFloat (e2.x * c + 0.5)
                                                                        ++ " "
                                                                        ++ String.fromFloat (e2.y * c + 0.5)
                                                                    )
                                                                , stroke "black"
                                                                , strokeWidth "0.0002"
                                                                ]
                                                                []
                                                        )
                                                        (List.find (\{ id } -> id == id1) entities)
                                                        (List.find (\{ id } -> id == id2) entities)
                                                )
                                            |> Maybe.join
                                    )
                                |> Maybe.join
                        )
                    |> Maybe.values
               )
        )
