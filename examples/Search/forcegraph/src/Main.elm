module Main exposing (..)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Force exposing (..)
import Html exposing (Html, p, text)
import List
import List.Extra as List
import Maybe
import Maybe.Extra as Maybe
import Process
import Search
import Search.Problem exposing (Node)
import Search.Problem.Graph exposing (routeFinding)
import Search.Problem.NPuzzle as NPuzzle exposing (complexEightPuzzle, mediumEightPuzzle, simpleEightPuzzle, visualize)
import Search.Problem.NQueens exposing (incrementalEightQueens)
import Search.Problem.Romania as Romania
import Search.Result exposing (Result(..))
import Search.Visualization.TreeMap as TreeMap
import Search.Visualization.TreeMap2 as TreeMap2
import Set exposing (Set)
import String
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Tuple


type alias State =
    List ( Int, Int )


type Msg
    = NewModel (Search.Model State State)


type alias Node =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , id : State
    }


type alias Model =
    { searchModel : Search.Model State State
    , existingStates : Set State
    , nodes : List (Force.Entity State {})
    , edges :
        List
            { source : State
            , target : State
            , distance : Float
            , strength : Maybe Float
            }
    }


main =
    Browser.document
        { view =
            \model ->
                { title = "Search of 8-Puzzle"
                , body = [ forceMap model ]
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
            problem =
                incrementalEightQueens

            initialState =
                problem.stateToComparable problem.initialState

            initialSearchModel =
                Search.bestFirst problem
        in
        ( { searchModel = initialSearchModel
          , existingStates = Set.fromList [ initialState ]
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
        , searchTask initialSearchModel
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewModel m ->
            let
                newNodes =
                    model.searchModel.explored
                        |> Dict.filter (\k _ -> not <| Set.member k model.existingStates)
                        |> Dict.toList

                nodes : List (Entity State {})
                nodes =
                    (newNodes
                        |> List.indexedMap
                            (\i ( state, { parent } ) ->
                                let
                                    parentNode =
                                        model.nodes |> List.find (\{ id } -> Just id == parent)
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

                simulatedNodes : List (Entity State {})
                simulatedNodes =
                    computeSimulation
                        (simulation
                            [ center 0 0
                            , customLinks 1 model.edges
                            , manyBodyStrength -0.002 (List.map .id nodes)
                            ]
                        )
                        nodes
                        |> List.map (\({ x } as a) -> { a | x = x })

                edges =
                    (newNodes
                        |> List.map
                            (\( state, { parent, pathCost } ) ->
                                Maybe.map
                                    (\parent_ ->
                                        { source = parent_
                                        , target = state
                                        , distance =
                                            pathCost
                                                - (m.explored
                                                    |> Dict.get (m.problem.stateToComparable parent_)
                                                    |> Maybe.map .pathCost
                                                    |> Maybe.withDefault 0
                                                  )
                                        , strength = Nothing
                                        }
                                    )
                                    parent
                            )
                        |> Maybe.values
                    )
                        ++ model.edges
            in
            ( { model
                | searchModel = m
                , nodes = simulatedNodes
                , edges = edges
                , existingStates =
                    Set.union
                        (newNodes
                            |> List.map Tuple.first
                            |> Set.fromList
                        )
                        model.existingStates
              }
              --, Cmd.none
            , searchTask m
            )


searchTask : Search.Model State State -> Cmd Msg
searchTask model =
    case model.solution of
        Pending ->
            Task.perform
                NewModel
                (Process.sleep 0
                    |> Task.andThen
                        (\_ ->
                            Task.succeed
                                (Search.next model)
                        )
                )

        _ ->
            Cmd.none


forceMap : Model -> Html Msg
forceMap model =
    let
        minX =
            List.minimumBy .x model.nodes |> Maybe.map .x |> Maybe.withDefault 0

        minY =
            List.minimumBy .y model.nodes |> Maybe.map .y |> Maybe.withDefault 0

        maxX =
            List.maximumBy .x model.nodes |> Maybe.map .x |> Maybe.withDefault 0

        maxY =
            List.maximumBy .y model.nodes |> Maybe.map .y |> Maybe.withDefault 0

        cx =
            1 / (maxX - minX)

        cy =
            1 / (maxY - minY)

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
    svg
        [ width "900"
        , height "900"
        , viewBox "0 0 1 1"
        , Svg.Attributes.style "border: 1px dotted black"
        ]
        ((model.nodes
            |> List.map
                (\entity ->
                    g
                        [ transform
                            ("translate ("
                                ++ posString ( entity.x, entity.y )
                                ++ ")"
                            )
                        ]
                        [ circle [ r "0.002" ] []

                        {--, Svg.text_
                            [ stroke "black", strokeWidth "0.1", transform "translate(0.005 0) scale(0.001) ", fontSize "10" ]
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
        [ d
            ("M "
                ++ posString ( e1.x, e1.y )
                ++ "L "
                ++ posString ( e2.x, e2.y )
            )
        , stroke "black"
        , strokeWidth "0.0002"
        ]
        []
