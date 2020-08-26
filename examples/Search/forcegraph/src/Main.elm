module Main exposing (..)

import Browser
import Process
import Search
import Search.Problem exposing (Node)
import Search.Problem.Graph exposing (routeFinding)
import Search.Problem.MotionPlanning as MotionPlanning
import Search.Problem.NPuzzle as NPuzzle exposing (complexEightPuzzle, mediumEightPuzzle, simpleEightPuzzle, visualize)
import Search.Problem.NQueens exposing (incrementalEightQueens)
import Search.Problem.Romania as Romania
import Search.Result exposing (Result(..))
import Search.Visualization.Graph as GraphVis
import Task


type alias State =
    MotionPlanning.State


type Msg
    = NewModel (Search.Model State State)


type alias Model =
    { search : Search.Model State State
    , graph : GraphVis.Model State
    }


main =
    Browser.document
        { view =
            \model ->
                { title = "Search"
                , body = [ GraphVis.view model.graph ]
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
                MotionPlanning.simpleProblem

            search =
                Search.breadthFirst

            initialSearchModel =
                search problem
        in
        ( { search = initialSearchModel
          , graph = GraphVis.init problem
          }
        , searchTask initialSearchModel
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewModel newSearchModel ->
            ( { model
                | search = newSearchModel
                , graph = GraphVis.update model.graph newSearchModel
              }
            , searchTask newSearchModel
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
