module Main exposing (..)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (..)
import Json.Decode
import Problem.Example exposing (Queens, SlidingPuzzle, complexEightPuzzle, mediumEightPuzzle, queens, routeFinding, simpleEightPuzzle, slidingPuzzleVisual)
import Problem.Search as Search exposing (Result(..))
import Problem.Search.Visual as Visual
import Process
import Task


type alias ProblemState =
    SlidingPuzzle


type Msg
    = NewModel1 (Search.Model ProblemState)
    | NewModel2 (Search.Model ProblemState)
    | NewModel3 (Search.Model ProblemState)


type alias Model =
    { searchModel1 : Search.Model ProblemState
    , searchModel2 : Search.Model ProblemState
    , searchModel3 : Search.Model ProblemState
    }


main =
    Browser.document
        { view =
            \{ searchModel1, searchModel2, searchModel3 } ->
                { title = "Search of 8-Puzzle"
                , body =
                    [ table []
                        [ tr []
                            [ th [] [ text "Uniform-cost" ]
                            , th [] [ text "Best-first" ]
                            , th [] [ text "Greedy" ]
                            ]
                        , tr []
                            [ td [] [ Visual.scatter searchModel1 ]
                            , td [] [ Visual.scatter searchModel2 ]
                            , td [] [ Visual.scatter searchModel3 ]
                            ]
                        , tr []
                            [ td [] [ Visual.tree searchModel1 ]
                            , td [] [ Visual.tree searchModel2 ]
                            , td [] [ Visual.tree searchModel3 ]
                            ]
                        , tr []
                            [ td [] [ Visual.treeMap searchModel1 ]
                            , td [] [ Visual.treeMap searchModel2 ]
                            , td [] [ Visual.treeMap searchModel3 ]
                            ]
                        ]
                    ]
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
                complexEightPuzzle

            m1 =
                Search.uniformCost problem

            m2 =
                Search.bestFirst problem

            m3 =
                Search.greedy problem
        in
        ( { searchModel1 = m1
          , searchModel2 = m2
          , searchModel3 = m3
          }
        , Cmd.batch
            [ searchTask NewModel1 m1
            , searchTask NewModel2 m2
            , searchTask NewModel3 m3
            ]
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewModel1 m ->
            ( { model | searchModel1 = m }, searchTask NewModel1 m )

        NewModel2 m ->
            ( { model | searchModel2 = m }, searchTask NewModel2 m )

        NewModel3 m ->
            ( { model | searchModel3 = m }, searchTask NewModel3 m )


searchTask : (Search.Model ProblemState -> Msg) -> Search.Model ProblemState -> Cmd Msg
searchTask msg model =
    case model.result of
        Pending ->
            Task.perform
                msg
                (Process.sleep 1000
                    |> Task.andThen
                        (\_ -> Task.succeed (Search.next model))
                )

        _ ->
            Cmd.none
