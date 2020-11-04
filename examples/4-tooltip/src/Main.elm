module Main exposing (..)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Element exposing (layout)
import Html exposing (..)
import Json.Decode
import Problem exposing (Problem)
import Problem.Example exposing (Queens, SlidingPuzzle, complexEightPuzzle, mediumEightPuzzle, queens, routeFinding, simpleEightPuzzle, slidingPuzzleVisual)
import Problem.Search as Search exposing (Result(..))
import Problem.Search.Visual as Visual
import Process
import Task


main =
    Browser.document
        { view =
            \model ->
                { title = "Search"
                , body = view model
                }
        , init = init
        , update = update
        , subscriptions = \_ -> Visual.tooltip.sub Move
        }


type alias ProblemState =
    SlidingPuzzle


problem : Problem ProblemState
problem =
    complexEightPuzzle


type Msg
    = NewModel1 (Search.Model ProblemState)
    | NewModel2 (Search.Model ProblemState)
    | NewModel3 (Search.Model ProblemState)
    | Show (Maybe (Search.Node ProblemState))
    | Move { x : Float, y : Float }


type alias Model =
    { searchModel1 : Search.Model ProblemState
    , searchModel2 : Search.Model ProblemState
    , searchModel3 : Search.Model ProblemState
    , tooltipModel : Visual.TooltipModel Msg ProblemState
    }


view : Model -> List (Html Msg)
view { searchModel1, searchModel2, searchModel3, tooltipModel } =
    [ Visual.tooltip.view tooltipModel
    , table []
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
            [ td [] [ Visual.tree (Just tooltipModel) searchModel1 ]
            , td [] [ Visual.tree (Just tooltipModel) searchModel2 ]
            , td [] [ Visual.tree (Just tooltipModel) searchModel3 ]
            ]
        , tr []
            [ td [] [ Visual.treeMap (Just tooltipModel) searchModel1 ]
            , td [] [ Visual.treeMap (Just tooltipModel) searchModel2 ]
            , td [] [ Visual.treeMap (Just tooltipModel) searchModel3 ]
            ]
        ]
    ]


init : () -> ( Model, Cmd Msg )
init =
    \_ ->
        let
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
          , tooltipModel = Visual.tooltip.init problem Show (Just Problem.Example.slidingPuzzleVisual)
          }
        , Cmd.batch
            [ searchTask NewModel1 m1
            , searchTask NewModel2 m2
            , searchTask NewModel3 m3
            ]
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ tooltipModel } as model) =
    case msg of
        NewModel1 m ->
            ( { model | searchModel1 = m }, searchTask NewModel1 m )

        NewModel2 m ->
            ( { model | searchModel2 = m }, searchTask NewModel2 m )

        NewModel3 m ->
            ( { model | searchModel3 = m }, searchTask NewModel3 m )

        Show s ->
            ( { model | tooltipModel = { tooltipModel | node = s } }, Cmd.none )

        Move p ->
            ( { model | tooltipModel = { tooltipModel | position = p } }, Cmd.none )


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
