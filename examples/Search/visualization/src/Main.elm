module Main exposing (..)

import Browser
import Html exposing (Html, li, text, ul)
import List.Extra as List
import Process
import Search exposing (Model, Node, Parent(..), Solution(..), graphSearchStep, init, insertLast)
import Search.Problems.NPuzzle exposing (mediumEightPuzzle)
import Task


main =
    Browser.document
        { view =
            \model ->
                { title = "Breadth-first search of 8-Puzzle"
                , body = [ visualization model.exploredNodes ]
                }
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias State =
    List Int


type Msg
    = NewModel (Search.Model State)


searchTask : Search.Model State -> Cmd Msg
searchTask model =
    case model.solution of
        Pending ->
            Task.perform
                NewModel
                (Process.sleep 0
                    |> Task.andThen
                        (\_ -> Task.succeed (graphSearchStep model))
                )

        _ ->
            Cmd.none


type alias Model =
    { searchModel : Search.Model State
    , exploredNodes : List (Node State)
    }


init : () -> ( Model, Cmd Msg )
init =
    \_ ->
        let
            initialModel =
                { searchModel = Search.init insertLast mediumEightPuzzle, exploredNodes = [] }
        in
        ( initialModel, searchTask initialModel.searchModel )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewModel m ->
            ( { model
                | searchModel = m
                , exploredNodes =
                    case List.head model.searchModel.frontier of
                        Just a ->
                            a :: model.exploredNodes

                        Nothing ->
                            model.exploredNodes
              }
            , searchTask m
            )


emptyNode : Node State
emptyNode =
    { state = [], parent = Nothing, pathCost = 0 }


root : List (Node State) -> Node State
root l =
    Maybe.withDefault emptyNode (List.find (\a -> a.parent == Nothing) l)


children : Node State -> List (Node State) -> List (Node State)
children parent l =
    List.filter (\a -> a.parent == Just (Parent parent)) l


type Hierarchy a
    = Hierarchy ( a, List (Hierarchy a) )


descendants : Node State -> List (Node State) -> Hierarchy State
descendants ancestor l =
    Hierarchy ( ancestor.state, children ancestor l |> List.map (\child -> descendants child l) )


listify : Hierarchy a -> List (Html Msg)
listify (Hierarchy ( a, h )) =
    li [] [ text (Debug.toString a) ]
        :: (if List.length h > 0 then
                [ li [] [ ul [] (List.concat (List.map listify h)) ] ]

            else
                []
           )


visualization : List (Node State) -> Html Msg
visualization l =
    ul [] (listify (descendants (root l) l))
