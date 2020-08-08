module Main exposing (..)

import Browser
import Browser.Events
import Json.Decode
import Process
import Search exposing (Result(..))
import Search.Problems.NPuzzle as NPuzzle exposing (complexEightPuzzle, mediumEightPuzzle, simpleEightPuzzle, visualize)
import Search.Visualization as Visualization
import Task


type alias State =
    List Int


type Msg
    = NewModel (Search.Model State)
    | Show (Maybe ( Float, State )) 
    | Move ( Float, Float )


type alias Model =
    Visualization.Model State Msg


main =
    Browser.document
        { view =
            \model ->
                { title = "Breadth-first search of 8-Puzzle"
                , body = [ Visualization.html model ]
                }
        , init = init
        , update = update
        , subscriptions = \_ -> Browser.Events.onMouseMove (Json.Decode.map Move decode)
        }


init : () -> ( Model, Cmd Msg )
init =
    \_ ->
        let
            initialModel =
                Search.breadthFirst mediumEightPuzzle
        in
        ( { searchModel = initialModel
          , msg = Show
          , visualizeState = NPuzzle.visualize
          , tooltip = Just { node = Nothing, position = ( 0, 0 ) }
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
            ( { model | tooltip = tooltip |> Maybe.map (\t -> { t | node = s }) }, Cmd.none )

        Move ( x, y ) ->
            ( { model | tooltip = tooltip |> Maybe.map (\t -> { t | position = ( x, y ) }) }, Cmd.none )


decode : Json.Decode.Decoder ( Float, Float )
decode =
    Json.Decode.map2 (\a b -> ( a, b ))
        (Json.Decode.field "pageX" Json.Decode.float)
        (Json.Decode.field "pageY" Json.Decode.float)


searchTask : Search.Model State -> Cmd Msg
searchTask model =
    case model.solution of
        Pending ->
            Task.perform
                NewModel
                (Process.sleep 0
                    |> Task.andThen
                        (\_ -> Task.succeed (Search.next model))
                )

        _ ->
            Cmd.none
