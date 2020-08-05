module Main exposing (..)

import Browser
import Browser.Events
import Element exposing (..)
import Html exposing (Html)
import Json.Decode
import List.Extra as List
import Process
import Search
    exposing
        ( Model
        , Parent(..)
        , Solution(..)
        , graphSearchStep
        , init
        , insertLast
        )
import Search.Problems.NPuzzle exposing (complexEightPuzzle, mediumEightPuzzle, simpleEightPuzzle)
import Search.Visualization exposing (visualization)
import Task


type alias State =
    List Int


type alias Model =
    { searchModel : Search.Model State
    , shown : Maybe ( Float, State )
    , pos : ( Float, Float )
    }


type Msg
    = NewModel (Search.Model State)
    | Show (Maybe ( Float, State ))
    | Move ( Float, Float )


main =
    Browser.document
        { view =
            \model ->
                { title = "Breadth-first search of 8-Puzzle"
                , body = [ layout [] (body model) ]
                }
        , init = init
        , update = update
        , subscriptions = \_ -> Browser.Events.onMouseMove (Json.Decode.map Move decode)
        }


body : Model -> Element Msg
body model =
    column [ width fill, height fill, pointer ]
        [ html
            (visualization
                model.searchModel
                Show
                visualizeNPuzzle
                model.shown
                model.pos
            )
        ]


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
                        (\_ -> Task.succeed (graphSearchStep model))
                )

        _ ->
            Cmd.none


init : () -> ( Model, Cmd Msg )
init =
    \_ ->
        let
            initialModel =
                Search.init insertLast mediumEightPuzzle
        in
        ( { searchModel = initialModel
          , shown = Nothing
          , pos = ( 0, 0 )
          }
        , searchTask initialModel
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewModel m ->
            ( { model | searchModel = m }
            , searchTask m
            )

        Show s ->
            ( { model | shown = s }, Cmd.none )

        Move ( x, y ) ->
            ( { model | pos = ( x, y ) }, Cmd.none )



-- STAY HERE


visualizeNPuzzle : State -> Html Msg
visualizeNPuzzle state =
    layout []
        (column []
            (state
                |> List.groupsOf 3
                |> List.map
                    (\row ->
                        row
                            |> List.map (String.fromInt >> text >> el [])
                    )
                |> List.map (row [])
            )
        )
