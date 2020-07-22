module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import List.Extra as List
import Search exposing (SearchProblem)
import Search.Component exposing (breadthFirstSearch, SearchModel, searchInit)
import Search.NPuzzle as NPuzzle
import Set



---- PROBLEM ----


type alias State =
    List Int


puzzle : SearchProblem State
puzzle =
    [ [ 2, 0, 5 ]
    , [ 1, 4, 8 ]
    , [ 3, 6, 7 ]
    ]
        |> List.concat
        |> NPuzzle.fromList
        |> Result.withDefault NPuzzle.empty



---- MODEL ----


type alias Model =
    { searchModel : SearchModel State }


init : ( Model, Cmd Msg )
init =
    ( { searchModel = searchInit puzzle }, Cmd.none )



---- UPDATE ----


type Msg
    = Start
    | SearchOn ( SearchModel State, SearchModel State -> Cmd Msg )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( model, breadthFirstSearch SearchOn model.searchModel )

        SearchOn ( result, callback ) ->
            ( { model | searchModel = result }, callback result )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Start ] [ text "Start!" ]
        , div [] [ text "State of currently explored node: ", text (Debug.toString (Maybe.map (\a -> a.state) (List.head model.searchModel.frontier))) ]
        , div [] [ text "Number of explored nodes: ", text (String.fromInt (Set.size model.searchModel.explored)) ]
        , div [] [ text "Number of frontier nodes: ", text (String.fromInt (List.length model.searchModel.frontier)) ]
        , div []
            [ text "Solution: "
            , case model.searchModel.solution of
                Just solution ->
                    case solution of
                        Just a ->
                            text (Debug.toString a)

                        Nothing ->
                            text "There is no solution."

                Nothing ->
                    text "Not found yet."
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
