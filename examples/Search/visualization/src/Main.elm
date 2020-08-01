module Main exposing (..)

import Browser
import Browser.Dom exposing (Element)
import Element
    exposing
        ( Element
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , layout
        , none
        , padding
        , rgb
        , row
        , scale
        , spacing
        , text
        , width
        , wrappedRow
        )
import Element.Background as Background
import Element.Border as Border
import List.Extra as List
import Process
import Search
    exposing
        ( Model
        , Node
        , Parent(..)
        , Solution(..)
        , graphSearchStep
        , init
        , insertFirst
        , insertLast
        , path
        )
import Search.Problems.NPuzzle exposing (mediumEightPuzzle)
import Task


main =
    Browser.document
        { view =
            \model ->
                { title = "Breadth-first search of 8-Puzzle"
                , body =
                    [ layout [ scale 0.95 ]
                        (visualization (\_ -> none) model.exploredNodes)
                    ]
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
            searchModel =
                Search.init insertLast mediumEightPuzzle

            initialModel =
                { searchModel = searchModel, exploredNodes = searchModel.frontier }
        in
        ( initialModel, searchTask initialModel.searchModel )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewModel m ->
            ( { model
                | searchModel = m
                , exploredNodes = List.uniqueBy .state (model.exploredNodes ++ m.frontier)
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


properties level maxLevel =
    let
        s =
            1 - (toFloat level / max (toFloat maxLevel) 5 * 0.4)
    in
    [ width fill
    , height fill
    , padding 2
    , spacing 2
    , Border.rounded 20
    , Background.color (rgb s s s)
    ]


columnOrRow : Int -> Int -> (List (Element Msg) -> Element Msg)
columnOrRow level maxLevel =
    (if modBy 2 level == 0 then
        column

     else
        wrappedRow
    )
        (properties level maxLevel)


rowOrColumn : Int -> Int -> (List (Element Msg) -> Element Msg)
rowOrColumn level maxLevel =
    (if modBy 2 level == 0 then
        wrappedRow

     else
        column
    )
        (properties level maxLevel)


listify : (State -> Element Msg) -> Int -> Int -> Hierarchy State -> Element Msg
listify visualizeState level maxDepth (Hierarchy ( a, h )) =
    columnOrRow level
        maxDepth
        (el [ centerX, centerY ] (visualizeState a)
            :: (if List.length h > 0 then
                    [ rowOrColumn
                        level
                        maxDepth
                        (List.map (listify visualizeState (level + 1) maxDepth) h)
                    ]

                else
                    []
               )
        )


depth : List (Node a) -> Int
depth l =
    l |> List.map (\a -> List.length (path a)) |> List.foldl max 0


visualizeNPuzzle : State -> Element Msg
visualizeNPuzzle state =
    column [ scale 0.6 ]
        (state
            |> List.groupsOf 3
            |> List.map
                (\row ->
                    row
                        |> List.map (String.fromInt >> text >> el [])
                )
            |> List.map (row [])
        )


visualization : (State -> Element Msg) -> List (Node State) -> Element Msg
visualization visualizeState l =
    listify visualizeState 0 (depth l) (descendants (root l) l)
