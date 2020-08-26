module Search.Problem.MotionPlanning exposing (State, problem, simpleConfig, simpleProblem, stateToString)

import Dict
import List.Extra as List
import Search
import Search.Problem exposing (Problem)
import Search.Result exposing (Result(..))


type alias State =
    ( Int, Int )


type alias Obstacle =
    List ( Int, Int )


type alias Config =
    { size : ( Int, Int )
    , obstacles : List Obstacle
    , start : ( Int, Int )
    , goal : ( Int, Int )
    }


problem : Config -> Problem State State
problem { size, obstacles, start, goal } =
    let
        ( width, height ) =
            size

        obstacleStates_ : List ( Int, Int )
        obstacleStates_ =
            obstacles
                |> List.map (obstacleStates [])
                |> List.concat
    in
    { initialState = start
    , actions =
        \( x, y ) ->
            [ ( 1, ( x + 1, y ) )
            , ( 1, ( x, y + 1 ) )
            , ( 1, ( x - 1, y ) )
            , ( 1, ( x, y - 1 ) )
            , ( sqrt 2, ( x + 1, y + 1 ) )
            , ( sqrt 2, ( x + 1, y - 1 ) )
            , ( sqrt 2, ( x - 1, y + 1 ) )
            , ( sqrt 2, ( x - 1, y - 1 ) )
            ]
                |> List.filter
                    (\( _, ( x_, y_ ) ) ->
                        x_
                            >= 0
                            && y_
                            >= 0
                            && x_
                            < width
                            && y_
                            < height
                            && List.notMember
                                ( x_, y_ )
                                obstacleStates_
                    )
    , heuristic =
        let
            manhattanDist ( x1, y1 ) ( x2, y2 ) =
                abs (x2 - x1) + abs (y2 - y1)
        in
        manhattanDist goal >> toFloat
    , goalTest = (==) goal
    , stateToComparable = identity
    }


obstacleStates : List ( Int, Int ) -> Obstacle -> List ( Int, Int )
obstacleStates states obstacle =
    case obstacle of
        ( x1, y1 ) :: ( x2, y2 ) :: tail ->
            if x1 == x2 then
                obstacleStates
                    ((List.range (min y1 y2) (max y1 y2)
                        |> List.map (\y -> ( x1, y ))
                     )
                        ++ states
                    )
                    (( x2, y2 ) :: tail)

            else if y1 == y2 then
                obstacleStates
                    ((List.range (min x1 x2) (max x1 x2)
                        |> List.map (\x -> ( x, y1 ))
                     )
                        ++ states
                    )
                    (( x2, y2 ) :: tail)

            else if x2 - x1 == y2 - y1 then
                obstacleStates
                    ((List.range (min x1 x2) (max x1 x2)
                        |> List.map (\a -> ( a, a ))
                     )
                        ++ states
                    )
                    (( x2, y2 ) :: tail)

            else
                obstacleStates
                    states
                    (( x1, y1 )
                        :: ( x1 + ((x2 - x1) // 2)
                           , y1 + ((y2 - y1) // 2)
                           )
                        :: ( x2, y2 )
                        :: tail
                    )

        a :: [] ->
            a :: states |> List.unique

        [] ->
            states |> List.unique


simpleConfig : Config
simpleConfig =
    { size = ( 20, 18 )
    , obstacles =
        [ [ ( 5, 13 )
          , ( 5, 15 )
          , ( 15, 15 )
          , ( 15, 8 )
          , ( 13, 8 )
          , ( 13, 13 )
          , ( 5, 13 )
          ]
        ]
    , start = ( 0, 0 )
    , goal = ( 19, 17 )
    }


simpleProblem : Problem State State
simpleProblem =
    problem simpleConfig


stateToString :
    Config
    -> Search.Model State State
    -> String
stateToString { size, obstacles } searchModel =
    let
        ( width, height ) =
            size

        horizontal a b =
            a ++ (List.range 0 (width - 1) |> List.map (\_ -> "─") |> String.concat) ++ b

        top =
            horizontal "\n╭" "╮"

        bottom =
            horizontal "\n╰" "╯\n"

        path =
            case searchModel.solution of
                Search.Result.Solution ( state, node ) ->
                    Search.path searchModel ( state, node )
                        |> List.map Tuple.second

                _ ->
                    []

        obstacleStates_ =
            obstacles
                |> List.map (obstacleStates [])
                |> List.concat

        mid =
            List.range 0 (height - 1)
                |> List.map
                    (\y ->
                        "|"
                            ++ (List.range 0 (width - 1)
                                    |> List.map
                                        (\x ->
                                            if searchModel.solution |> Search.Result.is ( x, y ) then
                                                "●"

                                            else if ( x, y ) == searchModel.problem.initialState then
                                                "○"

                                            else if List.member ( x, y ) path then
                                                "•"

                                            else if Dict.member ( x, y ) searchModel.explored then
                                                "⋅"

                                            else if List.member ( x, y ) obstacleStates_ then
                                                "▒"

                                            else
                                                " "
                                        )
                                    |> String.concat
                               )
                            ++ "|"
                    )
                |> List.reverse
                |> List.foldl (\a acc -> acc ++ "\n" ++ a) ""
    in
    top ++ mid ++ bottom
