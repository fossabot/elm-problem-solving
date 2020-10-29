module Problem.Example.KnuthConjecture exposing (..)

import Problem exposing (Problem)


type Operation
    = Start
    | Factorial
    | SquareRoot
    | Floor


type alias State =
    Float


factorial : Int -> Int
factorial n =
    List.product (List.range 1 n)


knuthConjecture : Float -> Problem State
knuthConjecture goal =
    { initialState = 4
    , actions =
        \n ->
            if toFloat (round n) == n then
                [ { stepCost = 1, result = sqrt n }
                , { stepCost = 1, result = toFloat (factorial (round n)) }
                ]

            else
                [ { stepCost = 1, result = toFloat (floor n) }
                , { stepCost = 1, result = sqrt n }
                ]
    , heuristic = \_ -> 0
    , goalTest = \n -> n == goal
    , stateToString = String.fromFloat
    }


simpleKnuthConjecture : Problem State
simpleKnuthConjecture =
    knuthConjecture 1


complexKnuthConjecture : Problem State
complexKnuthConjecture =
    knuthConjecture 5
