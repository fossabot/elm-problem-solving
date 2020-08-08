module Search.Problem.KnuthConjecture exposing (..)

import Search exposing (Problem)


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
                [ ( 1, sqrt n )
                , ( 1, toFloat (factorial (round n)) )
                ]

            else
                [ ( 1, sqrt n )
                , ( 1, toFloat (floor n) )
                ]
    , heuristic = \_ -> 0
    , goalTest = \n -> n == goal
    }


simpleKnuthConjecture : Problem State
simpleKnuthConjecture =
    knuthConjecture 1


complexKnuthConjecture : Problem State
complexKnuthConjecture =
    knuthConjecture 5
