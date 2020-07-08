module Search.VacuumWorld exposing (..)

import Search exposing (..)


type Location
    = A
    | B


type Condition
    = Clean
    | Dirty


type alias State =
    { location : Location
    , a : Condition
    , b : Condition
    }


type alias Action =
    State -> State


left : Action
left state =
    { state | location = A }


right : Action
right state =
    { state | location = B }


suck : Action
suck state =
    case state.location of
        A ->
            { state | a = Clean }

        B ->
            { state | b = Clean }


vacuumWorld : SearchProblem State
vacuumWorld =
    { initialState =
        { location = A
        , a = Dirty
        , b = Dirty
        }
    , actions = \_ -> [ left, right, suck ]
    , stepCost = \_ _ -> 1
    , goalTest = \state -> state.a == Clean && state.b == Clean
    }
