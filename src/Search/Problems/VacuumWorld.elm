module Search.Problems.VacuumWorld exposing (vacuumWorld)

import Search exposing (Problem)


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


vacuumWorld : Problem State
vacuumWorld =
    { initialState =
        { location = A
        , a = Dirty
        , b = Dirty
        }
    , actions = \state -> List.map (\f -> ( 1, f state )) [ left, right, suck ]
    , goalTest = \state -> state.a == Clean && state.b == Clean
    }
