module Problem.Example.VacuumWorld exposing (vacuumWorld, State)

import Problem exposing (Problem)


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


stateToString : State -> String
stateToString state =
    (case state.location of
        A ->
            "A"

        B ->
            "B"
    )
        ++ ","
        ++ (case state.a of
                Clean ->
                    "Clean"

                Dirty ->
                    "Dirty"
           )
        ++ ","
        ++ (case state.b of
                Clean ->
                    "Clean"

                Dirty ->
                    "Dirty"
           )


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
    , actions = \state -> List.map (\f -> {stepCost= 1, result = f state }) [ left, right, suck ]
    , heuristic = \_ -> 0
    , goalTest = \state -> state.a == Clean && state.b == Clean
    , stateToString = stateToString
    }
