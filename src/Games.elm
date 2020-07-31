module Games exposing (..)


type alias Game a =
    { initialState : a
    , actions : a -> List ( Float, a )
    , terminalTest : a -> Bool
    , utility : a -> Float
    }
