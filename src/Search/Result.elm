module Search.Result exposing (Result(..), is, map, withDefault)

import Search.Problem exposing (Node)


type Result a
    = Pending
    | Solution a
    | Failure


map : (a -> b) -> Result a -> Result b
map f result =
    case result of
        Solution a ->
            Solution (f a)

        Pending ->
            Pending

        Failure ->
            Failure


withDefault : a -> Result a -> a
withDefault default result =
    case result of
        Solution a ->
            a

        Pending ->
            default

        Failure ->
            default


is : a -> Result ( a, Node a ) -> Bool
is state result =
    case result of
        Solution ( state_, _ ) ->
            state_ == state

        _ ->
            False
