module Search.Result exposing (Result(..), map, withDefault)


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
