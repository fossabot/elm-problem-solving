module Search.Result exposing (map)

import Search


map : (a -> b) -> Search.Result a -> Search.Result b
map f r =
    case r of
        Search.Solution a ->
            Search.Solution (f a)

        Search.Pending ->
            Search.Pending

        Search.Failure ->
            Search.Failure
