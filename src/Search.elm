module Search exposing (..)

{-| Intelligent search


# Uninformed search

@docs breadthFirstSearch, breadthFirstTreeSearch, depthFirstSearch, depthFirstTreeSearch


# Informed search


# Accessing progress while searching

This is useful if you want to work with the internal model of the search algorithms from the `Search` module.

For example:

  - For making animations of how the algorithms work.
  - For extensive logging. Or for just displaying the number of explored states.

You can use the functions from this module to embed the inner structure of the search algorithm (the `SearchModel`) into the model of your web application.

Here is a minimal example, which you can also find in the [`Search/ComponentMinimal`](../../../../examples/Search/ComponentMinimal/src/Main.elm) example. A logging use case can be found in the [`Search/Component`](../../../../examples/Search/Component/src/Main.elm) example.

    ...

In this example, the model of the application _is_ the model of the search algorithm. In a real scenario, it would most likely reside as a sub-model within the application model. You can look that up in the [`Search/Component`](../../../../examples/Search/Component/src/Main.elm) example.

@docs treeSearchStep, graphSearchStep

-}

import Dict exposing (Dict)
import List.Extra as List
import Search.Problem as Problem exposing (Node, emptyNode, expand)


type alias Problem a b =
    Problem.Problem a b



-- QUEUES


type alias Queue a b =
    { pop : Dict b (Node a) -> List a -> Maybe ( a, List a ) }


{-| simulates a First-In-First-Out queue when using `::` for insertion
-}
fifo : Queue a b
fifo =
    { pop = \_ -> List.unconsLast }


{-| simulates a Last-In-First-Out queue when using `::` for insertion
-}
lifo : Queue a b
lifo =
    { pop = \_ -> List.uncons }


{-| simulates a priority queue when using `::` for insertion
-}
priority : (Dict b (Node a) -> a -> comparable) -> Queue a b
priority f =
    { pop =
        \d l ->
            List.minimumBy (f d) l
                |> Maybe.map (\a -> ( a, List.remove a l ))
    }



-- STRATEGIES


type alias Strategy a b =
    { frontier :
        Problem a b
        -> Dict b (Node a)
        -> List ( a, Node a )
        -> a
        -> List a
        -> List ( a, Node a )
    }


treeSearch : Strategy a b
treeSearch =
    { frontier = \_ _ childNodes _ t -> childNodes }


{-| Ensures states are not explored twice and always at the lowest known path cost.
-}
graphSearch : Strategy a comparable
graphSearch =
    { frontier =
        \problem explored childNodes h t ->
            -- only add child node if
            -- a) their state is not the same as their parent's and
            -- b) their state is not in a sibling node with a lower path cost TODO
            -- c) their state is not already explored and
            -- d) their state is not already in the frontier with a lower path cost TODO
            childNodes
                |> List.filter
                    (\( state, { pathCost } ) ->
                        -- check parent
                        not (state == h)
                            -- check sibling
                            && not
                                (List.any
                                    (\( state_, sibling ) ->
                                        state
                                            == state_
                                            && pathCost
                                            < sibling.pathCost
                                    )
                                    childNodes
                                )
                            -- check explored
                            && not
                                (Dict.member (problem.stateToComparable state) explored)
                    )
    }



-- MODEL


type Result a
    = Pending
    | Solution a
    | Failure


{-| This record represents the inner state of the search algorithm. You can integrate it into the model of your web application.

The `state` parameter refers to the `State` type of the search problem. For example, if you want to search an eight-puzzle, you can import it with `import Search.EightPuzzle exposing (State)`.

Initialize your model with `searchInit` (see below).

-- technically it would suffice to store only explored states and not their children
-- but there is no noticeable difference in performance (TODO benchmarks)
-- and having the children is useful for performant visualization, where we want to reconstruct the search tree

-}
type alias Model a b =
    { strategy : Strategy a b
    , queue : Queue a b
    , problem : Problem a b
    , explored : Dict b (Node a)
    , frontier : List a
    , solution : Result ( a, Node a )
    , maxPathCost : Float
    }


{-| Initializes your model of the search algorithm. It takes a `Problem state` as parameter, because it needs to know the `initialState` of the search problem for initializing the frontier, and also the whole other information about the search problem for running the search algorithm later.
-}
init :
    Strategy a comparable
    -> Queue a comparable
    -> Problem a comparable
    -> Model a comparable
init strategy queue problem =
    { strategy = strategy
    , queue = queue
    , problem = problem
    , explored = Dict.empty
    , frontier = [ problem.initialState ]
    , solution = Pending
    , maxPathCost = 0
    }


searchStep :
    Strategy a comparable
    -> Queue a comparable
    -> Model a comparable
    -> Model a comparable
searchStep strategy queue ({ problem, explored } as model) =
    case queue.pop explored model.frontier of
        Just ( h, t ) ->
            let
                ( updatedParent, childNodes ) =
                    expand problem
                        ( h
                        , Dict.get (problem.stateToComparable h) explored
                            |> Maybe.withDefault emptyNode
                        )

                filteredChildNodes =
                    strategy.frontier problem explored childNodes h t
            in
            { model
                | solution =
                    case List.find (\( a, _ ) -> problem.goalTest a) childNodes of
                        Just a ->
                            Solution a

                        Nothing ->
                            model.solution
                , frontier =
                    (filteredChildNodes
                        |> List.map Tuple.first
                        |> List.reverse
                    )
                        ++ t
                , explored =
                    explored
                        |> Dict.insert
                            (problem.stateToComparable h)
                            updatedParent
                        |> Dict.union
                            (filteredChildNodes
                                |> List.map (\( a, node ) -> ( problem.stateToComparable a, node ))
                                |> Dict.fromList
                            )
                , maxPathCost = List.foldl max model.maxPathCost (List.map (Tuple.second >> .pathCost) childNodes)
            }

        Nothing ->
            { model | solution = Failure }



-- STEPPERS


next : Model a comparable -> Model a comparable
next model =
    searchStep model.strategy model.queue model


nextN : Int -> Model a comparable -> Model a comparable
nextN n model =
    if n > 0 then
        searchStep model.strategy model.queue model |> nextN (n - 1)

    else
        model


nextGoal : Model a comparable -> ( Maybe ( a, Node a ), Model a comparable )
nextGoal model =
    let
        newModel =
            next model
    in
    case newModel.solution of
        Solution a ->
            ( Just a, newModel )

        Failure ->
            ( Nothing, newModel )

        Pending ->
            nextGoal newModel



-- INTERFACE


breadthFirst : Problem a comparable -> Model a comparable
breadthFirst =
    init graphSearch fifo


depthFirst : Problem a comparable -> Model a comparable
depthFirst =
    init graphSearch lifo


{-| Dijkstra's algorithm.
-}
uniformCost : Problem a comparable -> Model a comparable
uniformCost problem =
    init graphSearch
        (priority
            (\explored state ->
                explored
                    |> getUnsafe (problem.stateToComparable state)
                    |> .pathCost
            )
        )
        problem


greedy : Problem a comparable -> Model a comparable
greedy problem =
    init graphSearch
        (priority (\_ state -> problem.heuristic state))
        problem


{-| A\* search.
-}
bestFirst : Problem a comparable -> Model a comparable
bestFirst problem =
    init
        graphSearch
        (priority
            (\explored state ->
                (explored
                    |> getUnsafe (problem.stateToComparable state)
                    |> .pathCost
                )
                    + problem.heuristic state
            )
        )
        problem



--


getUnsafe : comparable -> Dict comparable (Node b) -> Node b
getUnsafe el dict =
    dict |> Dict.get el |> Maybe.withDefault emptyNode


path : Model a comparable -> ( a, Node a ) -> List ( Float, a )
path ({ problem, explored } as model) ( state, { pathCost, parent } ) =
    ( pathCost, state )
        :: (case parent of
                Just state_ ->
                    path
                        model
                        ( state_, getUnsafe (problem.stateToComparable state_) explored )

                Nothing ->
                    []
           )
