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

However, these algorithms are much slower than their equivalents from the `Search` module. In my opinion they should have the same time efficiency as the algorithms from the `Search` module, but I have to investigate this further, as the algorithms here seem to slow down pretty quickly.

You can use the functions from this module to embed the inner structure of the search algorithm (the `SearchModel`) into the model of your web application.

Here is a minimal example, which you can also find in the [`Search/ComponentMinimal`](../../../../examples/Search/ComponentMinimal/src/Main.elm) example. A logging use case can be found in the [`Search/Component`](../../../../examples/Search/Component/src/Main.elm) example.

    module Main exposing (..)

    import Browser
    import Html exposing (text)
    import Search.Component exposing (SearchModel, breadthFirstSearch, searchInit)
    import Search.NPuzzle exposing (simpleEightPuzzle)

    type alias State =
        List Int

    type Msg
        = SearchOn ( SearchModel State, SearchModel State -> Cmd Msg )

    main : Program () (SearchModel State) Msg
    main =
        let
            searchModel =
                searchInit simpleEightPuzzle
        in
        Browser.element
            { view = \model -> text (Debug.toString model)
            , init = \_ -> ( searchModel, breadthFirstSearch SearchOn searchModel )
            , update =
                \msg model ->
                    case msg of
                        SearchOn ( result, callback ) ->
                            ( result, callback result )
            , subscriptions = always Sub.none
            }

In this example, the model of the application _is_ the model of the search algorithm. In a real scenario, it would most likely reside as a sub-model within the application model. You can look that up in the [`Search/Component`](../../../../examples/Search/Component/src/Main.elm) example.

@docs treeSearchStep, graphSearchStep

    searchAsync :
        (SearchModel comparable -> SearchModel comparable)
        -> (( SearchModel comparable, SearchModel comparable -> Cmd msg ) -> msg)
        -> SearchModel comparable
        -> Cmd msg
    searchAsync stepMethod msg searchModel =
        Task.perform
            msg
            (Process.sleep 0
                |> Task.andThen
                    (\_ -> Task.succeed ( breadthFirstSearchStep searchModel, searchAsync stepMethod msg ))
            )

-}

import Dict exposing (Dict)
import Graph
import List.Extra as List
import Search.Problem as Problem exposing (Node, expand, path)


type alias Problem a =
    Problem.Problem a



-- QUEUES


type alias Queue a =
    { pop : List a -> Maybe ( a, List a ) }


{-| simulates a First-In-First-Out queue when using `::` for insertion
-}
fifo : Queue a
fifo =
    { pop = List.unconsLast }


{-| simulates a Last-In-First-Out queue when using `::` for insertion
-}
lifo : Queue a
lifo =
    { pop = List.uncons }


{-| simulates a priority queue when using `::` for insertion
-}
priority : (a -> comparable) -> Queue a
priority f =
    { pop =
        \l ->
            List.minimumBy f l
                |> Maybe.map (\a -> ( a, List.remove a l ))
    }



-- STRATEGIES


type alias Strategy a =
    { frontier :
        Dict (List ( Float, a )) (List ( Float, a ))
        -> Node a
        -> List (Node a)
        -> List (Node a)
        -> List (Node a)
    }


treeSearch : Strategy a
treeSearch =
    { frontier = \_ _ t childNodes -> childNodes ++ t }


{-| Ensures states are not explored twice and always at the lowest known path cost.
-}
graphSearch : Strategy a
graphSearch =
    { frontier =
        \explored h t childNodes ->
            -- only add child node if
            -- a) their state is not the same as their parent's and
            -- b) their state is not in a sibling node with a lower path cost
            -- c) their state is not already explored and
            -- d) their state is not already in the frontier with a lower path cost
            (childNodes
                |> List.filter
                    (\newNode ->
                        not
                            (newNode.state == h.state)
                            && not
                                (List.any
                                    (\otherNewNode ->
                                        newNode.state
                                            == otherNewNode.state
                                            && newNode.pathCost
                                            < otherNewNode.pathCost
                                    )
                                    childNodes
                                )
                            && not
                                (List.any
                                    (\exploredNode ->
                                        Just newNode.state
                                            == Maybe.map Tuple.second (List.head exploredNode)
                                    )
                                    (Dict.keys explored)
                                )
                            && (case List.find (\node -> node.state == newNode.state) t of
                                    Just node ->
                                        newNode.pathCost < node.pathCost

                                    Nothing ->
                                        True
                               )
                    )
            )
                -- if a child node's state is already in the frontier but with a higher pathCost, remove it
                ++ (t
                        |> List.filter
                            (\node ->
                                case List.find (\newNode -> node.state == newNode.state) childNodes of
                                    Just newNode ->
                                        newNode.pathCost >= node.pathCost

                                    Nothing ->
                                        True
                            )
                   )
    }



-- MODEL


type Result a
    = Pending
    | Solution (Node a)
    | Failure


{-| This record represents the inner state of the search algorithm. You can integrate it into the model of your web application.

The `state` parameter refers to the `State` type of the search problem. For example, if you want to search an eight-puzzle, you can import it with `import Search.EightPuzzle exposing (State)`.

Initialize your model with `searchInit` (see below).

-- technically it would suffice to store only explored states and not their children
-- but there is no noticeable difference in performance (TODO benchmarks)
-- and having the children is useful for performant visualization, where we want to reconstruct the search tree

-}
type alias Model a =
    { strategy : Strategy a
    , queue : Queue (Node a)
    , problem : Problem a
    , explored : Dict (List ( Float, a )) (List ( Float, a ))
    , frontier : List (Node a)
    , solution : Result a
    , maxPathCost : Float
    }


{-| Initializes your model of the search algorithm. It takes a `Problem state` as parameter, because it needs to know the `initialState` of the search problem for initializing the frontier, and also the whole other information about the search problem for running the search algorithm later.
-}
init :
    Strategy comparable
    -> Queue (Node comparable)
    -> Problem comparable
    -> Model comparable
init strategy queue problem =
    { strategy = strategy
    , queue = queue
    , problem = problem
    , explored = Dict.empty
    , frontier =
        [ { state = problem.initialState
          , parent = Nothing
          , pathCost = 0
          }
        ]
    , solution = Pending
    , maxPathCost = 0
    }


searchStep :
    Strategy comparable
    -> Queue (Node comparable)
    -> Model comparable
    -> Model comparable
searchStep strategy queue ({ explored } as model) =
    case queue.pop model.frontier of
        Just ( h, t ) ->
            let
                childNodes =
                    expand model.problem h
            in
            case List.find (\node -> model.problem.goalTest node.state) childNodes of
                Just a ->
                    { model
                        | solution = Solution a
                        , frontier = t
                        , explored = Dict.insert (path h) [ ( a.pathCost, a.state ) ] explored
                        , maxPathCost = max model.maxPathCost a.pathCost
                    }

                Nothing ->
                    { model
                        | frontier = strategy.frontier explored h t childNodes
                        , explored =
                            Dict.insert (path h)
                                (childNodes |> List.map (\node -> ( node.pathCost, node.state )))
                                explored
                        , maxPathCost = List.foldl max model.maxPathCost (List.map .pathCost childNodes)
                    }

        Nothing ->
            { model | solution = Failure }



-- STEPPERS


next : Model comparable -> Model comparable
next model =
    searchStep model.strategy model.queue model


nextN : Int -> Model comparable -> Model comparable
nextN n model =
    if n > 0 then
        searchStep model.strategy model.queue model |> nextN (n - 1)

    else
        model


nextGoal : Model comparable -> ( Maybe (Node comparable), Model comparable )
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


breadthFirst : Problem comparable -> Model comparable
breadthFirst =
    init graphSearch fifo


depthFirst : Problem comparable -> Model comparable
depthFirst =
    init graphSearch lifo


{-| Dijkstra's algorithm.
-}
uniformCost : Problem comparable -> Model comparable
uniformCost =
    init graphSearch (priority (\node -> node.pathCost))


greedy : Problem comparable -> Model comparable
greedy problem =
    init graphSearch
        (priority (\node -> problem.heuristic node.state))
        problem


{-| A\* search.
-}
bestFirst : Problem comparable -> Model comparable
bestFirst problem =
    init
        graphSearch
        (priority (\node -> node.pathCost + problem.heuristic node.state))
        problem
