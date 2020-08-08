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

-- TODO also check eack child node against the other child nodes

import Dict exposing (Dict)
import Html exposing (a)
import List.Extra as List


{-| This deviates from the AIMA book, where there are distinct `actions`, `result` and `stepCost` functions. I find a pure graph model more suitable, so there is a single function (called `actions`) which for each state reveals a list of tuples containing adjacent states and their respective step costs.
-}
type alias Problem a =
    { initialState : a
    , actions : a -> List ( Float, a )
    , heuristic : a -> Float
    , goalTest : a -> Bool
    }


type alias Node a =
    { state : a
    , parent : Maybe (Parent a)
    , pathCost : Float
    }


type Parent a
    = Parent (Node a)


path : Node a -> List ( Float, a )
path node =
    case node.parent of
        Just (Parent p) ->
            ( node.pathCost, node.state ) :: path p

        Nothing ->
            [ ( node.pathCost, node.state ) ]


expand : Problem a -> Node a -> List (Node a)
expand problem node =
    List.map
        (\( stepCost, result ) ->
            { state = result
            , parent = Just (Parent node)
            , pathCost = node.pathCost + stepCost
            }
        )
        (problem.actions node.state)


type alias QueueFetcher a =
    List a -> Maybe ( a, List a )


{-| simulates a First-In-First-Out queue when using `::` for insertion
-}
fifoFetch : QueueFetcher a
fifoFetch =
    List.unconsLast


{-| simulates a Last-In-First-Out queue when using `::` for insertion
-}
lifoFetch : QueueFetcher a
lifoFetch =
    List.uncons


{-| simulates a priority queue when using `::` for insertion
-- TODO more efficient implementation / show this is most efficient
-}
priorityFetch : (a -> comparable) -> QueueFetcher a
priorityFetch f l =
    let
        min =
            List.minimumBy f l
    in
    Maybe.map (\m -> ( m, List.remove m l )) min


argmin : (a -> comparable) -> a -> a -> a
argmin f a b =
    if f a < f b then
        b

    else
        a


type alias FrontierWorker a =
    Node a
    -> List (Node a)
    -> List (Node a)
    -> Model a
    -> List (Node a)


newSimpleFrontier : FrontierWorker comparable
newSimpleFrontier _ t childNodes _ =
    childNodes ++ t


{-| Ensures states are not explored twice and always at the lowest known path cost.
-}
newUnexploredFrontier : FrontierWorker comparable
newUnexploredFrontier h t childNodes model =
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
                            (Dict.keys model.explored)
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


type Solution state
    = Pending
    | Solution (Node state)
    | NoSolution


{-| This record represents the inner state of the search algorithm. You can integrate it into the model of your web application.

The `state` parameter refers to the `State` type of the search problem. For example, if you want to search an eight-puzzle, you can import it with `import Search.EightPuzzle exposing (State)`.

Initialize your model with `searchInit` (see below).

-}
type alias Model state =
    { problem : Problem state
    , queueFetch : QueueFetcher (Node state)

    -- technically it would suffice to store only explored states and not their children
    -- but there is no noticeable difference in performance (TODO  benchmarks)
    -- and having the children is useful for performant visualization, where we want to reconstruct the search tree
    , explored : Dict (List ( Float, state )) (List ( Float, state ))
    , frontier : List (Node state)
    , solution : Solution state
    , maxPathCost : Float
    }


type alias Step state =
    Model state
    -> Model state


searchStep : FrontierWorker comparable -> Step comparable
searchStep frontierWorker ({ explored } as model) =
    case model.queueFetch model.frontier of
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
                        | frontier = frontierWorker h t childNodes model
                        , explored =
                            Dict.insert (path h)
                                (childNodes |> List.map (\node -> ( node.pathCost, node.state )))
                                explored
                        , maxPathCost = List.foldl max model.maxPathCost (List.map .pathCost childNodes)
                    }

        Nothing ->
            { model | solution = NoSolution }


treeSearchStep : Step comparable
treeSearchStep =
    searchStep newSimpleFrontier


graphSearchStep : Step comparable
graphSearchStep =
    searchStep newUnexploredFrontier


{-| Initializes your model of the search algorithm. It takes a `Problem state` as parameter, because it needs to know the `initialState` of the search problem for initializing the frontier, and also the whole other information about the search problem for running the search algorithm later.
-}
init : QueueFetcher (Node state) -> Problem state -> Model state
init queue problem =
    { problem = problem
    , queueFetch = queue
    , explored = Dict.empty
    , frontier = [ makeRootNode problem.initialState ]
    , solution = Pending
    , maxPathCost = 0
    }


makeRootNode : state -> Node state
makeRootNode state =
    { state = state
    , parent = Nothing
    , pathCost = 0
    }


search : Step a -> Model a -> ( Maybe (Node a), Model a )
search step model =
    let
        newModel =
            step model
    in
    case newModel.solution of
        Solution a ->
            ( Just a, newModel )

        NoSolution ->
            ( Nothing, newModel )

        Pending ->
            search step newModel


treeSearch : Model comparable -> ( Maybe (Node comparable), Model comparable )
treeSearch searchModel =
    search treeSearchStep searchModel


breadthFirstTreeSearch : Problem comparable -> ( Maybe (Node comparable), Model comparable )
breadthFirstTreeSearch problem =
    treeSearch (init fifoFetch problem)


depthFirstTreeSearch : Problem comparable -> ( Maybe (Node comparable), Model comparable )
depthFirstTreeSearch problem =
    treeSearch (init lifoFetch problem)


uniformCostTreeSearch : Problem comparable -> ( Maybe (Node comparable), Model comparable )
uniformCostTreeSearch problem =
    treeSearch (init (priorityFetch .pathCost) problem)


greedyTreeSearch : Problem comparable -> ( Maybe (Node comparable), Model comparable )
greedyTreeSearch problem =
    treeSearch (init (priorityFetch (\node -> problem.heuristic node.state)) problem)


{-| A\* tree search.
-}
heuristicTreeSearch : Problem comparable -> ( Maybe (Node comparable), Model comparable )
heuristicTreeSearch problem =
    treeSearch (init (priorityFetch (\node -> node.pathCost + problem.heuristic node.state)) problem)


graphSearch : Model comparable -> ( Maybe (Node comparable), Model comparable )
graphSearch searchModel =
    search graphSearchStep searchModel


breadthFirstSearch : Problem comparable -> ( Maybe (Node comparable), Model comparable )
breadthFirstSearch problem =
    graphSearch (init fifoFetch problem)


depthFirstSearch : Problem comparable -> ( Maybe (Node comparable), Model comparable )
depthFirstSearch problem =
    graphSearch (init lifoFetch problem)


uniformCostSearch : Problem comparable -> ( Maybe (Node comparable), Model comparable )
uniformCostSearch problem =
    graphSearch (init (priorityFetch .pathCost) problem)


greedySearch : Problem comparable -> ( Maybe (Node comparable), Model comparable )
greedySearch problem =
    graphSearch (init (priorityFetch (\node -> problem.heuristic node.state)) problem)


{-| A\* search.
-}
heuristicSearch : Problem comparable -> ( Maybe (Node comparable), Model comparable )
heuristicSearch problem =
    graphSearch (init (priorityFetch (\node -> node.pathCost + problem.heuristic node.state)) problem)
