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

import List.Extra as List
import Set exposing (Set)


{-| This deviates from the AIMA book, where there are distinct `actions`, `result` and `stepCost` functions. I find a pure graph model more suitable, so there is a single function (called `actions`) which for each state reveals a list of tuples containing adjacent states and their respective step costs.
-}
type alias Problem a =
    { initialState : a
    , actions : a -> List ( Float, a )
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
            path p ++ [ ( node.pathCost, node.state ) ]

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


type alias QueueInserter a =
    a -> List a -> List a


{-| simulates a First-In-First-Out queue when using head for extraction
-}
insertLast : QueueInserter a
insertLast a l =
    l ++ [ a ]


{-| simulates a Last-In-First-Out queue when using head for extraction
-}
insertFirst : QueueInserter a
insertFirst a l =
    a :: l


{-| simulates a priority queue when using head for extraction
-- TODO more efficient implementation / show this is most efficient
-}
insertByPriority : (a -> comparable) -> QueueInserter a
insertByPriority p a l =
    List.sortBy p (a :: l)


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
    , queue : QueueInserter (Node state)
    , explored : Set state
    , frontier : List (Node state)
    , solution : Solution state
    }


type alias Step state =
    Model state
    -> Model state


treeSearchStep : Step comparable
treeSearchStep searchModel =
    case searchModel.frontier of
        h :: t ->
            let
                childNodes =
                    expand searchModel.problem h
            in
            case List.find (\node -> searchModel.problem.goalTest node.state) childNodes of
                Just a ->
                    { searchModel | solution = Solution a }

                Nothing ->
                    { searchModel | frontier = List.foldl searchModel.queue t childNodes }

        [] ->
            { searchModel | solution = NoSolution }


{-| Updates the 2nd list with path costs from the 1st list where the path costs are shorter.
TODO optimize
-}
updatePathCosts : List (Node a) -> List (Node a) -> List (Node a)
updatePathCosts l1 l2 =
    List.map
        (\a ->
            case List.find (\b -> a.state == b.state) l1 of
                Just b ->
                    { a | pathCost = min a.pathCost b.pathCost }

                Nothing ->
                    a
        )
        l2


newUnexploredFrontier :
    Node comparable
    -> List (Node comparable)
    -> List (Node comparable)
    -> Model comparable
    -> List (Node comparable)
newUnexploredFrontier h t childNodes searchModel =
    childNodes
        |> List.filter
            (\a ->
                not
                    ((a.state == h.state)
                        || Set.member a.state searchModel.explored
                        || List.any (\b -> a.state == b.state) t
                    )
            )
        |> List.foldl
            searchModel.queue
            (updatePathCosts childNodes t)


graphSearchStep : Model comparable -> Model comparable
graphSearchStep searchModel =
    case searchModel.frontier of
        h :: t ->
            let
                childNodes =
                    expand searchModel.problem h
            in
            case List.find (\node -> searchModel.problem.goalTest node.state) childNodes of
                Just a ->
                    { searchModel | frontier = t, solution = Solution a }

                Nothing ->
                    { searchModel
                        | explored = Set.insert h.state searchModel.explored
                        , frontier = newUnexploredFrontier h t childNodes searchModel
                    }

        [] ->
            { searchModel | solution = NoSolution }


{-| Initializes your model of the search algorithm. It takes a `Problem state` as parameter, because it needs to know the `initialState` of the search problem for initializing the frontier, and also the whole other information about the search problem for running the search algorithm later.
-}
init : QueueInserter (Node state) -> Problem state -> Model state
init queue problem =
    { problem = problem
    , queue = queue
    , explored = Set.empty
    , frontier =
        [ { parent = Nothing
          , state = problem.initialState
          , pathCost = 0
          }
        ]
    , solution = Pending
    }


search : Step a -> Model a -> ( Maybe (Node a), Model a )
search searchStep searchModel =
    let
        newSearchModel =
            searchStep searchModel
    in
    case newSearchModel.solution of
        Solution a ->
            ( Just a, newSearchModel )

        NoSolution ->
            ( Nothing, newSearchModel )

        Pending ->
            search searchStep newSearchModel


treeSearch : Model comparable -> ( Maybe (Node comparable), Model comparable )
treeSearch searchModel =
    search treeSearchStep searchModel


breadthFirstTreeSearch : Problem comparable -> ( Maybe (Node comparable), Model comparable )
breadthFirstTreeSearch problem =
    treeSearch (init insertLast problem)


depthFirstTreeSearch : Problem comparable -> ( Maybe (Node comparable), Model comparable )
depthFirstTreeSearch problem =
    treeSearch (init insertFirst problem)


graphSearch : Model comparable -> ( Maybe (Node comparable), Model comparable )
graphSearch searchModel =
    search graphSearchStep searchModel


breadthFirstSearch : Problem comparable -> ( Maybe (Node comparable), Model comparable )
breadthFirstSearch problem =
    graphSearch (init insertLast problem)


depthFirstSearch : Problem comparable -> ( Maybe (Node comparable), Model comparable )
depthFirstSearch problem =
    graphSearch (init insertFirst problem)
