module Search.Component exposing (SearchModel, searchInit, breadthFirstSearch, depthFirstSearch)

{-| Search algorithms exposing their internal model

This module is useful if you want to work with the internal model of the search algorithms from the `Search` module.

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

@docs SearchModel, searchInit, breadthFirstSearch, depthFirstSearch

-}

import Process
import Search exposing (Node, Queue, SearchProblem, addFirst, addLast, newUnexploredFrontier)
import Set exposing (Set)
import Task


{-| This record represents the inner state of the search algorithm. You can integrate it into the model of your web application.

The `state` parameter refers to the `State` type of the search problem. For example, if you want to search an eight-puzzle, you can import it with `import Search.EightPuzzle exposing (State)`.

Initialize your model with `searchInit` (see below).

-}
type alias SearchModel state =
    { problem : SearchProblem state
    , explored : Set state
    , frontier : List (Node state)
    , solution : Maybe (Maybe (Node state))
    }

{-|
Initialize your model of the search algorithm with this function. It takes a `Searchproblem state` as parameter, because it needs to know the `initialState` of the search problem for initializing the frontier, and also the whole other information about the search problem for running the search algorithm later.
-}
searchInit : SearchProblem state -> SearchModel state
searchInit problem =
    { problem = problem
    , explored = Set.empty
    , frontier =
        [ { parent = Nothing
          , state = problem.initialState
          , pathCost = 0
          }
        ]
    , solution = Nothing
    }


pollUnexploredFrontier :
    Queue (Node comparable)
    -> SearchModel comparable
    -> SearchModel comparable
pollUnexploredFrontier queue searchModel =
    case searchModel.frontier of
        h :: t ->
            if searchModel.problem.goalTest h.state then
                { searchModel | frontier = t, solution = Just (Just h) }

            else
                { searchModel
                    | explored = Set.insert h.state searchModel.explored
                    , frontier = newUnexploredFrontier h t searchModel.explored queue searchModel.problem
                }

        [] ->
            { searchModel | solution = Just Nothing }


search :
    (SearchModel comparable -> SearchModel comparable)
    -> (( SearchModel comparable, SearchModel comparable -> Cmd msg ) -> msg)
    -> SearchModel comparable
    -> Cmd msg
search stepMethod msg searchModel =
    Task.perform
        msg
        (Process.sleep 0
            |> Task.andThen (\_ -> ( breadthFirstSearchStep searchModel, search stepMethod msg ) |> Task.succeed)
        )


breadthFirstSearchStep : SearchModel comparable -> SearchModel comparable
breadthFirstSearchStep =
    pollUnexploredFrontier addLast


{-| Run a breadth-first search on your `SearchModel`. This will send a message with an intermediate result and a callback after one execution step and stop then.

In your application, you need to create the message that deals with this:

    type Msg
        = FinishedStep ( SearchModel State, SearchModel State -> Cmd Msg )
        | NoOp

In this case we called the message `SearchOn`. We need to pass this message to the search algorithm as the first parameter. This is so the algorithm knows what to do with its intermediate result. So, if our model of the search algorith is just called `model`, we call this function in the following way:

    breadthFirstSearch FinishedStep model

This returns a command, so put it somewhere inside the `update` function of your application, for example one that is called on the click of a start button.

Now we also need an update function to deal with the message we're sending. If 

All this only executes one step! This may be fine. For example, you may have a step button th

-}
breadthFirstSearch :
    (( SearchModel comparable, SearchModel comparable -> Cmd msg ) -> msg)
    -> SearchModel comparable
    -> Cmd msg
breadthFirstSearch =
    search breadthFirstSearchStep


depthFirstSearchStep : SearchModel comparable -> SearchModel comparable
depthFirstSearchStep =
    pollUnexploredFrontier addFirst


depthFirstSearch :
    (( SearchModel comparable, SearchModel comparable -> Cmd msg ) -> msg)
    -> SearchModel comparable
    -> Cmd msg
depthFirstSearch =
    search depthFirstSearchStep
