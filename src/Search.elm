module Search exposing (..)

import List.Extra as List
import Set exposing (Set)


{-| This deviates from the AIMA book, where there are distinct `actions`, `result` and `stepCost` functions. I find a pure graph model more suitable, so there is a single function (called `actions`) which for each state reveals a list of tuples containing adjacent states and their respective step costs.
-}
type alias SearchProblem a =
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


expand : SearchProblem a -> Node a -> List (Node a)
expand problem node =
    List.map
        (\( stepCost, result ) ->
            { state = result
            , parent = Just (Parent node)
            , pathCost = node.pathCost + stepCost
            }
        )
        (problem.actions node.state)


type alias Queue a =
    a -> List a -> List a


{-| simulates a First-In-First-Out queue when using head for extraction
-}
addLast : Queue a
addLast a l =
    l ++ [ a ]


{-| simulates a Last-In-First-Out queue when using head for extraction
-}
addFirst : Queue a
addFirst a l =
    a :: l


{-| simulates a priority queue when using head for extraction
-- TODO more efficient implementation / show this is most efficient
-}
addByPriority : (a -> comparable) -> Queue a
addByPriority p a l =
    List.sortBy p (a :: l)


type alias FrontierWorker a =
    Queue (Node a)
    -> SearchProblem a
    -> Set a
    -> List (Node a)
    -> Maybe (Node a)


pollFrontier : FrontierWorker comparable
pollFrontier queue problem explored frontier =
    case frontier of
        h :: t ->
            if problem.goalTest h.state then
                Just h

            else
                pollFrontier
                    queue
                    problem
                    (Set.insert h.state explored)
                    (List.foldl queue t (expand problem h))

        [] ->
            Nothing


type alias Search a =
    SearchProblem a -> Maybe (Node a)


search : FrontierWorker a -> Queue (Node a) -> Search a
search frontierWorker queue problem =
    frontierWorker
        queue
        problem
        Set.empty
        [ { state = problem.initialState
          , parent = Nothing
          , pathCost = 0.0
          }
        ]


treeSearch : Queue (Node comparable) -> Search comparable
treeSearch queue problem =
    search pollFrontier queue problem


breadthFirstTreeSearch : Search comparable
breadthFirstTreeSearch =
    treeSearch addLast


depthFirstTreeSearch : Search comparable
depthFirstTreeSearch =
    treeSearch addFirst


{-| TODO optimize
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
    -> Set comparable
    -> Queue (Node comparable)
    -> SearchProblem comparable
    -> List (Node comparable)
newUnexploredFrontier h t explored queue problem =
    let
        childNodes =
            expand problem h
    in
    childNodes
        |> List.filter
            (\a ->
                not
                    ((a.state == h.state)
                        || Set.member a.state explored
                        || List.any (\b -> a.state == b.state) t
                    )
            )
        |> List.foldl
            queue
            (updatePathCosts childNodes t)


pollUnexploredFrontier : FrontierWorker comparable
pollUnexploredFrontier queue problem explored frontier =
    case frontier of
        h :: t ->
            if problem.goalTest h.state then
                Just h

            else
                pollUnexploredFrontier
                    queue
                    problem
                    (Set.insert h.state explored)
                    (newUnexploredFrontier h t explored queue problem)

        [] ->
            Nothing


graphSearch : Queue (Node comparable) -> Search comparable
graphSearch queue problem =
    search pollUnexploredFrontier queue problem


breadthFirstSearch : Search comparable
breadthFirstSearch =
    graphSearch addLast


depthFirstSearch : Search comparable
depthFirstSearch =
    graphSearch addFirst
