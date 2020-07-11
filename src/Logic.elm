module Logic exposing (..)

{-| Convenience functions for working with logical expressions


# Propositional Logic

@docs not, and, or, eitherOr, implies, equals, contradicts


# Predicate Logic [First-Order Logic]

Elm's native features are suitable for doing Predicate Logic:

    -- F = {(1, "one"), (2, "two"), (3, "three")}
    f a = List.member a [ (1, "one"), (2, "two"), (3, "three") ]
    --> <function> : ( number, String ) -> Bool

    -- for all x: F(x)
    List.all f
    --> <function> : List ( number, String ) -> Bool

    -- for all x: x or False
    List.all (\x -> f x || false)
    --> <function> : List ( number, String ) -> Bool

    -- for all x in D = {1, 2, 3}: F(x, "one")
    d = [1, 2, 3]
    List.all (\x -> f (x, "one")) d
    --> False

    -- exists x in D = {1, 2, 3}: F(x, "one")
    List.any (\x -> f (x, "one")) d
    --> True

Nesting unspecified domains is not possible.

    -- forall x in D: exists y: F(x, y)
    -- List.all (\x -> List.any (\y -> f (x, y))) d
    --> Syntax error!
    -- forall x in D: exists y **in {"one", "two", "three"}: F(x, y)
    -- List.all (\x -> List.any (\y -> f (x, y)) ["one", "two", "three"]) d
    --> True




# Modal Logic


## Necessity

@docs necessary, necessary2, necessary3, necessary4, necessary5


## Possibility

@docs possible, possible2, possible3, possible4, possible5


## Contingency

@docs contingent, contingent2, contingent3, contingent4, contingent5


## Forcing

@docs force, force2, force3, force4, force5

-}

-- Propositional Logic


{-| Alias of Elm's built-in `not`.

    not True
    --> False

    not False
    --> True

-}
not : Bool -> Bool
not a =
    not a


{-| Prefix notation alias for `&&`.

    and True True
    --> True

    and True False
    --> False

    and False True
    --> False

    and False False
    --> True

-}
and : Bool -> Bool -> Bool
and a b =
    a && b


{-| Prefix notation alias for `||`.

    or True True
    --> True

    or True False
    --> True

    or False True
    --> True

    or False False
    --> True

-}
or : Bool -> Bool -> Bool
or a b =
    a || b


{-| Exclusive logical `or`, that is `either one but not both`.

    eitherOr True True
    --> False

    eitherOr True False
    --> True

    eitherOr False True
    --> True

    eitherOr False False
    --> False

-}
eitherOr : Bool -> Bool -> Bool
eitherOr a b =
    (a && b) || (not a && not b)


{-| Material implication [subjunction, conditional].

    implies True True
    --> True

    implies True False
    --> False

    implies False True
    --> True

    implies False False
    --> True

-}
implies : Bool -> Bool -> Bool
implies a b =
    b || not a


{-| Just a prefix notation alias for the boolean equivalence `==`.

    equals True True
    --> True

    equals True False
    --> False

    equals False True
    --> False

    equals False False
    --> True

-}
equals : Bool -> Bool -> Bool
equals a b =
    a == b


{-| Logical contradiction.

    contradicts True True
    --> False

    contradicts True False
    --> True

    contradicts False True
    --> True

    contradicts False False
    --> False

-}
contradicts : Bool -> Bool -> Bool
contradicts a b =
    a /= b



-- Modal Logic


{-| Checks whether a logical expression with one open variable (`Bool -> Bool`) is true in all possible worlds.

    necessary (\a -> a || not a)
    --> True

    necessary (\a -> True)
    --> False

    necessary (\a -> False)
    --> False

    necessary (\a -> a)
    --> False

    necessary (\a -> not a)
    --> False

-}
necessary : (Bool -> Bool) -> Bool
necessary a =
    List.all a [ True, False ]


{-| Checks whether a logical expression with two open variables (`Bool -> Bool -> Bool`) is true in all possible worlds.

    necessary2 (\a b -> implies a (a || b))
    --> True

    necessary2 (\a b -> False || b)
    --> False

-}
necessary2 : (Bool -> Bool -> Bool) -> Bool
necessary2 a =
    necessary (necessary << a)


{-| Checks whether a logical expression with three open variables (`Bool -> Bool -> Bool -> Bool`) is true in all possible worlds.

    necessary3 (\a b c -> not (a && b && c && (not b)))
    --> True

    necessary2 (\a b c -> implies a c)
    --> False

-}
necessary3 : (Bool -> Bool -> Bool -> Bool) -> Bool
necessary3 a =
    necessary (necessary2 << a)


necessary4 : (Bool -> Bool -> Bool -> Bool -> Bool) -> Bool
necessary4 a =
    necessary (necessary3 << a)


necessary5 : (Bool -> Bool -> Bool -> Bool -> Bool -> Bool) -> Bool
necessary5 a =
    necessary (necessary4 << a)


{-| Checks whether a logical expression with one open variable (`Bool -> Bool`) is true in at least one possible world.

    possible (\a -> not a)
    --> True

    possible (\a -> a && (not a))
    --> False

-}
possible : (Bool -> Bool) -> Bool
possible a =
    List.any a [ True, False ]


{-| Checks whether a logical expression with two open variables (`Bool -> Bool -> Bool`) is true in at least one possible world.

    possible (\a b -> a && b)
    --> True

    possible (\a b -> contradicts a b && equals a b)
    --> False

-}
possible2 : (Bool -> Bool -> Bool) -> Bool
possible2 a =
    possible (possible << a)


{-| Checks whether a logical expression with three open variables (`Bool -> Bool -> Bool -> Bool`) is true in at least one possible world.

    possible3 (\a b c -> contradicts (equals a b) (c && True))
    --> True

    possible3 (\a b c -> equals a b && equals b c && equals c (not a))
    --> False

-}
possible3 : (Bool -> Bool -> Bool -> Bool) -> Bool
possible3 a =
    possible (possible2 << a)


possible4 : (Bool -> Bool -> Bool -> Bool -> Bool) -> Bool
possible4 a =
    possible (possible3 << a)


possible5 : (Bool -> Bool -> Bool -> Bool -> Bool -> Bool) -> Bool
possible5 a =
    possible (possible4 << a)


{-| Checks whether a logical expression with one open variable (`Bool -> Bool`) is true in at least one possible world and false in at least one possible world.

    contingent (\a -> a || a)
    --> True

    contingent (\a -> True)
    --> False

    contingent (\a -> contradicts a a)
    --> False

-}
contingent : (Bool -> Bool) -> Bool
contingent a =
    List.any a [ True, False ] && List.any (not << a) [ True, False ]


{-| Checks whether a logical expression with two open variables (`Bool -> Bool -> Bool`) is true in at least one possible world and false in at least one possible world.

    contingent2 (\a b -> contradicts a b)
    --> True

    contingent2 (\a b -> False)
    --> False

-}
contingent2 : (Bool -> Bool -> Bool) -> Bool
contingent2 a =
    contingent (contingent << a)


{-| Checks whether a logical expression with three open variables (`Bool -> Bool -> Bool -> Bool`) is true in at least one possible world and false in at least one possible world.

    contingent3 (\a b c -> or a (and b c))
    --> True

    contingent3 (\a b c -> True)
    --> False

-}
contingent3 : (Bool -> Bool -> Bool -> Bool) -> Bool
contingent3 a =
    contingent (contingent2 << a)


contingent4 : (Bool -> Bool -> Bool -> Bool -> Bool) -> Bool
contingent4 a =
    contingent (contingent3 << a)


contingent5 : (Bool -> Bool -> Bool -> Bool -> Bool -> Bool) -> Bool
contingent5 a =
    contingent (contingent4 << a)


{-| Evaluates a logical expression with one open variable (`Bool -> Bool`) as either necessarily true (`Just True`) or necessarily false (`Just False`) or contingent (`Nothing`).

    force (\a -> True)
    --> Just True

    force (\a -> a || not a)
    --> Just True

    force (\a -> False)
    --> Just False

    force (\a -> contradicts a a)
    --> Just False

    force (\a -> a || a)
    --> Nothing

-}
force : (Bool -> Bool) -> Maybe Bool
force a =
    if necessary a then
        Just True

    else if not (possible a) then
        Just False

    else
        Nothing


{-| Evaluates a logical expression with two open variables (`Bool -> Bool -> Bool`) as either necessarily true (`Just True`) or necessarily false (`Just False`) or contingent (`Nothing`).

    force2 (\a b -> implies a b || implies b a)
    --> Just True

    force2 (\a b -> False)
    --> Just False

    force2 (\a b -> implieas a b)
    --> Nothing

-}
force2 : (Bool -> Bool -> Bool) -> Maybe Bool
force2 a =
    if necessary2 a then
        Just True

    else if not (possible2 a) then
        Just False

    else
        Nothing


{-| Evaluates a logical expression with two open variables (`Bool -> Bool -> Bool -> Bool`) as either necessarily true (`Just True`) or necessarily false (`Just False`) or contingent (`Nothing`).

    force3 (\a b c -> c || True)
    --> Just True

    force3 (\a b c -> not True)
    --> Just False

    force 3 (\a b c -> b)
    --> Nothing

-}
force3 : (Bool -> Bool -> Bool -> Bool) -> Maybe Bool
force3 a =
    if necessary3 a then
        Just True

    else if not (possible3 a) then
        Just False

    else
        Nothing


force4 : (Bool -> Bool -> Bool -> Bool -> Bool) -> Maybe Bool
force4 a =
    if necessary4 a then
        Just True

    else if not (possible4 a) then
        Just False

    else
        Nothing


force5 : (Bool -> Bool -> Bool -> Bool -> Bool -> Bool) -> Maybe Bool
force5 a =
    if necessary5 a then
        Just True

    else if not (possible5 a) then
        Just False

    else
        Nothing
