-- GAMES


maximin =
    [ test "fills in, recognizes utility 0" <|
        \_ ->
            Expect.equal
                (minimaxDecision ticTacToe
                    ("""
                    xxo
                    oox
                    xo 
                    """
                        |> String.replace "    " ""
                        |> Game.TicTacToe.stateFromString
                    )
                    Nothing
                    |> Maybe.map (\( u, s ) -> ( u, Game.TicTacToe.stateToString s |> addSpaces 20 ))
                )
                (Just
                    ( 0
                    , """
                    xxo
                    oox
                    xox
                    """
                    )
                )
    , test "fills in, recognizes utility 1" <|
        \_ ->
            Expect.equal
                (minimaxDecision ticTacToe
                    ("""
                    xx 
                    oo 
                       
                    """
                        |> String.replace "    " ""
                        |> Game.TicTacToe.stateFromString
                    )
                    Nothing
                    |> Maybe.map (\( u, s ) -> ( u, Game.TicTacToe.stateToString s |> addSpaces 20 ))
                )
                (Just
                    ( 1
                    , """
                    xxx
                    oo 
                       
                    """
                    )
                )
    , test "fills in, recognizes utility -1" <|
        \_ ->
            Expect.equal
                (minimaxDecision ticTacToe
                    ("""
                    xxo
                    oo 
                      x
                    """
                        |> String.replace "    " ""
                        |> Game.TicTacToe.stateFromString
                    )
                    Nothing
                    |> Maybe.map (\( u, s ) -> ( u, Game.TicTacToe.stateToString s |> addSpaces 20 ))
                )
                (Just
                    ( -1
                    , """
                    xxo
                    oox
                      x
                    """
                    )
                )
    ]