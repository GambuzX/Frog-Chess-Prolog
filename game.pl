:- include('game_state.pl').
:- include('display.pl').
:- include('input.pl').

/** 
 * Player Frog
 * playerFrog(+Number, -PlayerFrog)
 * Associates a frog to each player number.
 *
 * Number -> Number of the player. Since for now we only consider 2 players, it is either 1 or 2.
 * PlayerFrog -> Returns the frog associated with the player number.
 */
playerFrog(1, blue).
playerFrog(2, yellow).

/** 
 * Valid position
 * validPosition(+Position)
 * Checks if a given position is valid, that is, inside a 8x8 0-indexed board
 *
 * Position -> Position to check, in the format [Row, Column].
 */
validPosition([Row, Column]) :-
    Row >= 0, Row =< 7,
    Column >= 0, Column =< 7.

/**
 * Display a error message
 * errorMsg(+Msg)
 * Displays a error message on screen and fails.
 *
 * Msg -> Message to be displayed
 */
errorMsg(Msg) :-
    nl, write(Msg), nl, nl, fail.

/**
 * Initialize board
 * initBoard(-Board)
 * Creates a new board filled with frogs.
 * 
 * B -> Variable to return created board.
 */
initBoard(B) :-
    test(B).


/**
 * Get Target Row
 * getTargetRow(+Board, +InRow, -OutRow)
 * Returns the row of of index InRow from Board in OutRow.
 * Uses InRow as a counter and returns current value when it reaches 0.
 * 
 * Board -> List of rows.
 * InRow -> Target row to find.
 * OutRow -> Variable to return target row.
 */
getTargetRow([TargetRow | _], 0, TargetRow).
getTargetRow([_ | Rest], InRow, OutRow) :-
    InRow > 0,
    N is InRow-1,
    getTargetRow(Rest, N, OutRow).

/**
 * Get Value in Row
 * getValueInRow(+Row, +Col, -Value)
 * Returns the value in index Col of given Row, in the variable Value.
 * Uses Col as a counter and returns current value when it reaches 0.
 * 
 * Row -> Row to search value.
 * Col -> Target column to find.
 * Value -> Variable to return target value.
 */
getValueInRow([TargetVal | _], 0, TargetVal).
getValueInRow([_ | Rest], Col, Value) :-
    Col > 0,
    N is Col-1,
    getValueInRow(Rest, N, Value).

/**
 * Get position
 * getPosition(+Board, +Position, -Value)
 * Returns the value in position [Row, Col] in the given board.
 * 
 * Board -> List of lists representing the board.
 * Position -> Position in the board, containing the values [Row, Col].
 * Value -> Variable to return target value.
 */
getPosition(Board, [Row, Col], Value) :-
    getTargetRow(Board, Row, TargetRow),
    getValueInRow(TargetRow, Col, Value).


/**
 * Set new row
 * setNewRow(+Row, +TargetCol, +NewValue, +CurrColI, -NewRow)
 * Sets the value from Row at index TargetCol to NewValue. The rest of the row is copied.
 * Iterates over the row columns, copying them to NewRow, except in the case that it
 * finds the target column.
 * 
 * Row -> Row to iterate over.
 * TargetCol -> Target column.
 * NewValue -> New value to be added.
 * CurrColI -> Column iterator, from 0 to 7.
 * NewRow -> Returns the modified row.
 */
setNewRow(_, _, _, 8, []).

setNewRow([_ | Rest], TargetCol, NewValue, TargetCol, [NewValue | NewRow]) :-
    NextColI is TargetCol+1,
    setNewRow(Rest, TargetCol, NewValue, NextColI, NewRow).

setNewRow([CurrVal | Rest], TargetCol, NewValue, ColI, [CurrVal | NewRow]) :-
    TargetCol \= ColI, ColI >= 0, ColI =< 7,
    NextColI is ColI+1,
    setNewRow(Rest, TargetCol, NewValue, NextColI, NewRow).


/**
 * Set position helper
 * setPositionHelper(+Board, +TargetPosition, +NewValue, +CurrRowI, -NewBoard)
 * Changes the board position given by TargetPosition, [Row, Col], to the NewValue.
 * The rest of the board is copied.
 * Iterates over the board rows, copying them to the NewBoard, except in the case that
 * it finds the target row to be changed.
 * 
 * Board -> Original board to be changed.
 * TargetPosition -> Target position.
 * NewValue -> New value to be added.
 * CurrRowI -> Row iterator, from 0 to 7.
 * NewBoard -> Returns the modified board.
 */
setPositionHelper(_, _, _, 8, []).

setPositionHelper([CurrRow | Rest], [TargetRow, TargetCol], NewValue, TargetRow, [NewRow | NewBoard]) :-
    setNewRow(CurrRow, TargetCol, NewValue, 0, NewRow),
    NextRowI is TargetRow+1,
    setPositionHelper(Rest, [TargetRow, TargetCol], NewValue, NextRowI, NewBoard).

setPositionHelper([CurrRow | Rest], [TargetRow, TargetCol], NewValue, RowI, [CurrRow | NewBoard]) :-
    TargetRow \= RowI, RowI >= 0, RowI =< 7,
    NextRowI is RowI+1,
    setPositionHelper(Rest, [TargetRow, TargetCol], NewValue, NextRowI, NewBoard).

/**
 * Set position
 * setPosition(+Board, +Position, +NewValue, -NewBoard)
 * Changes the board position given by (Row, Col) to the NewValue.
 * 
 * Board -> Original board to be changed.
 * Position -> Position in the board to be changed, [Row , Column].
 * NewValue -> New value to be added.
 * NewBoard -> Returns the modified board.
 */
setPosition(Board, Pos, NewValue, NewBoard) :-
    setPositionHelper(Board, Pos, NewValue, 0, NewBoard).



/**
 * Valid jump
 * validJump(+StartPosition, -EndPosition)
 * Generates all valid jumping positions from StartPosition.
 * Jumps can be horizontal, vertical or diagonal, 2 positions from the starting one.
 * 
 * StartPosition -> Starting position.
 * EndPosition -> Ending position.
 */
validJump([SRow, SCol], [ERow, ECol]) :-
    ERow is SRow,
    ECol is SCol+2,
    validPosition([ERow, ECol]).
    
validJump([SRow, SCol], [ERow, ECol]) :-
    ERow is SRow,
    ECol is SCol-2,
    validPosition([ERow, ECol]).
    
validJump([SRow, SCol], [ERow, ECol]) :-
    ERow is SRow+2,
    ECol is SCol,
    validPosition([ERow, ECol]).
    
validJump([SRow, SCol], [ERow, ECol]) :-
    ERow is SRow-2,
    ECol is SCol,
    validPosition([ERow, ECol]).
    
validJump([SRow, SCol], [ERow, ECol]) :-
    ERow is SRow-2,
    ECol is SCol-2,
    validPosition([ERow, ECol]).
    
validJump([SRow, SCol], [ERow, ECol]) :-
    ERow is SRow+2,
    ECol is SCol-2,
    validPosition([ERow, ECol]).
    
validJump([SRow, SCol], [ERow, ECol]) :-
    ERow is SRow-2,
    ECol is SCol+2,
    validPosition([ERow, ECol]).
    
validJump([SRow, SCol], [ERow, ECol]) :-
    ERow is SRow+2,
    ECol is SCol+2,
    validPosition([ERow, ECol]).


/**
 * Middle Position
 * middlePosition(+StartPos, +EndPosition, -MidPosition )
 * Determines the position between Start and End.
 * 
 * StartPosition -> Starting position.
 * EndPosition -> Ending position.
 * MidPosition -> Position between Start and End.
 */
middlePosition([SRow,SCol], [ERow,ECol], [MRow,MCol]) :-
    MRow is (SRow+ERow)/2,
    MCol is (SCol+ECol)/2.


/**
 * Frog can jump
 * frogCanJump(+Board, +FrogPosition)
 * Checks if a frog in a given position can jump in any direction.
 * 
 * Board -> Game board.
 * FrogPosition -> Frog position.
 */
frogCanJump(Board, FrogPos) :-
    % look through all valid jumps
    validJump(FrogPos, Dest),

    % check if dest is empty
    getPosition(Board, Dest, empty),

    % determine middle position
    middlePosition(FrogPos, Dest, MidPos),

    % check if there is a frog in middle position
    getPosition(Board, MidPos, MidFrog),
    playerFrog(_, MidFrog).

/**
 * Jump
 * jump(+InputBoard, +StartPos, +MidPosition, +EndPosition, +Frog, -OutputBoard)
 * Jumps a frog from starting position to end position, updating all required cells.
 * Does not perform any validation.
 * 
 * InputBoard -> Initial board before jumping.
 * StartPosition -> Starting position.
 * MidPosition -> Position between Start and End.
 * EndPosition -> Ending position.
 * Frog -> Frog that is jumping.
 * OutputBoard -> Modified board after jumping.
 */
jump(InBoard, StartPos, MidPos, EndPos, Frog, OutBoard) :-
    
    % set middle position empty
    setPosition(InBoard, MidPos, empty, NewBoard1),

    % set initial position empty
    setPosition(NewBoard1, StartPos, empty, NewBoard2),

    % set end position to player frog
    setPosition(NewBoard2, EndPos, Frog, OutBoard).


/**
 * Read jump positions
 * readJumpPositions(+Board, +Player, -StartPos, -MidPosition, -EndPosition, -Frog)
 * Asks the user for input regarding the positions for the frog jump.
 * Asks for start position and end position, until a valid option is provided.
 * 
 * Board -> Game board.
 * Player -> Player of current turn.
 * StartPosition -> Starting position.
 * MidPosition -> Position between Start and End.
 * EndPosition -> Ending position.
 * Frog -> Frog that is jumping.
 */
readJumpPositions(Board, Player, InitPos, MidPos, EndPos, Frog) :-
    % starting position
    repeat,
        (
            readPosition('Frog to jump? ', InitPos), 
            getPosition(Board, InitPos, Frog),
            playerFrog(Player, Frog),
            frogCanJump(Board, InitPos),
            !;
            errorMsg('Invalid start position!')
        ),

    % end position
    (            
        readPosition('Position to jump? ', EndPos),
        getPosition(Board, EndPos, empty),
        validJump(InitPos, ValidPos),
        ValidPos = EndPos,
        !;
        errorMsg('Invalid jump destination')
    ),

    % check middle position for a frog
    (
        middlePosition(InitPos, EndPos, MidPos),
        getPosition(Board, MidPos, MidFrog),
        playerFrog(_, MidFrog),
        !;
        errorMsg('Frogs must jump over other frogs!')    
    ).


/**
 * Find Jumpable Frog In Row
 * jumpableFrogInRow(+Board, +Player, +Pos)
 * Checks if any frog in current Row can jump, iterating over columns.
 * 
 * Board -> Game board.
 * Player -> Player in question.
 * Pos -> Position currently checking.
 */
jumpableFrogInRow(_, _, [_,8]) :- !, fail.

jumpableFrogInRow(Board, Player, Pos) :-
    playerFrog(Player, Frog),
    getPosition(Board, Pos, Frog),
    frogCanJump(Board, Pos), !.

jumpableFrogInRow(Board, Player, [RowI, ColI]) :-
    NextColI is ColI+1,
    jumpableFrogInRow(Board, Player, [RowI, NextColI]).



/**
 * Find Jumpable Frog
 * findJumpableFrog(+Board, +Player, +RowI)
 * Searches for a frog of player Player that can jump in the board,
 * iterating over the rows.
 * 
 * Board -> Game board.
 * Player -> Player in question.
 * RowI -> Index of current row.
 */
findJumpableFrog(_, _, 8) :- !, fail.

findJumpableFrog(Board, Player, RowI) :-
    jumpableFrogInRow(Board, Player, [RowI, 0]), !.

findJumpableFrog(Board, Player, RowI) :-
    NextI is RowI+1,
    findJumpableFrog(Board, Player, NextI).


/**
 * Verify Win Condition
 * verifyWinCondition(+Board, +LastPlayer, -Loser)
 * Checks if the game has ended and returns the Loser, given 
 * that the last player jumping was LastPlayer.
 * 
 * Board -> Game board.
 * LastPlayer -> Player which last played.
 * Loser -> Player who lost.
 */
verifyWinCondition(Board, 1, 2) :- \+findJumpableFrog(Board, 2, 0), !.
verifyWinCondition(Board, 2, 1) :- \+findJumpableFrog(Board, 1, 0), !.
verifyWinCondition(Board, 1, 1) :- \+findJumpableFrog(Board, 1, 0), !.
verifyWinCondition(Board, 2, 2) :- \+findJumpableFrog(Board, 2, 0), !.


/**
 * Remove row outer frogs
 * removeRowOuterFrogs(+CurrRow, +Positon, -NewRow)
 * Iterates over the columns of a row, emptying the positions on the board edges.
 * 
 * CurrRow -> Initial row.
 * Positon -> Current position.
 * NewRow -> Modified row.
 */
removeRowOuterFrogs(_, [_, 8], []) :- !.

removeRowOuterFrogs([_ | Rest], [0, ColI], [empty | NewRow]) :-
    NextCol is ColI+1,
    removeRowOuterFrogs(Rest, [0, NextCol], NewRow).
    
removeRowOuterFrogs([_ | Rest], [7, ColI], [empty | NewRow]) :-
    NextCol is ColI+1,
    removeRowOuterFrogs(Rest, [7, NextCol], NewRow).

removeRowOuterFrogs([_ | Rest], [RowI, 0], [empty | NewRow]) :-
    removeRowOuterFrogs(Rest, [RowI, 1], NewRow).

removeRowOuterFrogs([_ | Rest], [RowI, 7], [empty | NewRow]) :-
    removeRowOuterFrogs(Rest, [RowI, 8], NewRow).

removeRowOuterFrogs([CurrVal | Rest], [RowI, ColI], [CurrVal | NewRow]) :-
    RowI > 0, RowI < 7,
    ColI > 0, ColI < 7,
    NextCol is ColI+1,
    removeRowOuterFrogs(Rest, [RowI, NextCol], NewRow).


/**
 * Remove outer frogs helper
 * removeOuterFrogsHelper(+InBoard, +RowI, -OutBoard)
 * Removes frogs from InBoard in outer positions, iterating over all the rows.
 * In each iteration, appends a modified row to the OutBoard.
 * 
 * InBoard -> Initial board.
 * RowI -> Current row.
 * OutBoard -> Modified board.
 */
removeOuterFrogsHelper([], 8, []) :- !.

removeOuterFrogsHelper([CurrRow | Rest], RowI, [NewRow | NewBoard]) :-
    removeRowOuterFrogs(CurrRow, [RowI, 0], NewRow),
    NextRow is RowI+1,
    removeOuterFrogsHelper(Rest, NextRow, NewBoard).

/**
 * Remove outer frogs
 * removeOuterFrogs(+InBoard, -OutBoard)
 * Removes frogs from InBoard in outer positions.
 * 
 * InBoard -> Initial board.
 * OutBoard -> Modified board.
 */
removeOuterFrogs(InBoard, OutBoard) :-
    removeOuterFrogsHelper(InBoard, 0, OutBoard).


playTurn(InBoard, Player, OutBoard) :-
    % read jump positions until valid
    repeat,
        readJumpPositions(InBoard, Player, InitPos, MidPos, EndPos, Frog), !,

    % perform the jump
    jump(InBoard, InitPos, MidPos, EndPos, Frog, OutBoard).


playGame :-
    initBoard(B),
    display_game(B, 1, 1),
    removeOuterFrogs(B, NewB),
    display_game(NewB, 1, 1).
    /*
    playTurn(B, 1, NewB),
    display_game(NewB, 2, 1).
    */