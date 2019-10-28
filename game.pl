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
 * Initialize board
 * initBoard(-Board)
 * Creates a new board filled with frogs.
 * 
 * B -> Variable to return created board.
 */
initBoard(B) :-
    initialBoard(B).


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
 * validJump(+StartPosition, +EndPosition)
 * Verifies if a jump from StartPosition to EndPosition is valid.
 * Jumps can be horizontal, vertical or diagonal, 2 positions from the starting one.
 * 
 * StartPosition -> Starting position.
 * EndPosition -> Ending position.
 */
validJump([SRow, SCol], [ERow, ECol]) :- 
    SRow = ERow,
    Comp is ECol-2,
    SCol = Comp.

validJump([SRow, SCol], [ERow, ECol]) :- 
    SRow = ERow,
    Comp is ECol+2,
    SCol = Comp.

validJump([SRow, SCol], [ERow, ECol]) :- 
    SCol = ECol,
    Comp is ERow-2,
    SRow = Comp.

validJump([SRow, SCol], [ERow, ECol]) :- 
    SCol = ECol,
    Comp is ERow+2,
    SRow = Comp.

validJump([SRow, SCol], [ERow, ECol]) :-
    RowComp is ERow-2,
    ColComp is ECol-2,
    SRow = RowComp,
    SCol = ColComp.

validJump([SRow, SCol], [ERow, ECol]) :-
    RowComp is ERow-2,
    ColComp is ECol+2,
    SRow = RowComp,
    SCol = ColComp.

validJump([SRow, SCol], [ERow, ECol]) :-
    RowComp is ERow+2,
    ColComp is ECol-2,
    SRow = RowComp,
    SCol = ColComp.

validJump([SRow, SCol], [ERow, ECol]) :-
    RowComp is ERow+2,
    ColComp is ECol+2,
    SRow = RowComp,
    SCol = ColComp.



middlePosition([SRow,SCol], [ERow,ECol], [MRow,MCol]) :-
    MRow is (SRow+ERow)/2,
    MCol is (SCol+ECol)/2.

/**
 * Jump
 * jump(+InputBoard, +Player, +StartPosition, +EndPosition, -OutputBoard)
 * Jumps a frog from starting position to end position.
 * Verifies if the jump is valid.
 * 
 * InputBoard -> Initial board before jumping.
 * Player -> Player responsible for the jump.
 * StartPosition -> Starting position.
 * EndPosition -> Ending position.
 * OutputBoard -> Modified board after jumping.
 */
jump(InBoard, StartPos, MidPos, EndPos, Frog, OutBoard) :-
    
    % set middle position empty
    setPosition(InBoard, MidPos, empty, NewBoard1),

    % set initial position empty
    setPosition(NewBoard1, StartPos, empty, NewBoard2),

    % set end position to player frog
    setPosition(NewBoard2, EndPos, Frog, OutBoard).



readJumpPositions(Board, Player, InitPos, MidPos, EndPos, Frog) :-
    % starting position
    readPosition('Frog to jump? ', InitPos),
    getPosition(Board, InitPos, Frog), 
    playerFrog(Player, Frog),

    % end position
    readPosition('Position to jump? ', EndPos),
    getPosition(Board, EndPos, empty),
    
    % check if jump is valid
    validJump(InitPos, EndPos),

    % check middle position for a frog
    middlePosition(InitPos, EndPos, MidPos),
    getPosition(Board, MidPos, MidFrog),
    playerFrog(_, MidFrog).




playTurn(InBoard, Player, OutBoard) :-

    % read jump positions until valid
    repeat,
        nl, readJumpPositions(InBoard, Player, InitPos, MidPos, EndPos, Frog),
        !,

    % perform the jump
    jump(InBoard, InitPos, MidPos, EndPos, Frog, OutBoard).


playGame :-
    initBoard(B),
    display_game(B, 1, 1),
    playTurn(B, 1, NewB),
    display_game(NewB, 2, 1).