:- include('game_state.pl').
:- include('display.pl').
:- include('input.pl').

/** 
 * Player Frog
 * playerFrog(+Number, -PlayerFrogColor)
 * Associates a frog color to each player number.
 *
 * Number -> Number of the player. Since for now we only consider 2 players, it is either 1 or 2.
 * PlayerFrogColor -> Returns the frog color associated with the player number.
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
 * setPositionHelper(+Board, +TargetRow, +TargetCol, +NewValue, +CurrRowI, -NewBoard)
 * Changes the board position given by (Row, Col) to the NewValue.
 * The rest of the board is copied.
 * Iterates over the board rows, copying them to the NewBoard, except in the case that
 * it finds the target row to be changed.
 * 
 * Board -> Original board to be changed.
 * TargetRow -> Target Row.
 * TargetCol -> Target column.
 * NewValue -> New value to be added.
 * CurrRowI -> Row iterator, from 0 to 7.
 * NewBoard -> Returns the modified board.
 */
setPositionHelper(_, _, _, _, 8, []).

setPositionHelper([CurrRow | Rest], TargetRow, TargetCol, NewValue, TargetRow, [NewRow | NewBoard]) :-
    setNewRow(CurrRow, TargetCol, NewValue, 0, NewRow),
    NextRowI is TargetRow+1,
    setPositionHelper(Rest, TargetRow, TargetCol, NewValue, NextRowI, NewBoard).

setPositionHelper([CurrRow | Rest], TargetRow, TargetCol, NewValue, RowI, [CurrRow | NewBoard]) :-
    TargetRow \= RowI, RowI >= 0, RowI =< 7,
    NextRowI is RowI+1,
    setPositionHelper(Rest, TargetRow, TargetCol, NewValue, NextRowI, NewBoard).

/**
 * Set position
 * setPosition(+Board, +Row, +Col, +NewValue, -NewBoard)
 * Changes the board position given by (Row, Col) to the NewValue.
 * 
 * Board -> Original board to be changed.
 * Row -> Target Row.
 * Col -> Target column.
 * NewValue -> New value to be added.
 * NewBoard -> Returns the modified board.
 */
setPosition(Board, [Row, Col], NewValue, NewBoard) :-
    setPositionHelper(Board, Row, Col, NewValue, 0, NewBoard).

/**
 * Valid jump
 * validJump(+StartRow, +StartColumn, +EndRow, +EndColumn)
 * Verifies if a jump from (StartRow, StartColumn) to (EndRow, EndColumn) is valid.
 * Jumps can be horizontal, vertical or diagonal, 2 positions from the starting one.
 * 
 * StartRow -> Starting row.
 * StartColumn -> Starting column.
 * EndRow -> Ending row.
 * EndColumn -> Ending column.
 */
validJump(SRow, SCol, ERow, ECol) :- 
    SRow = ERow,
    Comp is ECol-2,
    SCol = Comp.

validJump(SRow, SCol, ERow, ECol) :- 
    SRow = ERow,
    Comp is ECol+2,
    SCol = Comp.

validJump(SRow, SCol, ERow, ECol) :- 
    SCol = ECol,
    Comp is ERow-2,
    SRow = Comp.

validJump(SRow, SCol, ERow, ECol) :- 
    SCol = ECol,
    Comp is ERow+2,
    SRow = Comp.

validJump(SRow, SCol, ERow, ECol) :-
    RowComp is ERow-2,
    ColComp is ECol-2,
    SRow = RowComp,
    SCol = ColComp.

validJump(SRow, SCol, ERow, ECol) :-
    RowComp is ERow-2,
    ColComp is ECol+2,
    SRow = RowComp,
    SCol = ColComp.

validJump(SRow, SCol, ERow, ECol) :-
    RowComp is ERow+2,
    ColComp is ECol-2,
    SRow = RowComp,
    SCol = ColComp.

validJump(SRow, SCol, ERow, ECol) :-
    RowComp is ERow+2,
    ColComp is ECol+2,
    SRow = RowComp,
    SCol = ColComp.


/**
 * Jump
 * jump(+InputBoard, +Player, +StartRow, +StartColumn, +EndRow, +EndColumn, -OutputBoard)
 * Jumps a frog from starting position to end position.
 * Verifies if the jump is valid.
 * 
 * InputBoard -> Initial board before jumping.
 * Player -> Player responsible for the jump.
 * StartRow -> Starting row.
 * StartColumn -> Starting column.
 * EndRow -> Ending row.
 * EndColumn -> Ending column.
 * OutputBoard -> Modified board after jumping.
 */
jump(InBoard, Player, SRow, SCol, ERow, ECol, OutBoard) :-
    % check if positions are inside the board
    SRow >= 0, SRow =< 7,
    ERow >= 0, ERow =< 7,

    % check if jump is valid
    validJump(SRow, SCol, ERow, ECol),

    % check if there is a frog of current player in start position
    getPosition(InBoard, SRow, SCol, Frog), 
    playerFrog(Player, Frog).

    % check middle position for a frog

    % set middle position empty

    % set initial position empty

    % set end position to current frog




playGame :-
    initBoard(B),
    display_game(B, 1, 1),
    readPosition(Row, Col),
    setPosition(B, Row, Col, empty, NewB),
    display_game(NewB, 2, 1).