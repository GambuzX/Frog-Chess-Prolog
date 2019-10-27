:- include('game_state.pl').
:- include('display.pl').
:- include('input.pl').

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
 * getPosition(+Board, +Row, +Col, -Value)
 * Returns the value in position (Row, Col) in the given board.
 * 
 * Board -> List of lists representing the board.
 * Row -> Target row.
 * Col -> Target column.
 * Value -> Variable to return target value.
 */
getPosition(Board, Row, Col, Value) :-
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
setPosition(Board, Row, Col, NewValue, NewBoard) :-
    setPositionHelper(Board, Row, Col, NewValue, 0, NewBoard).

play_game :-
    initBoard(B),
    display_game(B, 1, 1),
    readPosition(Row, Col),
    setPosition(B, Row, Col, empty, NewB),
    display_game(NewB, 2, 1).