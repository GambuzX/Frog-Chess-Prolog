/** 
 * Valid position
 * valid_position(+Position)
 * Checks if a given position is valid, that is, inside a 8x8 0-indexed board
 *
 * Position -> Position to check, in the format [Row, Column].
 */
valid_position([0, 0]).
valid_position([1, 0]).
valid_position([2, 0]).
valid_position([3, 0]).
valid_position([4, 0]).
valid_position([5, 0]).
valid_position([6, 0]).
valid_position([7, 0]).
valid_position([0, 1]).
valid_position([7, 1]).
valid_position([0, 2]).
valid_position([7, 2]).
valid_position([0, 3]).
valid_position([7, 3]).
valid_position([0, 4]).
valid_position([7, 4]).
valid_position([0, 5]).
valid_position([7, 5]).
valid_position([0, 6]).
valid_position([7, 6]).
valid_position([0, 7]).
valid_position([1, 7]).
valid_position([2, 7]).
valid_position([3, 7]).
valid_position([4, 7]).
valid_position([5, 7]).
valid_position([6, 7]).
valid_position([7, 7]).
valid_position(Position) :- valid_fill_position(Position).

/** 
 * Valid fill position
 * valid_fill_position(+Position)
 * Checks if a given position is valid to fill with a frog in the beginning of the game
 *
 * Position -> Position to check, in the format [Row, Column].
 */
valid_fill_position([1, 1]).
valid_fill_position([2, 1]).
valid_fill_position([3, 1]).
valid_fill_position([4, 1]).
valid_fill_position([5, 1]).
valid_fill_position([6, 1]).
valid_fill_position([1, 2]).
valid_fill_position([2, 2]).
valid_fill_position([3, 2]).
valid_fill_position([4, 2]).
valid_fill_position([5, 2]).
valid_fill_position([6, 2]).
valid_fill_position([1, 3]).
valid_fill_position([2, 3]).
valid_fill_position([3, 3]).
valid_fill_position([4, 3]).
valid_fill_position([5, 3]).
valid_fill_position([6, 3]).
valid_fill_position([1, 4]).
valid_fill_position([2, 4]).
valid_fill_position([3, 4]).
valid_fill_position([4, 4]).
valid_fill_position([5, 4]).
valid_fill_position([6, 4]).
valid_fill_position([1, 5]).
valid_fill_position([2, 5]).
valid_fill_position([3, 5]).
valid_fill_position([4, 5]).
valid_fill_position([5, 5]).
valid_fill_position([6, 5]).
valid_fill_position([1, 6]).
valid_fill_position([2, 6]).
valid_fill_position([3, 6]).
valid_fill_position([4, 6]).
valid_fill_position([5, 6]).
valid_fill_position([6, 6]).

/**
 * Get Target Row
 * get_target_row(+Board, +InRow, -OutRow)
 * Returns the row of index InRow from Board in OutRow.
 * Uses InRow as a counter and returns current value when it reaches 0.
 * 
 * Board -> List of rows.
 * InRow -> Target row to find.
 * OutRow -> Variable to return target row.
 */
get_target_row([TargetRow | _], 0, TargetRow).
get_target_row([_ | Rest], InRow, OutRow) :-
    InRow > 0,
    N is InRow-1,
    get_target_row(Rest, N, OutRow).

/**
 * Get Value in Row
 * get_value_in_row(+Row, +Col, -Value)
 * Returns the value in index Col of given Row, in the variable Value.
 * Uses Col as a counter and returns current value when it reaches 0.
 * 
 * Row -> Row to search value.
 * Col -> Target column to find.
 * Value -> Variable to return target value.
 */
get_value_in_row([TargetVal | _], 0, TargetVal).
get_value_in_row([_ | Rest], Col, Value) :-
    Col > 0,
    N is Col-1,
    get_value_in_row(Rest, N, Value).

/**
 * Get position
 * get_position(+Board, +Position, -Value)
 * Returns the value in position [Row, Col] in the given board.
 * 
 * Board -> List of lists representing the board.
 * Position -> Position in the board, containing the values [Row, Col].
 * Value -> Variable to return target value.
 */
get_position(Board, [Row, Col], Value) :-
    get_target_row(Board, Row, TargetRow),
    get_value_in_row(TargetRow, Col, Value).


/**
 * Set new row
 * set_new_row(+Row, +TargetCol, +NewValue, +CurrColI, -NewRow)
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
set_new_row(_, _, _, 8, []).

set_new_row([_ | Rest], TargetCol, NewValue, TargetCol, [NewValue | NewRow]) :-
    NextColI is TargetCol+1,
    set_new_row(Rest, TargetCol, NewValue, NextColI, NewRow).

set_new_row([CurrVal | Rest], TargetCol, NewValue, ColI, [CurrVal | NewRow]) :-
    TargetCol \= ColI, ColI >= 0, ColI =< 7,
    NextColI is ColI+1,
    set_new_row(Rest, TargetCol, NewValue, NextColI, NewRow).


/**
 * Set position helper
 * set_position_helper(+Board, +TargetPosition, +NewValue, +CurrRowI, -NewBoard)
 * Changes the board position given by Target_position, [Row, Col], to the NewValue.
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
set_position_helper(_, _, _, 8, []).

set_position_helper([CurrRow | Rest], [TargetRow, TargetCol], NewValue, TargetRow, [NewRow | NewBoard]) :-
    set_new_row(CurrRow, TargetCol, NewValue, 0, NewRow),
    NextRowI is TargetRow+1,
    set_position_helper(Rest, [TargetRow, TargetCol], NewValue, NextRowI, NewBoard).

set_position_helper([CurrRow | Rest], [TargetRow, TargetCol], NewValue, RowI, [CurrRow | NewBoard]) :-
    TargetRow \= RowI, RowI >= 0, RowI =< 7,
    NextRowI is RowI+1,
    set_position_helper(Rest, [TargetRow, TargetCol], NewValue, NextRowI, NewBoard).

/**
 * Set position
 * set_position(+Board, +Position, +NewValue, -NewBoard)
 * Changes the board position given by (Row, Col) to the NewValue.
 * 
 * Board -> Original board to be changed.
 * Position -> Position in the board to be changed, [Row , Column].
 * NewValue -> New value to be added.
 * NewBoard -> Returns the modified board.
 */
set_position(Board, Pos, NewValue, NewBoard) :-
    set_position_helper(Board, Pos, NewValue, 0, NewBoard).