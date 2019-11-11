/** 
 * Player Frog
 * player_frog(+Number, -player_frog)
 * Associates a frog to each player number.
 *
 * Number -> Number of the player. Since for now we only consider 2 players, it is either 1 or 2.
 * player_frog -> Returns the frog associated with the player number.
 */
player_frog(1, blue).
player_frog(2, yellow).

/** 
 * Iterate values
 * iterate_values(+Current, +Target, -Out)
 * Iterates over the values from Curr to Target, returning them in Out.
 * 
 * Target -> Last value.
 * Curr -> Current value.
 * Out -> Variable to return values.
 */
iterate_values(Curr, Target, Curr) :-
    Curr >= 0,
    Curr =< Target.

iterate_values(Curr, Target, Out) :-
    Curr >= 0,
    Curr =< Target,
    Next is Curr+1,
    iterate_values(Next, Target, Out).


/** 
 * Valid row
 * valid_row(+Board, -Row)
 * Checks if a given row is valid given the current board.
 * 
 * Board -> Game Board.
 * Row -> Row index to check/retrieve.
 */
valid_row(Board, Row) :-
    length(Board, L),
    LastRow is L-1,
    iterate_values(0, LastRow, Row).

/** 
 * Valid column
 * valid_column(+Board, -Column)
 * Checks if a given column is valid given the current board.
 * 
 * Board -> Game Board.
 * Column -> Column index to check/retrieve.
 */
valid_column([FirstRow | _], Column) :-
    length(FirstRow, L),
    LastCol is L-1,
    iterate_values(0, LastCol, Column).


/** 
 * Valid position
 * valid_position(+Board, -Position)
 * Checks if a given position is valid, that is, inside the given board.
 *
 * Position -> Position to check, in the format [Row, Column].
 */
valid_position(Board, [Row, Column]) :-
    valid_row(Board, Row),
    valid_column(Board, Column).

/** 
 * Valid fill row
 * valid_fill_row(+Board, -Row)
 * Checks if a given row is valid to fill given the current board.
 * Can not be in an edge of the board.
 * 
 * Board -> Game Board.
 * Row -> Row index to check/retrieve.
 */
valid_fill_row(Board, Row) :-
    length(Board, L),
    LastFillRow is L-2,
    iterate_values(1, LastFillRow, Row).


/** 
 * Valid fill column
 * valid_fill_column(+Board, -Column)
 * Checks if a given column is valid to fill given the current board.
 * Can not be in an edge of the board.
 * 
 * Board -> Game Board.
 * Column -> Column index to check/retrieve.
 */
valid_fill_column([FirstRow | _], Column) :-
    length(FirstRow, L),
    LastFillCol is L-2,
    iterate_values(1, LastFillCol, Column).

/** 
 * Valid fill position
 * valid_fill_position(+Board, -Position)
 * Checks if a given position is valid to fill with a frog in the beginning of the game.
 * May be used to retrieve valid positions from the board.
 *
 * Board -> Game board.
 * Position -> Position to check/retrieve, in the format [Row, Column].
 */
valid_fill_position(Board, [Row, Column]) :-
    valid_fill_row(Board, Row),
    valid_fill_column(Board, Column).

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
 * set_new_row(+Row, +NCols, +TargetCol, +NewValue, +CurrColI, -NewRow)
 * Sets the value from Row at index TargetCol to NewValue. The rest of the row is copied.
 * Iterates over the row columns, copying them to NewRow, except in the case that it
 * finds the target column.
 * 
 * Row -> Row to iterate over.
 * NCols -> Number of columns in the board.
 * TargetCol -> Target column.
 * NewValue -> New value to be added.
 * CurrColI -> Column iterator, from 0 to 7.
 * NewRow -> Returns the modified row.
 */
set_new_row(_, NCols, _, _, NCols, []).

set_new_row([_ | Rest], NCols, TargetCol, NewValue, TargetCol, [NewValue | NewRow]) :-
    NextColI is TargetCol+1,
    set_new_row(Rest, NCols, TargetCol, NewValue, NextColI, NewRow).

set_new_row([CurrVal | Rest], NCols, TargetCol, NewValue, ColI, [CurrVal | NewRow]) :-
    TargetCol \= ColI, 
    ColI >= 0, ColI < NCols,
    NextColI is ColI+1,
    set_new_row(Rest, NCols, TargetCol, NewValue, NextColI, NewRow).


/**
 * Set position helper
 * set_position_helper(+Board, +NRows, +TargetPosition, +NewValue, +CurrRowI, -NewBoard)
 * Changes the board position given by Target_position, [Row, Col], to the NewValue.
 * The rest of the board is copied.
 * Iterates over the board rows, copying them to the NewBoard, except in the case that
 * it finds the target row to be changed.
 * 
 * Board -> Original board to be changed.
 * NRows -> Number of rows in the board.
 * TargetPosition -> Target position.
 * NewValue -> New value to be added.
 * CurrRowI -> Row iterator, from 0 to 7.
 * NewBoard -> Returns the modified board.
 */
set_position_helper(_, NRows, _, _, NRows, []).

set_position_helper([CurrRow | Rest], NRows, [TargetRow, TargetCol], NewValue, TargetRow, [NewRow | NewBoard]) :-
    length(CurrRow, NCols),
    set_new_row(CurrRow, NCols, TargetCol, NewValue, 0, NewRow),
    NextRowI is TargetRow+1,
    set_position_helper(Rest, NRows, [TargetRow, TargetCol], NewValue, NextRowI, NewBoard).

set_position_helper([CurrRow | Rest], NRows, [TargetRow, TargetCol], NewValue, RowI, [CurrRow | NewBoard]) :-
    TargetRow \= RowI, 
    RowI >= 0, RowI < NRows,
    NextRowI is RowI+1,
    set_position_helper(Rest, NRows, [TargetRow, TargetCol], NewValue, NextRowI, NewBoard).

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
    length(Board, NRows),
    set_position_helper(Board, NRows, Pos, NewValue, 0, NewBoard).

/**
 * Create empty row
 * append_rows(+Columns, -OutRow)
 * Creates an empty row of size Columns
 * 
 * Columns -> Number of columns in the row.
 * OutRow -> Variable to return created empty row.
 */
create_empty_row(0, []) :- !.
create_empty_row(Columns, [empty | EmptyRow]) :-
    Count is Columns-1,
    create_empty_row(Count, EmptyRow).

/**
 * Append rows
 * append_rows(+Rows, +EmptyRow, -OutBoard)
 * Creates a new board by appending EmptyRow in an empty list Rows times.
 * 
 * Rows -> Number of rows in the board.
 * EmptyRow -> Row to append.
 * OutBoard -> Variable to return created empty board.
 */
append_rows(0, _, []) :- !.
append_rows(Rows, EmptyRow, [EmptyRow | OutBoard]) :-
    Count is Rows-1,
    append_rows(Count, EmptyRow, OutBoard).

/**
 * Create empty board
 * create_empty_board(+Rows, +Columns, -OutBoard)
 * Creates a new empty board of given dimensions.
 * 
 * Rows -> Number of rows in the board.
 * Columns -> Number of columns in the board.
 * OutBoard -> Variable to return created empty board.
 */
create_empty_board(Rows, Columns, OutBoard) :-
    create_empty_row(Columns, EmptyRow),
    append_rows(Rows, EmptyRow, OutBoard).