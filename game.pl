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
 * Row to index
 * rowToIndex(+Row, -Index)
 * Returns index of given Row value.
 * 
 * Row -> Value representing the row, from a to h
 * Index -> Variable to return Index
 */
rowToIndex(Row, Index) :- 
    char_code(Row, Code),
    Index is Code-97.

/**
 * Column to index
 * colToIndex(+Col, -Index)
 * Returns index of given Col value.
 * 
 * Col -> Value representing the column, from 1 to 8
 * Index -> Variable to return Index
 */
colToIndex(Col, Index) :- Index is Col-1.


/**
 * Get Target Row
 * getTargetRow(+Board, +InRow, -OutRow)
 * Returns the row of of index InRow from Board in OutRow.
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
    rowToIndex(Row, RowI),
    colToIndex(Col, ColI),
    getTargetRow(Board, RowI, TargetRow),
    getValueInRow(TargetRow, ColI, Value).


play_game(V) :-
    initBoard(B),
    readPosition(Row, Col),
    getPosition(B, Row, Col, V).