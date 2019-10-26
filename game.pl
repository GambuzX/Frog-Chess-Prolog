:- include('game_state.pl').
:- include('display.pl').
:- include('input.pl').

initBoard(B) :-
    initialBoard(B).

rowToIndex(Row, Index) :- 
    char_code(Row, Code),
    Index is Code-97.

colToIndex(Col, Index) :- Index is Col-1.


% Get value
getTargetRow([TargetRow | _], 0, TargetRow).
getTargetRow([_ | Rest], InRow, OutRow) :-
    InRow > 0,
    N is InRow-1,
    getTargetRow(Rest, N, OutRow).

getValueInRow([TargetVal | _], 0, TargetVal).
getValueInRow([_ | Rest], Col, Value) :-
    Col > 0,
    N is Col-1,
    getValueInRow(Rest, N, Value).

getPosition(Board, Row, Col, Value) :-
    rowToIndex(Row, RowI),
    colToIndex(Col, ColI),
    getTargetRow(Board, RowI, TargetRow),
    getValueInRow(TargetRow, ColI, Value).




play_game(V) :-
    initBoard(B),
    readPosition(Row, Col),
    getPosition(B, Row, Col, V).