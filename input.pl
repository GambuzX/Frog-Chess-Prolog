readInput(Input) :-
    get0(Ch),
    readRest(Ch, AllChars),
    name(Input, AllChars).

readRest(10, []).
readRest(13, []).
readRest(Ch, [Ch | Rest]) :-
    Ch \= 10,
    Ch \= 13,
    get0(Ch1),
    readRest(Ch1, Rest).

readInteger(Int) :-
    readInput(Int),
    integer(Int).

readChar(Char) :-
    get0(Ch),
    Ch \= 10,
    Ch \= 13,
    name(Char, [Ch]),
    readRest(Char, _).

readCol(Col) :-
    readInteger(Col),
    Col >= 0,
    Col =< 7.

readRow(Row) :-
    readChar(Row),
    char_type(Row, alpha),
    char_code(Row, Code),
    Code >= 97,
    Code =< 104.