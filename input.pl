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



readSingleChar(Char) :-
    get_single_char(Ch),
    (Ch >= 97, Ch =< 122 ;
    Ch >= 65, Ch =< 90),
    name(Char, [Ch]).

readSingleInteger(Int) :-
    get_single_char(Ch),
    Ch >= 48,
    Ch =< 57,
    name(Int, [Ch]).



readCol(Col) :-
    readSingleInteger(Col),
    Col >= 1,
    Col =< 8.

readRow(Row) :-
    readSingleChar(Row),
    char_type(Row, alpha),
    char_code(Row, Code),
    Code >= 97,
    Code =< 104.

readPosition(Row, Col) :-
    write('Position: ('),
    readRow(Row),
    write(Row),
    put_char(','),
    readCol(Col),
    write(Col),
    put_char(')'),
    nl.