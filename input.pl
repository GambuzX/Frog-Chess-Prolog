/**
 * Read input
 * readInput(-Input)
 * Reads a line of input from the user, until a newline is sent.
 * 
 * Input -> Variable to return read value.
 */
readInput(Input) :-
    get0(Ch),
    readRest(Ch, AllChars),
    name(Input, AllChars).

/**
 * Read rest
 * readRest(+Char, -Input)
 * Reads input from the user, char by char, and appends it to List, until newline is found.
 * 
 * Ch -> Last read character.
 * Input -> List of read characters, to which information is added.
 */
readRest(10, []).
readRest(13, []).
readRest(Ch, [Ch | Rest]) :-
    Ch \= 10,
    Ch \= 13,
    get0(Ch1),
    readRest(Ch1, Rest).
    
/**
 * Read integer
 * readInteger(-Int)
 * Reads an integer value from the user, waiting for newline.
 * 
 * Int -> Variable to return read integer.
 */
readInteger(Int) :-
    readInput(Int),
    integer(Int).

/**
 * Read char
 * readChar(-Char)
 * Reads a char from the user, waiting for newline.
 * 
 * Char -> Variable to return read char.
 */
readChar(Char) :-
    get0(Ch),
    Ch \= 10,
    Ch \= 13,
    name(Char, [Ch]),
    readRest(Char, _).

/**
 * Read single char
 * readSingleChar(-Char)
 * Reads a single char from the user, not waiting for newline.
 * 
 * Char -> Variable to return read char.
 */
readSingleChar(Char) :-
    get_single_char(Ch),
    (Ch >= 97, Ch =< 122 ;
    Ch >= 65, Ch =< 90),
    name(Char, [Ch]).

/**
 * Read single integer
 * readSingleInteger(-Int)
 * Reads a single integer value from the user, not waiting for newline.
 * 
 * Int -> Variable to return read integer.
 */
readSingleInteger(Int) :-
    get_single_char(Ch),
    Ch >= 48,
    Ch =< 57,
    name(Int, [Ch]).

/**
 * Read column
 * readCol(-Col)
 * Reads a column from the user, that is, an integer from 1 to 8.
 * Does not wait for newline.
 * 
 * Col -> Variable to return read value.
 */
readCol(Col) :-
    readSingleInteger(Col),
    Col >= 1,
    Col =< 8.

/**
 * Read row
 * readRow(-Row)
 * Reads a row from the user, that is, a char from a to h.
 * Does not wait for newline.
 * 
 * Row -> Variable to return read value.
 */
readRow(Row) :-
    readSingleChar(Row),
    char_type(Row, alpha),
    char_code(Row, Code),
    Code >= 97,
    Code =< 104.

    

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
 * Read position
 * readPosition(-Row, -Col)
 * Reads a position from the user, that is, a row and a column.
 * Writes text to help the user understand what is being asked, 
 * in the format: Position: (Row,Col)
 * 
 * Row -> Variable to return read row.
 * Col -> Variable to return read col.
 */
readPosition(Row, Col) :-
    write('Position: ('),
    readRow(RowInp),
    write(RowInp),
    put_char(','),
    readCol(ColInp),
    write(ColInp),
    put_char(')'),
    nl,
    rowToIndex(RowInp, Row),
    colToIndex(ColInp, Col).