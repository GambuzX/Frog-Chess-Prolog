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
 * Index to row
 * indexToRow(+Index, -Row)
 * Returns row char of given index.
 * 
 * Index -> Row index, from 0 to 7.
 * Row -> Row letter, from 'a' to 'h'.
 */
indexToRow(Index, Row) :-
    Ascii is Index+97,
    char_code(Row, Ascii).

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
 * Index to column
 * indexToCol(+Index, -Col)
 * Returns column of given index.
 * 
 * Index -> Column index, from 0 to 7.
 * Col -> Column value, from 1 to 8.
 */
indexToCol(Index, Col) :- Col is Index+1.

/**
 * Read position
 * readPosition(+PrefixText, -Position)
 * Reads a position from the user, that is, a row and a column.
 * Prefixes a message specified by PrefixText.
 * 
 * PrefixText -> message to be prefixed.
 * Position -> Variable to return read position.
 */
readPosition(PrefixText, [Row, Col]) :-
    write(PrefixText),
    write('('),
    readRow(RowInp),
    write(RowInp),
    put_char(','),
    readCol(ColInp),
    write(ColInp),
    put_char(')'),
    nl,
    rowToIndex(RowInp, Row),
    colToIndex(ColInp, Col).

/**
 * Yes or No Answer
 * ynAnswer(+Answer)
 * States that Answer is classified as a yes or no answer.
 * 
 * Answer -> User answer, 'y' or 'n'.
 */
ynAnswer('y').
ynAnswer('n').

/**
 * Ask Yes or No Question
 * askYNQuestion(+Question, -Answer)
 * Asks the user a Yes or No question.
 * 
 * Question -> Question to be prefixed.
 * Answer -> User answer, 'y' or 'n'.
 */
askYNQuestion(Question, Answer) :-
    write(Question),
    readSingleChar(Char),
    put_char(Char),
    downcase_atom(Char, Answer),
    ynAnswer(Answer).

/**
 * Waits for input
 * waitForInput
 * Waits for any user input
 */
waitForInput :-
    nl, write('Press any key to continue...'),
    get_single_char(_), nl.