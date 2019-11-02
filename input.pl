/**
 * Read input
 * read_input(-Input)
 * Reads a line of input from the user, until a newline is sent.
 * 
 * Input -> Variable to return read value.
 */
read_input(Input) :-
    get0(Ch),
    read_rest(Ch, AllChars),
    name(Input, AllChars).

/**
 * Read rest
 * read_rest(+Char, -Input)
 * Reads input from the user, char by char, and appends it to List, until newline is found.
 * 
 * Ch -> Last read character.
 * Input -> List of read characters, to which information is added.
 */
read_rest(10, []).
read_rest(13, []).
read_rest(Ch, [Ch | Rest]) :-
    Ch \= 10,
    Ch \= 13,
    get0(Ch1),
    read_rest(Ch1, Rest).
    
/**
 * Read integer
 * read_integer(-Int)
 * Reads an integer value from the user, waiting for newline.
 * 
 * Int -> Variable to return read integer.
 */
read_integer(Int) :-
    read_input(Int),
    integer(Int).

/**
 * Read char
 * read_char(-Char)
 * Reads a char from the user, waiting for newline.
 * 
 * Char -> Variable to return read char.
 */
read_char(Char) :-
    get0(Ch),
    Ch \= 10,
    Ch \= 13,
    name(Char, [Ch]),
    read_rest(Char, _).

/**
 * Read single char
 * read_single_char(-Char)
 * Reads a single char from the user, not waiting for newline.
 * 
 * Char -> Variable to return read char.
 */
read_single_char(Char) :-
    get_single_char(Ch),
    (Ch >= 97, Ch =< 122 ;
    Ch >= 65, Ch =< 90),
    name(Char, [Ch]).

/**
 * Read single integer
 * read_single_integer(-Int)
 * Reads a single integer value from the user, not waiting for newline.
 * 
 * Int -> Variable to return read integer.
 */
read_single_integer(Int) :-
    get_single_char(Ch),
    Ch >= 48,
    Ch =< 57,
    name(Int, [Ch]).

/**
 * Read column
 * read_col(-Col)
 * Reads a column from the user, that is, an integer from 1 to 8.
 * Does not wait for newline.
 * 
 * Col -> Variable to return read value.
 */
read_col(Col) :-
    read_single_integer(Col),
    Col >= 1,
    Col =< 8.

/**
 * Read row
 * read_row(-Row)
 * Reads a row from the user, that is, a char from a to h.
 * Does not wait for newline.
 * 
 * Row -> Variable to return read value.
 */
read_row(Row) :-
    read_single_char(Row),
    char_type(Row, alpha),
    char_code(Row, Code),
    Code >= 97,
    Code =< 104.

    

/**
 * Row to index
 * row_to_index(+Row, -Index)
 * Returns index of given Row value.
 * 
 * Row -> Value representing the row, from a to h
 * Index -> Variable to return Index
 */
row_to_index(Row, Index) :- 
    char_code(Row, Code),
    Index is Code-97.

    

/**
 * Index to row
 * index_to_row(+Index, -Row)
 * Returns row char of given index.
 * 
 * Index -> Row index, from 0 to 7.
 * Row -> Row letter, from 'a' to 'h'.
 */
index_to_row(Index, Row) :-
    Ascii is Index+97,
    char_code(Row, Ascii).

/**
 * Column to index
 * col_to_index(+Col, -Index)
 * Returns index of given Col value.
 * 
 * Col -> Value representing the column, from 1 to 8
 * Index -> Variable to return Index
 */
col_to_index(Col, Index) :- Index is Col-1.

/**
 * Index to column
 * index_to_col(+Index, -Col)
 * Returns column of given index.
 * 
 * Index -> Column index, from 0 to 7.
 * Col -> Column value, from 1 to 8.
 */
index_to_col(Index, Col) :- Col is Index+1.

/**
 * Read position
 * read_position(+PrefixText, -Position)
 * Reads a position from the user, that is, a row and a column.
 * Prefixes a message specified by PrefixText.
 * 
 * PrefixText -> message to be prefixed.
 * Position -> Variable to return read position.
 */
read_position(PrefixText, [Row, Col]) :-
    write(PrefixText),
    write('('),
    read_row(RowInp),
    write(RowInp),
    put_char(','),
    read_col(ColInp),
    write(ColInp),
    put_char(')'),
    nl,
    row_to_index(RowInp, Row),
    col_to_index(ColInp, Col).

/**
 * Yes or No Answer
 * yn_answer(+Answer)
 * States that Answer is classified as a yes or no answer.
 * 
 * Answer -> User answer, 'y' or 'n'.
 */
yn_answer('y').
yn_answer('Y').
yn_answer('n').
yn_answer('N').

/**
 * Ask Yes or No Question
 * ask_yn_question(+Question, -Answer)
 * Asks the user a Yes or No question.
 * 
 * Question -> Question to be prefixed.
 * Answer -> User answer, 'y' or 'n'.
 */
ask_yn_question(Question, Answer) :-
    write(Question),
    read_single_char(Char),
    put_char(Char),
    downcase_atom(Char, Answer),
    yn_answer(Answer).

/**
 * Waits for input
 * wait_for_input
 * Waits for any user input
 */
wait_for_input :-
    nl, write('Press any key to continue...'),
    get_single_char(_), nl.

/**
 * Reads the game mode 
 * read_game_mode(-Mode)
 *
 * Mode -> Number that represents the game mode
 */
read_game_mode(Mode) :-
    read_single_integer(Mode),
    Mode >= 1,
    Mode =< 3.