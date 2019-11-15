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
 * read_col(+MaxCol, -Col)
 * Reads a column from the user, that is, an integer from 1 to MaxCol.
 * Does not wait for newline.
 * 
 * MaxCol -> Number of max valid column.
 * Col -> Variable to return read value.
 */
read_col(MaxCol, Col) :-
    read_single_integer(Col),
    Col >= 1,
    Col =< MaxCol.

/**
 * Read row
 * read_row(+MaxRow, -Row)
 * Reads a row from the user, that is, a char from a to MaxRow.
 * Does not wait for newline.
 * 
 * MaxRow -> Max valid row.
 * Row -> Variable to return read value.
 */
read_row(MaxRow, Row) :-
    read_single_char(RowChar),
    char_type(RowChar, alpha),
    downcase_atom(RowChar, Row),
    char_code(Row, Code),
    Code >= 97,
    Code =< 97+MaxRow-1.
    

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
 * read_position(+Board, +PrefixText, -Position)
 * Reads a position from the user, that is, a row and a column.
 * Prefixes a message specified by PrefixText.
 * 
 * Board -> Game board.
 * PrefixText -> message to be prefixed.
 * Position -> Variable to return read position.
 */
read_position(Board, PrefixText, [Row, Col]) :-

    [FirstRow | _] = Board,
    length(Board, NRows),
    length(FirstRow, NCols),

    write(PrefixText),
    write('('),
    read_row(NRows, RowInp),
    write(RowInp),
    put_char(','),
    read_col(NCols, ColInp),
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
yn_answer('n').

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
    Mode =< 3,
    write(Mode).

/**
 * Reads the game dimensions 
 * read_game_dimensions(-Rows, -Columns)
 * Both dimensions must be between 3 and 15.
 * 3x3 board is not allowed.
 *
 * Rows -> Number of rows in the board.
 * Columns -> Number of columns in the board.
 */
read_game_dimensions(Rows, Columns) :-
    write('Number of rows: '),
    read_integer(Rows),
    (
        Rows >= 3, !;
        write('Must have at least 3 rows.'), nl, fail
    ),
    (
        Rows =< 9, !;
        write('Max rows allowed is 9.'), nl, fail
    ),

    write('Number of columns: '),
    read_integer(Columns),
    (
        Columns >= 3, !;
        write('Must have at least 3 columns.'), nl, fail
    ),
    (
        Columns =< 9, !;
        write('Max columns allowed is 9.'), nl, fail
    ),

    (
        Rest is (Rows * Columns) mod 2, Rest = 1, write('Board must have an even number of inner positions!'), nl, !, fail;
        !
    ),
    nl.

/**
 * Reads the ai level
 * read_ai_level(-Level)
 * Reads an integer between 1 and 4.
 *
 * Level -> Level of the AI.
 */
read_ai_level(Level) :-
    read_single_integer(Level),
    Level >= 1,
    Level =< 4,
    write(Level).

/**
 * Reads the menu option
 * read_menu_option(-Option)
 * Reads an integer between 1 and 4.
 *
 * Option -> Selected menu option.
 */
read_menu_option(Option) :-
    read_single_integer(Option), 
    Option >= 1,
    Option =< 4, 
    write(Option).