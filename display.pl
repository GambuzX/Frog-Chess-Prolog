:- include('game_state.pl'). % file with various game states defined
:- include(library(ansi_term)). % library used to display colored output

/** 
 * Player Frog
 * player_frog(+Number, -PlayerFrogColor)
 * Associates a frog color to each player number.
 *
 * Number -> Number of the player. Since for now we only consider 2 players, it is either 1 or 2.
 * PlayerFrogColor -> Returns the frog color associated with the player number.
 */
player_frog(1, pink).
player_frog(2, green).

/**
 * Frog Color
 * frog_color(+PlayerFrogColor, -DisplayFrogColor)
 * Associates a display color to each frog color.
 * 
 * PlayerFrogColor -> Color of the frog of a given player.
 * DisplayFrogColor -> Color used to display the specified frog. 
 */
frog_color(pink, magenta).
frog_color(green, green).


/**
 * Display Game 
 * display_game(+Board, +Player)
 * Displays the current state of the game, that is, the board and the player turn.
 *
 * Board -> Matrix that represents the current board.
 * Player -> Number of the player to make the next move.
 */
display_game(Board, Player) :-
    player_frog(Player, _), nl,
    initialBoard(Board),
    display_board(Board), nl,
    display_turn(Player).

/**
 * Display Turn
 * display_turn(+Player)
 * Displays an indicator of the next player to make a move.
 *
 * Player -> The player number that will play the next turn.
 */
display_turn(Player) :-
    player_frog(Player, Frog),
    frog_color(Frog, Color),

    write('                     '), display_frog_ascii_1(Frog), nl,
    write('  /===============\\  '), display_frog_ascii_2(Frog), nl,
    write('  | Player '), ansi_format([fg(Color)], '~d', [Player]), write(' Turn |'), write('  '), display_frog_ascii_3(Frog), nl,
    write('  \\===============/  '), display_frog_ascii_4(Frog), nl,
    write('                     '), display_frog_ascii_5(Frog), nl, nl.

/**
 * Display Board
 * displayboard(+Board)
 * Displays the given board.
 * 
 * Board -> Matrix containing the representation of the board.
 */
display_board(Board) :- display_board_helper(Board, 0).

/**
 * Display Board Helper
 * display_board_helper(+Board, +RowNumber)
 * Helper function to display the board, that iterates over its Rows.
 * 
 * Board -> List of lists containig a representation of the board.
 * RowNumber -> Number of the row that will be displayed. The RowNumber is in the range [0, 7].
 */
display_board_helper([], _).
display_board_helper([Curr_Row|Rest], RowN) :-
    RowN >= 0,
    RowN < 8,
    display_row(Curr_Row, RowN), 
    NextRow is RowN + 1,
    display_board_helper(Rest, NextRow).

/**
 * Display Row
 * display_row(+Row, +RowNumber)
 * Display a row of the board. A row is composed by the content and the divisions around it.
 * The first row also displays the column identifiers, from 'a' to 'h'.
 * 
 * Row -> List with a representation of a board row.
 * RowNumber -> Number of the row that will be displayed. The RowNumber is in the range [0, 7].
 */
display_row(Row, 0) :-
    write('  '), display_col_head(0), nl,
    write('  '), display_top(0), nl,
    display_row_frogs(Row, 1),    
    write('  '), display_div(0), nl.

display_row(Row, 7) :-
    display_row_frogs(Row, 8),
    write('  '), display_bottom(0), nl.

display_row(Row, RowN) :-
    RowN > 0,   
    RowN < 7,
    N is RowN+1,
    display_row_frogs(Row, N),
    write('  '), display_div(0), nl.

/** 
 * Display Row Frogs
 * display_row_frogs(+Row, +RowNumber)
 * Display the content of a row in the board, spanning 5 lines.
 * In each third line, the row identifier is also displayed
 * 
 * Row -> List with a representation of a board row.
 * RowNumber -> Number of the row that will be displayed. The RowNumber is in range [0, 7].
 */
display_row_frogs(Row, N) :-
    write('  '), display_frog_row_1(Row, 0), nl,
    write('  '), display_frog_row_2(Row, 0), nl,
    ansi_format([fg(blue)], '~w', [N]), write(' '), display_frog_row_3(Row, 0), nl,
    write('  '), display_frog_row_4(Row, 0), nl,
    write('  '), display_frog_row_5(Row, 0), nl.



/**
 * Display Frog Row 1
 * display_frog_row_1(+Row, +ColumnNumber)
 * Display the first line of the content of a row, iterating over its columns.
 * It displays the first line of the frogs, empty space or the middle separators.
 *
 * Row -> List representing a board row.
 * ColumnNumber -> Number of the column to be displayed, in the range [0,7].
 */
display_frog_row_1([], _).

display_frog_row_1([Frog|Rest], 0) :-    
    put_code(186), % ║ 
    display_frog_ascii_1(Frog),
    put_code(186), % ║
    display_frog_row_1(Rest, 1).


display_frog_row_1([Frog|Rest], ColN) :-
    ColN > 0,
    ColN < 8,
    display_frog_ascii_1(Frog),
    put_code(186), % ║ 
    NextCol is ColN + 1,
    display_frog_row_1(Rest, NextCol).


/**
 * Display Frog Row 2
 * display_frog_row_2(+Row, +ColumnNumber)
 * Display the second line of the content of a row, iterating over its columns.
 * It displays the second line of the frogs, empty space or the middle separators.
 *
 * Row -> List representing a board row.
 * ColumnNumber -> Number of the column to be displayed, in the range [0,7].
 */
display_frog_row_2([], _).

display_frog_row_2([Frog|Rest], 0) :-    
    put_code(186), % ║ 
    display_frog_ascii_2(Frog),
    put_code(186), % ║ 
    display_frog_row_2(Rest, 1).


display_frog_row_2([Frog|Rest], ColN) :-
    ColN > 0,
    ColN < 8,
    display_frog_ascii_2(Frog),
    put_code(186), % ║ 
    NextCol is ColN + 1,
    display_frog_row_2(Rest, NextCol).


/**
 * Display Frog Row 3
 * display_frog_row_3(+Row, +ColumnNumber)
 * Display the third line of the content of a row, iterating over its columns.
 * It displays the third line of the frogs, empty space or the separators.
 *
 * Row -> List representing a board row.
 * ColumnNumber -> Number of the column to be displayed, in the range [0,7].
 */
display_frog_row_3([], _).

display_frog_row_3([Frog|Rest], 0) :-    
    put_code(186), % ║ 
    display_frog_ascii_3(Frog),
    put_code(186), % ║ 
    display_frog_row_3(Rest, 1).


display_frog_row_3([Frog|Rest], ColN) :-
    ColN > 0,
    ColN < 8,
    display_frog_ascii_3(Frog),
    put_code(186), % ║ 
    NextCol is ColN + 1,
    display_frog_row_3(Rest, NextCol).


/**
 * Display Frog Row 4
 * display_frog_row_4(+Row, +ColumnNumber)
 * Display the fourth line of the content of a row, iterating over its columns.
 * It displays the fourth line of the frogs, empty space or the separators.
 *
 * Row -> List representing a board row.
 * ColumnNumber -> Number of the column to be displayed, in the range [0,7].
 */
display_frog_row_4([], _).

display_frog_row_4([Frog|Rest], 0) :-    
    put_code(186), % ║ 
    display_frog_ascii_4(Frog),
    put_code(186), % ║ 
    display_frog_row_4(Rest, 1).

display_frog_row_4([Frog|Rest], ColN) :-
    ColN > 0,
    ColN < 8,
    display_frog_ascii_4(Frog),
    put_code(186), % ║ 
    NextCol is ColN + 1,
    display_frog_row_4(Rest, NextCol).


/**
 * Display Frog Row 5
 * display_frog_row_5(+Row, +ColumnNumber)
 * Display the fifth line of the content of a row, iterating over its columns.
 * It displays the fifth line of the frogs, empty space or the separators.
 *
 * Row -> List representing a board row.
 * ColumnNumber -> Number of the column to be displayed, in the range [0,7].
 */
display_frog_row_5([], _).

display_frog_row_5([Frog|Rest], 0) :-    
    put_code(186), % ║ 
    display_frog_ascii_5(Frog),
    put_code(186), % ║ 
    display_frog_row_5(Rest, 1).

display_frog_row_5([Frog|Rest], ColN) :-
    ColN > 0,
    ColN < 8,
    (
        ColN = 0,
        put_code(186);
        1 = 1
    ),
    display_frog_ascii_5(Frog),
    put_code(186), % ║ 
    NextCol is ColN + 1,
    display_frog_row_5(Rest, NextCol).

/**
 * Display Column Header
 * display_col_head(+ColumnNumber)
 * Displays the identifier of a column, iterating from column 0 to 7.
 * 
 * ColN -> Number of the column.
 */
display_col_head(7) :-
    write('        '),
    ansi_format([fg(blue)], '~w', [8]),
    write('       ').

display_col_head(ColN) :-
    ColN >= 0,
    ColN < 7,
    N is ColN+1,
    write('        '),
    ansi_format([fg(blue)], '~w', [N]),
    write('       '),
    NextCol is ColN + 1,
    display_col_head(NextCol).


/**
 * Display Top of the Board
 * display_top(+ColumnNumber)
 * Displays the top edge of the board.
 *
 * ColumnNumber -> Number of the column. Used to iterate through the row.
 */
display_top(0) :-
    put_code(201), %╔
    display_div_line(15),
    put_code(203), % ╦
    display_top(1).

display_top(7) :-
    display_div_line(15),
    put_code(187). % ╗

display_top(ColN) :-
    ColN > 0,
    ColN < 7,
    display_div_line(15),
    put_code(203), % ╦
    NextCol is ColN + 1,
    display_top(NextCol).


/**
 * Display Bottom of the Board
 * display_bottom(+ColumnNumber)
 * Displays the bottom edge of the board.
 *
 * ColumnNumber -> Number of the column. Used to iterate through the row.
 */
display_bottom(0) :-
    put_code(200), %╚
    display_div_line(15),
    put_code(202), % ╩
    display_bottom(1).

display_bottom(7) :-
    display_div_line(15),
    put_code(188). %╝

display_bottom(ColN) :-
    ColN > 0,
    ColN < 7,
    display_div_line(15),
    put_code(202), % ╩
    NextCol is ColN + 1,
    display_bottom(NextCol).


/**
 * Display Division of the Board
 * display_div(+ColumnNumber)
 * Displays a division inside the board, separating rows.
 *
 * ColumnNumber -> Number of the column. Used to iterate through the row.
 */
display_div(0) :-
    put_code(204), % ╠
    display_div_line(15),
    put_code(206), % ╬
    display_div(1).

display_div(7) :-
    display_div_line(15),
    put_code(185). % ╣ 

display_div(ColN) :-
    ColN > 0,
    ColN < 7,
    (
        ColN = 0, put_code(204); % ╠
        1 = 1
    ),
    display_div_line(15),
    put_code(206), % ╬
    NextCol is ColN + 1,
    display_div(NextCol).

/**
 * Display Division Line
 * display_div_line(+Count)
 * Displays the character used for a division Count times. 
 *
 * Count -> Number of times to print the character, decrementing in each call.
 */
display_div_line(0).
display_div_line(Count) :-
    Count > 0,
    put_code(205), % ═
    %put_char('\u2550'),
    N is Count-1,
    display_div_line(N).

/**
 * Display Frog Ascii 1
 * display_frog_ascii_1(+Count)
 * Displays the first line used in the ascii art of the frog.
 * The display color is given by the term frog_color.
 *
 * Frog -> Color of the frog to print if there is a frog. Otherwise, empty. 
 */
display_frog_ascii_1(Frog) :-
    Frog = empty,
    write('               ').

display_frog_ascii_1(Frog) :-
    Frog \= empty,
    frog_color(Frog, Color),
    ansi_format([fg(Color)], '~w', ['    (\')=(\')    ']).
    
/**
 * Display Frog Ascii 2
 * display_frog_ascii_2(+Count)
 * Displays the second line used in the ascii art of the frog.
 * The display color is given by the term frog_color.
 *
 * Frog -> Color of the frog to print if there is a frog. Otherwise, empty. 
 */
display_frog_ascii_2(Frog) :-
    Frog = empty,
    write('               ').

display_frog_ascii_2(Frog) :-
    Frog \= empty,
    frog_color(Frog, Color),
    ansi_format([fg(Color)], '~w', ['  __(  "  )__  ']).
    
/**
 * Display Frog Ascii 3
 * display_frog_ascii_3(+Count)
 * Displays the third line used in the ascii art of the frog.
 * The display color is given by the term frog_color.
 *
 * Frog -> Color of the frog to print if there is a frog. Otherwise, empty. 
 */
display_frog_ascii_3(Frog) :-
    Frog = empty,
    write('               ').

display_frog_ascii_3(Frog) :-
    Frog \= empty,
    frog_color(Frog, Color),
    ansi_format([fg(Color)], '~w', [' / _/\'---\'\\_ \\ ']).
    
/**
 * Display Frog Ascii 4
 * display_frog_ascii_4(+Count)
 * Displays the fourth line used in the ascii art of the frog.
 * The display color is given by the term frog_color.
 *
 * Frog -> Color of the frog to print if there is a frog. Otherwise, empty. 
 */
display_frog_ascii_4(Frog) :-
    Frog = empty,
    write('               ').

display_frog_ascii_4(Frog) :-
    Frog \= empty,
    frog_color(Frog, Color),
    ansi_format([fg(Color)], '~w', ['_\\\\ \\\\   // //_']).
    
/**
 * Display Frog Ascii 5
 * display_frog_ascii_5(+Count)
 * Displays the fifth line used in the ascii art of the frog.
 * The display color is given by the term frog_color.
 *
 * Frog -> Color of the frog to print if there is a frog. Otherwise, empty. 
 */
display_frog_ascii_5(Frog) :-
    Frog = empty,
    write('               ').

display_frog_ascii_5(Frog) :-
    Frog \= empty,
    frog_color(Frog, Color),
    ansi_format([fg(Color)], '~w', ['>__)/_\\-/_\\(__<']).