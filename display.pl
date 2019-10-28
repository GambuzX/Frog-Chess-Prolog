:- include(library(ansi_term)). % library used to display colored output

/**
 * Player Color
 * player_color(+Player, -DisplayColor)
 * Associates a display color to each player.
 * 
 * Player -> Player number.
 * DisplayColor -> Color used to display the specified player. 
 */
player_color(1, cyan).
player_color(2, yellow).

/**
 * Frog Color
 * frog_color(+PlayerFrogColor, -DisplayFrogColor)
 * Associates a display color to each frog color.
 * 
 * PlayerFrogColor -> Color of the frog of a given player.
 * DisplayFrogColor -> Color used to display the specified frog. 
 */
frog_color(blue, cyan).
frog_color(yellow, yellow).


/**
 * Display Game 
 * display_game(+Board, +Player, +JumpN)
 * Displays the current state of the game, that is, the board and the player turn.
 *
 * Board -> Matrix that represents the current board.
 * Player -> Number of the player to make the next move.
 * JumpN -> Number of the jump in the current turn
 */
display_game(Board, Player, JumpN) :-
    JumpN >= 1,
    playerFrog(Player, _), nl,
    display_board(Board), nl,
    display_turn(Player, JumpN).

/**
 * Display Turn
 * display_turn(+Player, +JumpN)
 * Displays an indicator of the next player to make a move.
 *
 * Player -> The player number that will play the next turn.
 * JumpN -> Number of the jump in the current turn
 */
display_turn(Player, JumpN) :-
    JumpN >= 1,
    player_color(Player, Color),

    write('  /===============\\  '), nl,
    write('  | '), ansi_format([fg(black), bg(Color)], 'Player ~d Turn', [Player]), write(' |  Jump number '), write(JumpN), nl,
    write('  \\===============/  '), nl, nl.

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
    display_row_content(Row, 0),    
    write('  '), display_div(0), nl.

display_row(Row, 7) :-
    display_row_content(Row, 7),
    write('  '), display_bottom(0), nl.

display_row(Row, RowN) :-
    RowN > 0,   
    RowN < 7,
    display_row_content(Row, RowN),
    write('  '), display_div(0), nl.

/** 
 * Display Row Content
 * display_row_content(+Row, +RowNumber)
 * Display the content of a row in the board, spanning 5 lines.
 * In each third line, the row identifier is also displayed
 * 
 * Row -> List with a representation of a board row.
 * RowNumber -> Number of the row that will be displayed. The RowNumber is in range [0, 7].
 */
display_row_content(Row, RowNumber) :-
    write('  '), display_content_row_1(Row, RowNumber, 0), nl,
    write('  '), display_content_row_2(Row, RowNumber, 0), nl,
    ID is 97+RowNumber, ansi_format([fg(blue)], '~c', [ID]), write(' '), display_content_row_3(Row, RowNumber, 0), nl,
    write('  '), display_content_row_4(Row, RowNumber, 0), nl,
    write('  '), display_content_row_5(Row, RowNumber, 0), nl.



/**
 * Display Content Row 1
 * display_content_row_1(+Row, +RowNumber, +ColumnNumber)
 * Display the first line of the content of a row, iterating over its columns.
 * It displays the first line of the frogs, empty space or the middle separators.
 *
 * Row -> List representing a board row.
 * RowNumber -> Number of the row to be displayed, in the range [0,7]. Used to determine if row is at an edge of the board.
 * ColumnNumber -> Number of the column to be displayed, in the range [0,7].
 */
display_content_row_1([], _, _).

display_content_row_1([Content|Rest], RowNumber, 0) :-    
    put_code(186), % ║ 
    display_content_ascii_1(Content, RowNumber, 0),
    put_code(186), % ║
    display_content_row_1(Rest, RowNumber, 1).


display_content_row_1([Content|Rest], RowNumber, ColN) :-
    ColN > 0,
    ColN < 8,
    display_content_ascii_1(Content, RowNumber, ColN),
    put_code(186), % ║ 
    NextCol is ColN + 1,
    display_content_row_1(Rest, RowNumber, NextCol).


/**
 * Display Content Row 2
 * display_content_row_2(+Row, +RowNumber, +ColumnNumber)
 * Display the second line of the content of a row, iterating over its columns.
 * It displays the second line of the frogs, empty space or the middle separators.
 *
 * Row -> List representing a board row.
 * RowNumber -> Number of the row to be displayed, in the range [0,7]. Used to determine if row is at an edge of the board.
 * ColumnNumber -> Number of the column to be displayed, in the range [0,7].
 */
display_content_row_2([], _, _).

display_content_row_2([Content|Rest], RowNumber, 0) :-    
    put_code(186), % ║ 
    display_content_ascii_2(Content, RowNumber, 0),
    put_code(186), % ║ 
    display_content_row_2(Rest, RowNumber, 1).


display_content_row_2([Content|Rest], RowNumber, ColN) :-
    ColN > 0,
    ColN < 8,
    display_content_ascii_2(Content, RowNumber, ColN),
    put_code(186), % ║ 
    NextCol is ColN + 1,
    display_content_row_2(Rest, RowNumber, NextCol).


/**
 * Display Content Row 3
 * display_content_row_3(+Row, +RowNumber, +ColumnNumber)
 * Display the third line of the content of a row, iterating over its columns.
 * It displays the third line of the frogs, empty space or the separators.
 *
 * Row -> List representing a board row.
 * RowNumber -> Number of the row to be displayed, in the range [0,7]. Used to determine if row is at an edge of the board.
 * ColumnNumber -> Number of the column to be displayed, in the range [0,7].
 */
display_content_row_3([], _, _).

display_content_row_3([Content|Rest], RowNumber, 0) :-    
    put_code(186), % ║ 
    display_content_ascii_3(Content, RowNumber, 0),
    put_code(186), % ║ 
    display_content_row_3(Rest, RowNumber, 1).


display_content_row_3([Content|Rest], RowNumber, ColN) :-
    ColN > 0,
    ColN < 8,
    display_content_ascii_3(Content, RowNumber, ColN),
    put_code(186), % ║ 
    NextCol is ColN + 1,
    display_content_row_3(Rest, RowNumber, NextCol).


/**
 * Display Content Row 4
 * display_content_row_4(+Row, +RowNumber, +ColumnNumber)
 * Display the fourth line of the content of a row, iterating over its columns.
 * It displays the fourth line of the frogs, empty space or the separators.
 *
 * Row -> List representing a board row.
 * RowNumber -> Number of the row to be displayed, in the range [0,7]. Used to determine if row is at an edge of the board.
 * ColumnNumber -> Number of the column to be displayed, in the range [0,7].
 */
display_content_row_4([], _, _).

display_content_row_4([Content|Rest], RowNumber, 0) :-    
    put_code(186), % ║ 
    display_content_ascii_4(Content, RowNumber, 0),
    put_code(186), % ║ 
    display_content_row_4(Rest, RowNumber, 1).

display_content_row_4([Content|Rest], RowNumber, ColN) :-
    ColN > 0,
    ColN < 8,
    display_content_ascii_4(Content, RowNumber, ColN),
    put_code(186), % ║ 
    NextCol is ColN + 1,
    display_content_row_4(Rest, RowNumber, NextCol).


/**
 * Display Content Row 5
 * display_content_row_5(+Row, +RowNumber, +ColumnNumber)
 * Display the fifth line of the content of a row, iterating over its columns.
 * It displays the fifth line of the frogs, empty space or the separators.
 *
 * Row -> List representing a board row.
 * RowNumber -> Number of the row to be displayed, in the range [0,7]. Used to determine if row is at an edge of the board.
 * ColumnNumber -> Number of the column to be displayed, in the range [0,7].
 */
display_content_row_5([], _, _).

display_content_row_5([Content|Rest], RowNumber, 0) :-    
    put_code(186), % ║ 
    display_content_ascii_5(Content, RowNumber, 0),
    put_code(186), % ║ 
    display_content_row_5(Rest, RowNumber, 1).

display_content_row_5([Content|Rest], RowNumber, ColN) :-
    ColN > 0,
    ColN < 8,
    display_content_ascii_5(Content, RowNumber, ColN),
    put_code(186), % ║ 
    NextCol is ColN + 1,
    display_content_row_5(Rest, RowNumber, NextCol).

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
 * Display Content Ascii 1
 * display_content_ascii_1(+Content, +RowNumber, +ColumnNumber)
 * Displays the first line of a content row.
 * Decides wheter to draw a frog, a flower of empty space, according to the Content.
 *
 * Content -> Content to print.
 * RowNumber -> Row of the current cell.
 * ColumnNumber -> Column of the current cell.
 */
display_content_ascii_1(Content, RowNumber, ColumnNumber) :-
    Content \= empty,
    (RowNumber = 0; RowNumber = 7; ColumnNumber = 0 ; ColumnNumber = 7),
    display_frog_ascii_1(Content, blue), !.

display_content_ascii_1(Content, RowNumber, ColumnNumber) :-
    Content \= empty,
    (RowNumber > 0; RowNumber < 7; ColumnNumber > 0 ; ColumnNumber < 7),
    display_frog_ascii_1(Content, default), !.

display_content_ascii_1(Content, RowNumber, ColumnNumber) :-
    Content = empty,
    (RowNumber = 0; RowNumber = 7; ColumnNumber = 0 ; ColumnNumber = 7),
    display_flower_ascii_1, !.

display_content_ascii_1(Content, RowNumber, ColumnNumber) :-
    Content = empty,
    (RowNumber > 0; RowNumber < 7; ColumnNumber > 0 ; ColumnNumber < 7),
    display_empty_line, !.

/**
 * Display Content Ascii 2
 * display_content_ascii_2(+Content, +RowNumber, +ColumnNumber)
 * Displays the second line of a content row.
 * Decides wheter to draw a frog, a flower of empty space, according to the Content.
 *
 * Content -> Content to print.
 * RowNumber -> Row of the current cell.
 * ColumnNumber -> Column of the current cell.
 */
display_content_ascii_2(Content, RowNumber, ColumnNumber) :-
    Content \= empty,
    (RowNumber = 0; RowNumber = 7; ColumnNumber = 0 ; ColumnNumber = 7),
    display_frog_ascii_2(Content, blue), !.

display_content_ascii_2(Content, RowNumber, ColumnNumber) :-
    Content \= empty,
    (RowNumber > 0; RowNumber < 7; ColumnNumber > 0 ; ColumnNumber < 7),
    display_frog_ascii_2(Content, default), !.

display_content_ascii_2(Content, RowNumber, ColumnNumber) :-
    Content = empty,
    (RowNumber = 0; RowNumber = 7; ColumnNumber = 0 ; ColumnNumber = 7),
    display_flower_ascii_2, !.

display_content_ascii_2(Content, RowNumber, ColumnNumber) :-
    Content = empty,
    (RowNumber > 0; RowNumber < 7; ColumnNumber > 0 ; ColumnNumber < 7),
    display_empty_line, !.

/**
 * Display Content Ascii 3
 * display_content_ascii_3(+Content, +RowNumber, +ColumnNumber)
 * Displays the third line of a content row.
 * Decides wheter to draw a frog, a flower of empty space, according to the Content.
 *
 * Content -> Content to print.
 * RowNumber -> Row of the current cell.
 * ColumnNumber -> Column of the current cell.
 */
display_content_ascii_3(Content, RowNumber, ColumnNumber) :-
    Content \= empty,
    (RowNumber = 0; RowNumber = 7; ColumnNumber = 0 ; ColumnNumber = 7),
    display_frog_ascii_3(Content, blue), !.
    
display_content_ascii_3(Content, RowNumber, ColumnNumber) :-
    Content \= empty,
    (RowNumber > 0; RowNumber < 7; ColumnNumber > 0 ; ColumnNumber < 7),
    display_frog_ascii_3(Content, default), !.

display_content_ascii_3(Content, RowNumber, ColumnNumber) :-
    Content = empty,
    (RowNumber = 0; RowNumber = 7; ColumnNumber = 0 ; ColumnNumber = 7),
    display_flower_ascii_3, !.

display_content_ascii_3(Content, RowNumber, ColumnNumber) :-
    Content = empty,
    (RowNumber > 0; RowNumber < 7; ColumnNumber > 0 ; ColumnNumber < 7),
    display_empty_line, !.

/**
 * Display Content Ascii 4
 * display_content_ascii_4(+Content, +RowNumber, +ColumnNumber)
 * Displays the fourth line of a content row.
 * Decides wheter to draw a frog, a flower of empty space, according to the Content.
 *
 * Content -> Content to print.
 * RowNumber -> Row of the current cell.
 * ColumnNumber -> Column of the current cell.
 */
display_content_ascii_4(Content, RowNumber, ColumnNumber) :-
    Content \= empty,
    (RowNumber = 0; RowNumber = 7; ColumnNumber = 0 ; ColumnNumber = 7),
    display_frog_ascii_4(Content, blue), !.
    
display_content_ascii_4(Content, RowNumber, ColumnNumber) :-
    Content \= empty,
    (RowNumber > 0; RowNumber < 7; ColumnNumber > 0 ; ColumnNumber < 7),
    display_frog_ascii_4(Content, default), !.

display_content_ascii_4(Content, RowNumber, ColumnNumber) :-
    Content = empty,
    (RowNumber = 0; RowNumber = 7; ColumnNumber = 0 ; ColumnNumber = 7),
    display_flower_ascii_4, !.

display_content_ascii_4(Content, RowNumber, ColumnNumber) :-
    Content = empty,
    (RowNumber > 0; RowNumber < 7; ColumnNumber > 0 ; ColumnNumber < 7),
    display_empty_line, !.

/**
 * Display Content Ascii 5
 * display_content_ascii_5(+Content, +RowNumber, +ColumnNumber)
 * Displays the fifth line of a content row.
 * Decides wheter to draw a frog, a flower of empty space, according to the Content.
 *
 * Content -> Content to print.
 * RowNumber -> Row of the current cell.
 * ColumnNumber -> Column of the current cell.
 */
display_content_ascii_5(Content, RowNumber, ColumnNumber) :-
    Content \= empty,
    (RowNumber = 0; RowNumber = 7; ColumnNumber = 0 ; ColumnNumber = 7),
    display_frog_ascii_5(Content, blue), !.
    
display_content_ascii_5(Content, RowNumber, ColumnNumber) :-
    Content \= empty,
    (RowNumber > 0; RowNumber < 7; ColumnNumber > 0 ; ColumnNumber < 7),
    display_frog_ascii_5(Content, default), !.

display_content_ascii_5(Content, RowNumber, ColumnNumber) :-
    Content = empty,
    (RowNumber = 0; RowNumber = 7; ColumnNumber = 0 ; ColumnNumber = 7),
    display_flower_ascii_5, !.

display_content_ascii_5(Content, RowNumber, ColumnNumber) :-
    Content = empty,
    (RowNumber > 0; RowNumber < 7; ColumnNumber > 0 ; ColumnNumber < 7),
    display_empty_line, !.

/**
 * Display Frog Ascii 1
 * display_frog_ascii_1(+Frog, +BGColor)
 * Displays the first line used in the ascii art of the frog.
 * The display color is given by the term frog_color.
 *
 * Frog -> Color of the frog to print.
 * BGColor -> Color of the background.
 */
display_frog_ascii_1(Frog, BGColor) :-
    frog_color(Frog, Color),
    ansi_format([fg(Color), bg(BGColor)], '~w', ['    ']),
    ansi_format([fg(black), bg(Color)], '~w', ['(\')=(\')']),
    ansi_format([fg(Color), bg(BGColor)], '~w', ['    ']).
    
/**
 * Display Frog Ascii 2
 * display_frog_ascii_2(+Frog, +BGColor)
 * Displays the second line used in the ascii art of the frog.
 * The display color is given by the term frog_color.
 *
 * Frog -> Color of the frog to print.
 * BGColor -> Color of the background.
 */
 display_frog_ascii_2(Frog, BGColor) :-
    frog_color(Frog, Color),
    ansi_format([fg(black), bg(BGColor)], '~w', ['  __']),
    ansi_format([fg(black), bg(Color)], '~w', ['(  "  )']),
    ansi_format([fg(Color), bg(BGColor)], '~w', ['__  ']).
    
/**
 * Display Frog Ascii 3
 * display_frog_ascii_3(+Frog, +BGColor)
 * Displays the third line used in the ascii art of the frog.
 * The display color is given by the term frog_color.
 *
 * Frog -> Color of the frog to print.
 * BGColor -> Color of the background.
 */
display_frog_ascii_3(Frog, BGColor) :-
    frog_color(Frog, Color),
    ansi_format([fg(Color), bg(BGColor)], '~w', [' ']),
    ansi_format([fg(black), bg(Color)], '~w', ['/ _/\'---\'\\_ \\']),
    ansi_format([fg(Color), bg(BGColor)], '~w', [' ']).
    
/**
 * Display Frog Ascii 4
 * display_frog_ascii_4(+Frog, +BGColor)
 * Displays the fourth line used in the ascii art of the frog.
 * The display color is given by the term frog_color.
 *
 * Frog -> Color of the frog to print.
 * BGColor -> Color of the background.
 */
display_frog_ascii_4(Frog, BGColor) :-
    frog_color(Frog, Color),
    ansi_format([fg(black), bg(BGColor)], '~w', ['_']),
    ansi_format([fg(black), bg(Color)], '~w', ['\\\\ \\\\   // //']),
    ansi_format([fg(black), bg(BGColor)], '~w', ['_']).
    
/**
 * Display Frog Ascii 5
 * display_frog_ascii_5(+Frog, +BGColor)
 * Displays the fifth line used in the ascii art of the frog.
 * The display color is given by the term frog_color.
 *
 * Frog -> Color of the frog to print.
 * BGColor -> Color of the background.
 */
display_frog_ascii_5(Frog, _) :-
    frog_color(Frog, Color),
    ansi_format([fg(black), bg(Color)], '~w', ['>__)/_\\-/_\\(__<']).

/**
 * Display Empty Line
 * display_empty_line(_)
 * Displays an empty line. Used to fill empty cells that are not outer cells of the board.
 */
display_empty_line :-
    write('               ').

/**
 * Display Flower Ascii 1
 * display_flower_ascii_1(_)
 * Displays the first line of a flower.
 */
display_flower_ascii_1 :-
    ansi_format([fg(green), bg(blue)], '~w', ['    ']),
    ansi_format([fg(black), bg(green)], '~w', ['/\\']),
    ansi_format([fg(black), bg(blue)], '~w', ['   ']),
    ansi_format([fg(black), bg(green)], '~w', ['/\\']),
    ansi_format([fg(green), bg(blue)], '~w', ['    ']).

/**
 * Display Flower Ascii 2
 * display_flower_ascii_2(_)
 * Displays the second line of a flower.
 */
display_flower_ascii_2 :-
    ansi_format([fg(green), bg(blue)], '~w', ['   ']),
    ansi_format([fg(black), bg(green)], '~w', ['/  \\']),
    ansi_format([fg(black), bg(blue)], '~w', [' ']),
    ansi_format([fg(black), bg(green)], '~w', ['/  \\']),
    ansi_format([fg(green), bg(blue)], '~w', ['   ']).

/**
 * Display Flower Ascii 3
 * display_flower_ascii_3(_)
 * Displays the third line of a flower.
 */
display_flower_ascii_3 :-
    ansi_format([fg(green), bg(blue)], '~w', ['  ']),
    ansi_format([fg(black), bg(green)], '~w', ['|    v    |']),
    ansi_format([fg(green), bg(blue)], '~w', ['  ']).

/**
 * Display Flower Ascii 4
 * display_flower_ascii_4(_)
 * Displays the fourth line of a flower.
 */
display_flower_ascii_4 :-
    ansi_format([fg(green), bg(blue)], '~w', ['  ']),
    ansi_format([fg(black), bg(green)], '~w', ['|         |']),
    ansi_format([fg(green), bg(blue)], '~w', ['  ']).

/**
 * Display Flower Ascii 5
 * display_flower_ascii_5(_)
 * Displays the fifth line of a flower.
 */
display_flower_ascii_5 :-
    ansi_format([fg(green), bg(blue)], '~w', ['   ']),
    ansi_format([fg(black), bg(green)], '~w', ['\\_______/']),
    ansi_format([fg(green), bg(blue)], '~w', ['   ']).