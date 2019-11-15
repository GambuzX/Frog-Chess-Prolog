%:- include(library(ansi_term)). % library used to display colored output

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
 * Color
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
 * display_game(+Board, +Player, +JumpCount)
 * Displays the current state of the game, that is, the board and the player turn.
 *
 * Board -> Matrix that represents the current board.
 * Player -> Number of the player to make the next move.
 * JumpCount -> Number of jumps in the current turn.
 */
display_game(Board, Player, JumpCount) :-
    JumpCount >= 0, 
    nl, display_board(Board), nl,
    display_jump_turn(Player, JumpCount).

/**
 * Display Jump Turn
 * display_jump_turn(+Player, +JumpCount)
 * Displays an indicator of the next player to make a move.
 * If player does not exist it does nothing.
 *
 * Player -> The player number that will play the next turn.
 * JumpCount -> Number of jumps in the current turn.
 */
display_jump_turn(Player, _) :- \+player_color(Player, _), !.
display_jump_turn(Player, JumpCount) :-
    JumpCount >= 0,
    player_color(Player, Color),

    write('  /===============\\  '), nl,
    write('  | '), ansi_format([fg(black), bg(Color)], 'Player ~d Turn', [Player]), write(' |  Jump count: '), write(JumpCount), nl,
    write('  \\===============/  '), nl, nl.

/**
 * Display Turn To Choose The Position Of Frog
 * display_turn(+Player)
 * Displays an indicator of the next player to put a frog on the board.
 * If player does not exist it does nothing.
 *
 * Player -> The player number that will put the next frog.
 */
display_turn(Player) :- \+player_color(Player, _), !.
display_turn(Player) :-
    player_color(Player, Color),

    write('  /===============\\  '), nl,
    write('  | '), ansi_format([fg(black), bg(Color)], 'Player ~d Turn', [Player]), write(' |'), nl,
    write('  \\===============/  '), nl, nl.

/**
 * Display CPU header
 * display_cpu_header(+Player)
 * Displays an indicator of the current cpu.
 *
 * Player -> The cpu number to print in the header.
 */
display_cpu_header(Player) :- \+player_color(Player, _), !.
display_cpu_header(Player) :-
    player_color(Player, Color),

    write('  /=======\\  '), nl,
    write('  | '), ansi_format([fg(black), bg(Color)], 'CPU ~d', [Player]), write(' |'), nl,
    write('  \\=======/  '), nl, nl.

/**
 * Display CPU Turn To Choose The Position Of Frog
 * display_cpu_fill_turn(+Player, +Position)
 * Displays the position choosen by the cpu.
 * If player does not exist it does nothing.
 *
 * Player -> The player number.
 * Position -> The position choosen by the cpu 
 */
display_cpu_fill_turn(Player, _) :- \+player_color(Player, _), !.

display_cpu_fill_turn(Player, [Row|Column]) :-
    player_color(Player, Color),
    write('  /=======================\\  '), nl,
    RowLetter is 97 + Row,
    Col is Column + 1,
    write('  | '), ansi_format([fg(black), bg(Color)], 'CPU choosed (~c, ~d)...', [RowLetter, Col]), write(' |'), nl,
    write('  \\=======================/  '), nl, nl.

/**
 * Display Board
 * displayboard(+Board)
 * Displays the given board.
 * 
 * Board -> Matrix containing the representation of the board.
 */
display_board(Board) :- 
    length(Board, NRows),
    display_board_helper(Board, NRows, 0).

/**
 * Display Board Helper
 * display_board_helper(+Board, +NRows, +RowNumber)
 * Helper function to display the board, that iterates over its Rows.
 * 
 * Board -> List of lists containig a representation of the board.
 * NRows -> Total number of rows.
 * RowNumber -> Number of the row that will be displayed. The RowNumber is in the range [0, NRows-1].
 */
display_board_helper([], _, _) :- !.
display_board_helper([CurrRow|Rest], NRows, RowN) :-
    RowN >= 0,
    RowN < NRows,
    length(CurrRow, NCols),
    display_row(CurrRow, [NRows, NCols], RowN), 
    NextRow is RowN + 1,
    display_board_helper(Rest, NRows, NextRow).

/**
 * Display Row
 * display_row(+Row, +Dimensions, +RowNumber)
 * Display a row of the board. A row is composed by the content and the divisions around it.
 * The first row also displays the column identifiers, starting from 'a'.
 * 
 * Row -> List with a representation of a board row.
 * Dimensions -> Board dimensions.
 * RowNumber -> Number of the row that will be displayed.
 */
display_row(Row, [NRows, NCols], 0) :-
    write('  '), display_col_head(NCols, 0), nl,
    write('  '), display_top(NCols, 0), nl,
    display_row_content(Row, [NRows, NCols], 0),    
    write('  '), display_div(NCols, 0), nl, !.

display_row(Row, [NRows, NCols], RowN) :-
    RowN is NRows-1,
    display_row_content(Row, [NRows, NCols], RowN),
    write('  '), display_bottom(NCols, 0), nl, !.

display_row(Row, [NRows, NCols], RowN) :-
    RowN > 0,   
    RowN < NRows-1,
    display_row_content(Row, [NRows, NCols], RowN),
    write('  '), display_div(NCols, 0), nl.

/** 
 * Display Row Content
 * display_row_content(+Row, +Dimensions, +RowNumber)
 * Display the content of a row in the board, spanning 5 lines.
 * In each third line, the row identifier is also displayed
 * 
 * Row -> List with a representation of a board row.
 * Dimensions -> Board dimensions.
 * RowNumber -> Number of the row that will be displayed.
 */
display_row_content(Row, [NRows, NCols], RowNumber) :-
    LastRow is NRows-1,
    LastCol is NCols-1,
    write('  '), display_content_row_1(Row, [LastRow, LastCol], RowNumber, 0), nl,
    write('  '), display_content_row_2(Row, [LastRow, LastCol], RowNumber, 0), nl,
    ID is 97+RowNumber, ansi_format([fg(blue)], '~c', [ID]), write(' '), display_content_row_3(Row, [LastRow, LastCol], RowNumber, 0), nl,
    write('  '), display_content_row_4(Row, [LastRow, LastCol], RowNumber, 0), nl,
    write('  '), display_content_row_5(Row, [LastRow, LastCol], RowNumber, 0), nl.



/**
 * Display Content Row 1
 * display_content_row_1(+Row, +Dimensions, +RowNumber, +ColumnNumber)
 * Display the first line of the content of a row, iterating over its columns.
 * It displays the first line of the frogs, empty space or the middle separators.
 *
 * Row -> List representing a board row.
 * Dimensions -> Board dimensions [LastRow, LastCol].
 * RowNumber -> Number of the row to be displayed, in the range [0,LastRow]. Used to determine if row is at an edge of the board.
 * ColumnNumber -> Number of the column to be displayed, in the range [0,LastCol].
 */
display_content_row_1([], _, _, _) :- !.

display_content_row_1([Content|Rest], Dimensions, RowNumber, 0) :-    
    put_code(186), % ║ 
    display_content_ascii_1(Content, Dimensions, RowNumber, 0),
    put_code(186), % ║
    display_content_row_1(Rest, Dimensions, RowNumber, 1).

display_content_row_1([Content|Rest], [LastRow, LastCol], RowNumber, ColN) :-
    ColN > 0,
    ColN =< LastCol,
    display_content_ascii_1(Content, [LastRow, LastCol], RowNumber, ColN),
    put_code(186), % ║ 
    NextCol is ColN + 1,
    display_content_row_1(Rest, [LastRow, LastCol], RowNumber, NextCol).


/**
 * Display Content Row 2
 * display_content_row_2(+Row, +Dimensions, +RowNumber, +ColumnNumber)
 * Display the second line of the content of a row, iterating over its columns.
 * It displays the second line of the frogs, empty space or the middle separators.
 *
 * Row -> List representing a board row.
 * Dimensions -> Board dimensions [LastRow, LastCol].
 * RowNumber -> Number of the row to be displayed, in the range [0,LastRow]. Used to determine if row is at an edge of the board.
 * ColumnNumber -> Number of the column to be displayed, in the range [0,LastCol].
 */
display_content_row_2([], _, _, _) :- !.

display_content_row_2([Content|Rest], Dimensions, RowNumber, 0) :-    
    put_code(186), % ║ 
    display_content_ascii_2(Content, Dimensions, RowNumber, 0),
    put_code(186), % ║ 
    display_content_row_2(Rest, Dimensions, RowNumber, 1).


display_content_row_2([Content|Rest], [LastRow, LastCol], RowNumber, ColN) :-
    ColN > 0,
    ColN =< LastCol,
    display_content_ascii_2(Content, [LastRow, LastCol], RowNumber, ColN),
    put_code(186), % ║ 
    NextCol is ColN + 1,
    display_content_row_2(Rest, [LastRow, LastCol], RowNumber, NextCol).


/**
 * Display Content Row 3
 * display_content_row_3(+Row, +Dimensions, +RowNumber, +ColumnNumber)
 * Display the third line of the content of a row, iterating over its columns.
 * It displays the third line of the frogs, empty space or the separators.
 *
 * Row -> List representing a board row.
 * Dimensions -> Board dimensions [LastRow, LastCol].
 * RowNumber -> Number of the row to be displayed, in the range [0,LastRow]. Used to determine if row is at an edge of the board.
 * ColumnNumber -> Number of the column to be displayed, in the range [0,LastCol].
 */
display_content_row_3([], _, _, _) :- !.

display_content_row_3([Content|Rest], Dimensions, RowNumber, 0) :-    
    put_code(186), % ║ 
    display_content_ascii_3(Content, Dimensions, RowNumber, 0),
    put_code(186), % ║ 
    display_content_row_3(Rest, Dimensions, RowNumber, 1).


display_content_row_3([Content|Rest], [LastRow, LastCol], RowNumber, ColN) :-
    ColN > 0,
    ColN =< LastCol,
    display_content_ascii_3(Content, [LastRow, LastCol], RowNumber, ColN),
    put_code(186), % ║ 
    NextCol is ColN + 1,
    display_content_row_3(Rest, [LastRow, LastCol], RowNumber, NextCol).


/**
 * Display Content Row 4
 * display_content_row_4(+Row, +Dimensions, +RowNumber, +ColumnNumber)
 * Display the fourth line of the content of a row, iterating over its columns.
 * It displays the fourth line of the frogs, empty space or the separators.
 *
 * Row -> List representing a board row.
 * Dimensions -> Board dimensions [LastRow, LastCol].
 * RowNumber -> Number of the row to be displayed, in the range [0,LastRow]. Used to determine if row is at an edge of the board.
 * ColumnNumber -> Number of the column to be displayed, in the range [0,LastCol].
 */
display_content_row_4([], _, _, _) :- !.

display_content_row_4([Content|Rest], Dimensions, RowNumber, 0) :-    
    put_code(186), % ║ 
    display_content_ascii_4(Content, Dimensions, RowNumber, 0),
    put_code(186), % ║ 
    display_content_row_4(Rest, Dimensions, RowNumber, 1).

display_content_row_4([Content|Rest], [LastRow, LastCol], RowNumber, ColN) :-
    ColN > 0,
    ColN =< LastCol,
    display_content_ascii_4(Content, [LastRow, LastCol], RowNumber, ColN),
    put_code(186), % ║ 
    NextCol is ColN + 1,
    display_content_row_4(Rest, [LastRow, LastCol], RowNumber, NextCol).


/**
 * Display Content Row 5
 * display_content_row_5(+Row, +Dimensions, +RowNumber, +ColumnNumber)
 * Display the fifth line of the content of a row, iterating over its columns.
 * It displays the fifth line of the frogs, empty space or the separators.
 *
 * Row -> List representing a board row.
 * Dimensions -> Board dimensions [LastRow, LastCol].
 * RowNumber -> Number of the row to be displayed, in the range [0,LastRow]. Used to determine if row is at an edge of the board.
 * ColumnNumber -> Number of the column to be displayed, in the range [0,LastCol].
 */
display_content_row_5([], _, _, _) :- !.

display_content_row_5([Content|Rest], Dimensions, RowNumber, 0) :-    
    put_code(186), % ║ 
    display_content_ascii_5(Content, Dimensions, RowNumber, 0),
    put_code(186), % ║ 
    display_content_row_5(Rest, Dimensions, RowNumber, 1).

display_content_row_5([Content|Rest], [LastRow, LastCol], RowNumber, ColN) :-
    ColN > 0,
    ColN =< LastCol,
    display_content_ascii_5(Content, [LastRow, LastCol], RowNumber, ColN),
    put_code(186), % ║ 
    NextCol is ColN + 1,
    display_content_row_5(Rest, [LastRow, LastCol], RowNumber, NextCol).

/**
 * Display Column Header
 * display_col_head(+NCols, +ColumnNumber)
 * Displays the identifier of a column, iterating from column 0 to NCols-1.
 * 
 * NCols -> Total number of columns.
 * ColN -> Number of the column.
 */
display_col_head(NCols, ColN) :-
    ColN is NCols-1,
    write('        '),
    ansi_format([fg(blue)], '~w', [NCols]),
    write('       '), !.

display_col_head(NCols, ColN) :-
    ColN >= 0,
    ColN < NCols-1,
    N is ColN+1,
    write('        '),
    ansi_format([fg(blue)], '~w', [N]),
    write('       '),
    NextCol is ColN + 1,
    display_col_head(NCols, NextCol).


/**
 * Display Top of the Board
 * display_top(+NCols, +ColumnNumber)
 * Displays the top edge of the board.
 *
 * NCols -> Number of columns in the board.
 * ColumnNumber -> Number of the column. Used to iterate through the row.
 */
display_top(NCols, 0) :-
    put_code(201), %╔
    display_div_line(15),
    put_code(203), % ╦
    display_top(NCols, 1), !.

display_top(NCols, ColN) :-
    ColN is NCols-1,
    display_div_line(15),
    put_code(187), !. % ╗

display_top(NCols, ColN) :-
    ColN > 0,
    ColN < NCols-1,
    display_div_line(15),
    put_code(203), % ╦
    NextCol is ColN + 1,
    display_top(NCols, NextCol).


/**
 * Display Bottom of the Board
 * display_bottom(+NCols, +ColumnNumber)
 * Displays the bottom edge of the board.
 *
 * NCols -> Number of columns in the board.
 * ColumnNumber -> Number of the column. Used to iterate through the row.
 */
display_bottom(NCols, 0) :-
    put_code(200), %╚
    display_div_line(15),
    put_code(202), % ╩
    display_bottom(NCols, 1), !.

display_bottom(NCols, ColN) :-
    ColN is NCols-1,
    display_div_line(15),
    put_code(188), !. %╝

display_bottom(NCols, ColN) :-
    ColN > 0,
    ColN < NCols-1,
    display_div_line(15),
    put_code(202), % ╩
    NextCol is ColN + 1,
    display_bottom(NCols, NextCol).


/**
 * Display Division of the Board
 * display_div(+NCols, +ColumnNumber)
 * Displays a division inside the board, separating rows.
 *
 * NCols -> Number of columns in the board.
 * ColumnNumber -> Number of the column. Used to iterate through the row.
 */
display_div(NCols, 0) :-
    put_code(204), % ╠
    display_div_line(15),
    put_code(206), % ╬
    display_div(NCols, 1).

display_div(NCols, ColN) :-
    ColN is NCols-1,
    display_div_line(15),
    put_code(185), !. % ╣ 

display_div(NCols, ColN) :-
    ColN > 0,
    ColN < NCols-1,
    display_div_line(15),
    put_code(206), % ╬
    NextCol is ColN + 1,
    display_div(NCols, NextCol).

/**
 * Display Division Line
 * display_div_line(+Count)
 * Displays the character used for a division Count times. 
 *
 * Count -> Number of times to print the character, decrementing in each call.
 */
display_div_line(0) :- !.
display_div_line(Count) :-
    Count > 0,
    put_code(205), % ═
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
display_content_ascii_1(Content, [LastRow, LastCol], RowNumber, ColumnNumber) :-
    Content \= empty,
    (RowNumber = 0; RowNumber = LastRow; ColumnNumber = 0 ; ColumnNumber = LastCol),
    display_frog_ascii_1(Content, blue), !.

display_content_ascii_1(Content, [LastRow, LastCol], RowNumber, ColumnNumber) :-
    Content \= empty,
    (RowNumber > 0; RowNumber < LastRow; ColumnNumber > 0 ; ColumnNumber < LastCol),
    display_frog_ascii_1(Content, default), !.

display_content_ascii_1(Content, [LastRow, LastCol], RowNumber, ColumnNumber) :-
    Content = empty,
    (RowNumber = 0; RowNumber = LastRow; ColumnNumber = 0 ; ColumnNumber = LastCol),
    display_flower_ascii_1, !.

display_content_ascii_1(Content, [LastRow, LastCol], RowNumber, ColumnNumber) :-
    Content = empty,
    (RowNumber > 0; RowNumber < LastRow; ColumnNumber > 0 ; ColumnNumber < LastCol),
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
display_content_ascii_2(Content, [LastRow, LastCol], RowNumber, ColumnNumber) :-
    Content \= empty,
    (RowNumber = 0; RowNumber = LastRow; ColumnNumber = 0 ; ColumnNumber = LastCol),
    display_frog_ascii_2(Content, blue), !.

display_content_ascii_2(Content, [LastRow, LastCol], RowNumber, ColumnNumber) :-
    Content \= empty,
    (RowNumber > 0; RowNumber < LastRow; ColumnNumber > 0 ; ColumnNumber < LastCol),
    display_frog_ascii_2(Content, default), !.

display_content_ascii_2(Content, [LastRow, LastCol], RowNumber, ColumnNumber) :-
    Content = empty,
    (RowNumber = 0; RowNumber = LastRow; ColumnNumber = 0 ; ColumnNumber = LastCol),
    display_flower_ascii_2, !.

display_content_ascii_2(Content, [LastRow, LastCol], RowNumber, ColumnNumber) :-
    Content = empty,
    (RowNumber > 0; RowNumber < LastRow; ColumnNumber > 0 ; ColumnNumber < LastCol),
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
display_content_ascii_3(Content, [LastRow, LastCol], RowNumber, ColumnNumber) :-
    Content \= empty,
    (RowNumber = 0; RowNumber = LastRow; ColumnNumber = 0 ; ColumnNumber = LastCol),
    display_frog_ascii_3(Content, blue), !.
    
display_content_ascii_3(Content, [LastRow, LastCol], RowNumber, ColumnNumber) :-
    Content \= empty,
    (RowNumber > 0; RowNumber < LastRow; ColumnNumber > 0 ; ColumnNumber < LastCol),
    display_frog_ascii_3(Content, default), !.

display_content_ascii_3(Content, [LastRow, LastCol], RowNumber, ColumnNumber) :-
    Content = empty,
    (RowNumber = 0; RowNumber = LastRow; ColumnNumber = 0 ; ColumnNumber = LastCol),
    display_flower_ascii_3, !.

display_content_ascii_3(Content, [LastRow, LastCol], RowNumber, ColumnNumber) :-
    Content = empty,
    (RowNumber > 0; RowNumber < LastRow; ColumnNumber > 0 ; ColumnNumber < LastCol),
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
display_content_ascii_4(Content, [LastRow, LastCol], RowNumber, ColumnNumber) :-
    Content \= empty,
    (RowNumber = 0; RowNumber = LastRow; ColumnNumber = 0 ; ColumnNumber = LastCol),
    display_frog_ascii_4(Content, blue), !.
    
display_content_ascii_4(Content, [LastRow, LastCol], RowNumber, ColumnNumber) :-
    Content \= empty,
    (RowNumber > 0; RowNumber < LastRow; ColumnNumber > 0 ; ColumnNumber < LastCol),
    display_frog_ascii_4(Content, default), !.

display_content_ascii_4(Content, [LastRow, LastCol], RowNumber, ColumnNumber) :-
    Content = empty,
    (RowNumber = 0; RowNumber = LastRow; ColumnNumber = 0 ; ColumnNumber = LastCol),
    display_flower_ascii_4, !.

display_content_ascii_4(Content, [LastRow, LastCol], RowNumber, ColumnNumber) :-
    Content = empty,
    (RowNumber > 0; RowNumber < LastRow; ColumnNumber > 0 ; ColumnNumber < LastCol),
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
display_content_ascii_5(Content, [LastRow, LastCol], RowNumber, ColumnNumber) :-
    Content \= empty,
    (RowNumber = 0; RowNumber = LastRow; ColumnNumber = 0 ; ColumnNumber = LastCol),
    display_frog_ascii_5(Content, blue), !.
    
display_content_ascii_5(Content, [LastRow, LastCol], RowNumber, ColumnNumber) :-
    Content \= empty,
    (RowNumber > 0; RowNumber < LastRow; ColumnNumber > 0 ; ColumnNumber < LastCol),
    display_frog_ascii_5(Content, default), !.

display_content_ascii_5(Content, [LastRow, LastCol], RowNumber, ColumnNumber) :-
    Content = empty,
    (RowNumber = 0; RowNumber = LastRow; ColumnNumber = 0 ; ColumnNumber = LastCol),
    display_flower_ascii_5, !.

display_content_ascii_5(Content, [LastRow, LastCol], RowNumber, ColumnNumber) :-
    Content = empty,
    (RowNumber > 0; RowNumber < LastRow; ColumnNumber > 0 ; ColumnNumber < LastCol),
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
 * display_empty_line
 * Displays an empty line. Used to fill empty cells that are not outer cells of the board.
 */
display_empty_line :-
    write('               ').

/**
 * Display Flower Ascii 1
 * display_flower_ascii_1
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
 * display_flower_ascii_2
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
 * display_flower_ascii_3
 * Displays the third line of a flower.
 */
display_flower_ascii_3 :-
    ansi_format([fg(green), bg(blue)], '~w', ['  ']),
    ansi_format([fg(black), bg(green)], '~w', ['|    v    |']),
    ansi_format([fg(green), bg(blue)], '~w', ['  ']).

/**
 * Display Flower Ascii 4
 * display_flower_ascii_4
 * Displays the fourth line of a flower.
 */
display_flower_ascii_4 :-
    ansi_format([fg(green), bg(blue)], '~w', ['  ']),
    ansi_format([fg(black), bg(green)], '~w', ['|         |']),
    ansi_format([fg(green), bg(blue)], '~w', ['  ']).

/**
 * Display Flower Ascii 5
 * display_flower_ascii_5
 * Displays the fifth line of a flower.
 */
display_flower_ascii_5 :-
    ansi_format([fg(green), bg(blue)], '~w', ['   ']),
    ansi_format([fg(black), bg(green)], '~w', ['\\_______/']),
    ansi_format([fg(green), bg(blue)], '~w', ['   ']).

/**
 * Display position
 * display_position(+Msg, +Pos)
 * Displays Pos in the format (Row, Column), with Msg before.
 *
 * Msg -> message to prepend.
 * Pos -> Position to display.
 */
display_position(Msg, Pos) :-
    write(Msg),
    display_position(Pos),
    nl.

/**
 * Display position
 * display_position(+Pos)
 * Displays Pos in the format (Row, Column).
 *
 * Pos -> Position to display.
 */
display_position([Row,Col]) :-
    put_char('('),
    write(Row),
    put_char(','),
    write(Col),
    put_char(')').

/**
 * Display winner
 * display_winner(+Winner)
 * Displays the winner with colors.
 *
 * Winner -> Player that won the game.
 */
display_winner(Winner) :-
    player_color(Winner, Color),
    ansi_format([fg(Color)], 'Player ~d won!', [Winner]).

/**
 * Display the three game modes
 * display_game_modes
 */
display_game_modes :-   
    nl,
    write('Frog Chess has the following modes: '), nl, nl,
    write('\t1- Player vs. Player'), nl,
    write('\t2- Player vs. CPU'), nl,
    write('\t3- CPU vs. CPU'), nl, nl. 

/**
 * Display the game name
 * display_game_name
 */
display_game_name :-   
    ansi_format([fg(blue)], "/=================================================================\\", []), nl,
    ansi_format([fg(blue)], "|  ____  ____   ____   ____       ____          ____  ____  ____  |", []), nl,
    ansi_format([fg(blue)], "| |     |    | |    | |          |      |    | |     |     |      |", []), nl,
    ansi_format([fg(blue)], "| |__   |____| |    | |  __      |      |____| |__   |___  |___   |", []), nl,
    ansi_format([fg(blue)], "| |     |  \\   |____| |____|     |____  |    | |____ ____| ____|  |", []), nl,
    ansi_format([fg(blue)], "\\=================================================================/", []), nl.

/**
 * Display jump
 * display_jump(+Msg, +StartPos, +EndPos)
 * Displays a cpu jump and prepends given msg.
 *
 * Msg -> Message to preprend.
 * StartPos -> Jump start position.
 * EndPos -> Jump end position.
 */
display_jump(Msg, StartPos, EndPos) :- 
    nl, 
    write(Msg), 
    display_position(StartPos), 
    write(' to '), 
    display_position(EndPos), nl.

/**
 * Display CPU Jump
 * display_cpu_jump(+StartPos, +EndPos)
 * Displays a cpu jump.
 * 
 * StartPos -> Starting position.
 * EndPos -> End position.
 */
display_cpu_jump([StartRow, StartCol], [EndRow, EndCol]) :-
    index_to_row(StartRow, SRow),
    index_to_col(StartCol, SCol),
    index_to_row(EndRow, ERow),
    index_to_col(EndCol, ECol),
    display_jump('CPU jumped from ', [SRow, SCol], [ERow, ECol]).

/**
 * Display a error message
 * error_msg(+Msg)
 * Displays a error message on screen and fails.
 *
 * Msg -> Message to be displayed.
 */
error_msg(Msg) :-
    nl, write(Msg), nl, nl, fail.

/**
 * Display AI levels
 * display_ai_levels
 */
display_ai_levels :-
    write('Choose one of the following CPU levels:'), nl,
    write('\t1- Easy'), nl,
    write('\t2- Medium'), nl,
    write('\t3- Hard'), nl,
    write('\t4- Expert'), nl, nl.

/**
 * Display Menu Options
 * display_menu_options
 */
display_menu_options :-
    nl, nl,
    ansi_format([fg(blue)], " MAIN MENU ", []), nl, nl,
    
    write(' 1- Play Game'), nl,
    write(' 2- Instructions'), nl,
    write(' 3- Credits'), nl,
    write(' 4- Exit'), nl, nl.

/**
 * Display Thank You Message
 * display_thank_you_msg
 * Displays a thank you message in the end of the game
 */
display_thank_you_msg :-
    nl, nl,
    ansi_format([fg(blue)], "Thanks for playing Frog Chess!", []), nl, 
    ansi_format([fg(blue)], "Hope you enjoyed it!", []), nl, nl.

/**
 * Display Instructions
 * display_instructions
 */
display_instructions :-
    nl, nl,
    ansi_format([bold], "Last Frog to Jump Wins!", []), nl, nl,
    write('Frog Chess is played with two players.'), nl, 
    write('The objective is to be the last frog to jump.'), nl,
    write('Players start by strategically placing their frogs on the board until all the spaces are filled with frogs.'), nl,
    write('Players then take turns jumping one another.'), nl,
    write('When a frog is jumped, it is removed from the board.'), nl,
    write('Frogs can jump forward, backwards, sideways and diagonally.'), nl,
    write('Frogs can do single or multiple jumps, even jump their own team frogs if needed.'), nl,
    write('It is possible to win the game with fewer frogs left on the board than their opponents.'), nl,
    write('The game moves quickly, and you will find yourself wanting to play again...'), nl, nl.

/**
 * Display Credits
 * display_credits
 */
display_credits :-
    nl, nl,
    write('Game developed by: '), nl,
    write('\tMario Gil Marinho Mesquita'), nl,
    write('\tPedro Miguel Rodrigues Ferraz Esteves'), nl, nl,
    write('Based on the Frog Chess board game by '), 
    ansi_format([bold], 'Binary Cocoa, LLC', []), write('!'), nl, nl. 

/**
 * Display CPU thinking message
 * display_cpu_think_msg(+CPU)
 *
 * CPU -> CPU that is thinking
 */
display_cpu_think_msg(CPU) :-
    nl,
    player_color(CPU, Color), 
    ansi_format([fg(Color)], 'CPU ~d is thinking...', [CPU]), nl, nl.