:- include('game_state.pl').

/* Display Game 
 * display_game(+Board, +Player)
 *
 * Board -> List with the board representation
 * Player -> Number of the player to make the next move
 * Characters used for representation: 
 *          - Y -> frog from the yellow player
 *          - P -> frog from the pink player
 *          - Other caracters are used to represent the board 
 */
display_game(Board, Player) :-
    initialBoard(Board),
    display_board(Board).

display_board(Board) :- display_board_helper(Board, 0).

/*

*/
display_board_helper([], _).
display_board_helper([Curr_Row|Rest], RowN) :-
    RowN < 8,
    display_row(Curr_Row, RowN), 
    NextRow is RowN + 1,
    display_board_helper(Rest, NextRow).

/*

*/
display_row(Row, 0) :-
    write(' '),
    display_col_head(0), nl,
    write(' '),
    display_top(0), nl,
    write(1),
    display_frog_row(Row, 0), nl,
    write(' '),
    display_div(0), nl.

display_row(Row, 7) :-
    write(8),
    display_frog_row(Row, 0), nl,
    write(' '),
    display_bottom(0), nl.

display_row(Row, RowN) :-
    RowN > 0,   
    RowN < 7,
    N is RowN+1,
    write(N),
    display_frog_row(Row, 0), nl,
    write(' '),
    display_div(0), nl.

/*

*/
display_frog_row([], _).

display_frog_row([Frog|Rest], 0) :-
    put_code(186),
    display_player_frog(Frog),
    put_code(186),
    display_frog_row(Rest, 1).

display_frog_row([Frog|Rest], ColN) :-
    ColN > 0,
    ColN < 8,
    display_player_frog(Frog),
    put_code(186),
    NextCol is ColN + 1,
    display_frog_row(Rest, NextCol).

/*

*/

display_col_head(7) :-
    put_char(' '),
    write(8),
    put_char(' ').

display_col_head(ColN) :-
    ColN >= 0,
    ColN < 7,
    N is ColN+1,
    put_char(' '),
    write(N),
    NextCol is ColN + 1,
    display_col_head(NextCol).


/*

*/
display_top(0) :-
    put_code(201),
    put_code(205), 
    put_code(203),
    display_top(1).

display_top(7) :-
    put_code(205), 
    put_code(187).

display_top(ColN) :-
    ColN > 0,
    ColN < 7,
    put_code(205),
    put_code(203),
    NextCol is ColN + 1,
    display_top(NextCol).


/*

*/
display_bottom(0) :-
    put_code(200), 
    put_code(205),
    put_code(202),
    display_bottom(1).

display_bottom(7) :-
    put_code(205),
    put_code(188).

display_bottom(ColN) :-
    ColN > 0,
    ColN < 7,
    put_code(205),
    put_code(202),
    NextCol is ColN + 1,
    display_bottom(NextCol).


/*

*/
display_div(0):- 
    put_code(204),
    put_code(205),
    put_code(206),
    display_div(1).
    
display_div(7) :-
    put_code(205),
    put_code(185).

display_div(ColN) :-
    ColN > 0,
    ColN < 7,
    put_code(205),
    put_code(206),
    NextCol is ColN + 1,
    display_div(NextCol).


/*

*/
display_player_frog(X) :-
    X = empty,
    put_char(' ').

display_player_frog(X) :-
    X = yellow,
    put_char('Y').

display_player_frog(X) :-
    X = pink,
    put_char('P').