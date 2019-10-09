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
    display_board(Board).


display_board([X| Y]) :-
    put_code(201),
    put_code(205), 
    display_first_line(X),
    display_rest_board(Y).


display_rest_board([]) :- 
    put_code(200), 
    put_code(205),
    display_last_line(7).

display_rest_board([L1|L]) :-
    put_code(186),
    display_line(L1),
    nl,
    (
        L \= [],
        display_empty_line(L1),
        nl;
        L = []
    ),
    display_rest_board(L).


display_first_line([]) :- put_code(187).
display_first_line([X|Y]) :-
    put_code(203),
    put_code(205),
    display_first_line(Y).


display_last_line(0) :- put_code(188).
display_last_line(N) :-
    put_code(202),
    put_code(205),
    N1 is N - 1,
    display_last_line(N1).


display_line([]).

display_line([X|Y]) :-
    display_player_frog(X),
    put_code(186),
    display_line(Y).

display_empty_line([]).
display_empty_line([X|Y]) :-
    put_code(205),
    put_code(206),
    display_empty_line(Y).


display_player_frog(X) :-
    X = empty,
    put_char(' ').

display_player_frog(X) :-
    X = yellow,
    put_char('Y').

display_player_frog(X) :-
    X = pink,
    put_char('P').
