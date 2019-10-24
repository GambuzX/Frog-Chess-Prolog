:- include('game_state.pl').
:- include('display.pl').

repeat.
repeat :- repeat.

play_game :-
    initialBoard(B),
    display_game(B, 1, 1).