:- include('game_state.pl').
:- include('display.pl').
:- include('input.pl').

/** 
 * Player Frog
 * player_frog(+Number, -player_frog)
 * Associates a frog to each player number.
 *
 * Number -> Number of the player. Since for now we only consider 2 players, it is either 1 or 2.
 * player_frog -> Returns the frog associated with the player number.
 */
player_frog(1, blue).
player_frog(2, yellow).

next_player(1, 2).
next_player(2, 1).

/** 
 * Valid position
 * valid_position(+Position)
 * Checks if a given position is valid, that is, inside a 8x8 0-indexed board
 *
 * Position -> Position to check, in the format [Row, Column].
 */
valid_position([0, 0]).
valid_position([1, 0]).
valid_position([2, 0]).
valid_position([3, 0]).
valid_position([4, 0]).
valid_position([5, 0]).
valid_position([6, 0]).
valid_position([7, 0]).
valid_position([0, 1]).
valid_position([1, 1]).
valid_position([2, 1]).
valid_position([3, 1]).
valid_position([4, 1]).
valid_position([5, 1]).
valid_position([6, 1]).
valid_position([7, 1]).
valid_position([0, 2]).
valid_position([1, 2]).
valid_position([2, 2]).
valid_position([3, 2]).
valid_position([4, 2]).
valid_position([5, 2]).
valid_position([6, 2]).
valid_position([7, 2]).
valid_position([0, 3]).
valid_position([1, 3]).
valid_position([2, 3]).
valid_position([3, 3]).
valid_position([4, 3]).
valid_position([5, 3]).
valid_position([6, 3]).
valid_position([7, 3]).
valid_position([0, 4]).
valid_position([1, 4]).
valid_position([2, 4]).
valid_position([3, 4]).
valid_position([4, 4]).
valid_position([5, 4]).
valid_position([6, 4]).
valid_position([7, 4]).
valid_position([0, 5]).
valid_position([1, 5]).
valid_position([2, 5]).
valid_position([3, 5]).
valid_position([4, 5]).
valid_position([5, 5]).
valid_position([6, 5]).
valid_position([7, 5]).
valid_position([0, 6]).
valid_position([1, 6]).
valid_position([2, 6]).
valid_position([3, 6]).
valid_position([4, 6]).
valid_position([5, 6]).
valid_position([6, 6]).
valid_position([7, 6]).
valid_position([0, 7]).
valid_position([1, 7]).
valid_position([2, 7]).
valid_position([3, 7]).
valid_position([4, 7]).
valid_position([5, 7]).
valid_position([6, 7]).
valid_position([7, 7]).

/** 
 * Valid fill position
 * valid_fill_position(+Position)
 * Checks if a given position is valid to fill with a frog in the beginning of the game
 *
 * Position -> Position to check, in the format [Row, Column].
 */
valid_fill_position([Row, Column]) :-
    Row > 0, Row < 7,
    Column > 0, Column < 7.

/**
 * Display a error message
 * error_msg(+Msg)
 * Displays a error message on screen and fails.
 *
 * Msg -> Message to be displayed
 */
error_msg(Msg) :-
    nl, write(Msg), nl, nl, fail.

/**
 * Initialize board
 * init_board(-Board, +FirstPlayer)
 * Creates a new board filled with frogs.
 * 
 * B -> Variable to return created board.
 * FirstPlayer -> First player to put a frog
 */
init_board(B, FirstPlayer) :-
    emptyBoard(InitBoard),
    fill_board(InitBoard, FirstPlayer, 0, B).

/**
 * Fill board
 * fill_board(+Board, +Player, +Frogs, -NewBoard)
 * Fills the board with frogs.
 * 
 * Board -> Board to fill.
 * Player -> Number of the player that will fill that next position
 * Frogs -> Number of frogs that are already on the board
 * NewBoard -> Board filled with frogs
 */
fill_board(Board, _, 36, Board).

fill_board(Board, Player, Frog, NewBoard) :-
    display_board(Board), !,
    display_fill_turn(Player), !,
    repeat,
        (
            read_position('Frog Position? ', Pos),
            get_position(Board, Pos, empty),
            valid_fill_position(Pos),
            !;
            error_msg('Invalid position!')
        ),
    player_frog(Player, Value),
    set_position(Board, Pos, Value, IntBoard),
    next_player(Player, NextPlayer),
    NextFrog is Frog + 1,
    fill_board(IntBoard, NextPlayer, NextFrog, NewBoard).    

/**
 * Get Target Row
 * get_target_row(+Board, +InRow, -OutRow)
 * Returns the row of index InRow from Board in OutRow.
 * Uses InRow as a counter and returns current value when it reaches 0.
 * 
 * Board -> List of rows.
 * InRow -> Target row to find.
 * OutRow -> Variable to return target row.
 */
get_target_row([TargetRow | _], 0, TargetRow).
get_target_row([_ | Rest], InRow, OutRow) :-
    InRow > 0,
    N is InRow-1,
    get_target_row(Rest, N, OutRow).

/**
 * Get Value in Row
 * get_value_in_row(+Row, +Col, -Value)
 * Returns the value in index Col of given Row, in the variable Value.
 * Uses Col as a counter and returns current value when it reaches 0.
 * 
 * Row -> Row to search value.
 * Col -> Target column to find.
 * Value -> Variable to return target value.
 */
get_value_in_row([TargetVal | _], 0, TargetVal).
get_value_in_row([_ | Rest], Col, Value) :-
    Col > 0,
    N is Col-1,
    get_value_in_row(Rest, N, Value).

/**
 * Get position
 * get_position(+Board, +Position, -Value)
 * Returns the value in position [Row, Col] in the given board.
 * 
 * Board -> List of lists representing the board.
 * Position -> Position in the board, containing the values [Row, Col].
 * Value -> Variable to return target value.
 */
get_position(Board, [Row, Col], Value) :-
    get_target_row(Board, Row, TargetRow),
    get_value_in_row(TargetRow, Col, Value).


/**
 * Set new row
 * set_new_row(+Row, +TargetCol, +NewValue, +CurrColI, -NewRow)
 * Sets the value from Row at index TargetCol to NewValue. The rest of the row is copied.
 * Iterates over the row columns, copying them to NewRow, except in the case that it
 * finds the target column.
 * 
 * Row -> Row to iterate over.
 * TargetCol -> Target column.
 * NewValue -> New value to be added.
 * CurrColI -> Column iterator, from 0 to 7.
 * NewRow -> Returns the modified row.
 */
set_new_row(_, _, _, 8, []).

set_new_row([_ | Rest], TargetCol, NewValue, TargetCol, [NewValue | NewRow]) :-
    NextColI is TargetCol+1,
    set_new_row(Rest, TargetCol, NewValue, NextColI, NewRow).

set_new_row([CurrVal | Rest], TargetCol, NewValue, ColI, [CurrVal | NewRow]) :-
    TargetCol \= ColI, ColI >= 0, ColI =< 7,
    NextColI is ColI+1,
    set_new_row(Rest, TargetCol, NewValue, NextColI, NewRow).


/**
 * Set position helper
 * set_position_helper(+Board, +TargetPosition, +NewValue, +CurrRowI, -NewBoard)
 * Changes the board position given by Target_position, [Row, Col], to the NewValue.
 * The rest of the board is copied.
 * Iterates over the board rows, copying them to the NewBoard, except in the case that
 * it finds the target row to be changed.
 * 
 * Board -> Original board to be changed.
 * TargetPosition -> Target position.
 * NewValue -> New value to be added.
 * CurrRowI -> Row iterator, from 0 to 7.
 * NewBoard -> Returns the modified board.
 */
set_position_helper(_, _, _, 8, []).

set_position_helper([CurrRow | Rest], [TargetRow, TargetCol], NewValue, TargetRow, [NewRow | NewBoard]) :-
    set_new_row(CurrRow, TargetCol, NewValue, 0, NewRow),
    NextRowI is TargetRow+1,
    set_position_helper(Rest, [TargetRow, TargetCol], NewValue, NextRowI, NewBoard).

set_position_helper([CurrRow | Rest], [TargetRow, TargetCol], NewValue, RowI, [CurrRow | NewBoard]) :-
    TargetRow \= RowI, RowI >= 0, RowI =< 7,
    NextRowI is RowI+1,
    set_position_helper(Rest, [TargetRow, TargetCol], NewValue, NextRowI, NewBoard).

/**
 * Set position
 * set_position(+Board, +Position, +NewValue, -NewBoard)
 * Changes the board position given by (Row, Col) to the NewValue.
 * 
 * Board -> Original board to be changed.
 * Position -> Position in the board to be changed, [Row , Column].
 * NewValue -> New value to be added.
 * NewBoard -> Returns the modified board.
 */
set_position(Board, Pos, NewValue, NewBoard) :-
    set_position_helper(Board, Pos, NewValue, 0, NewBoard).



/**
 * Valid jump
 * valid_jump(+StartPosition, -EndPosition)
 * Generates all valid jumping positions from StartPosition.
 * Jumps can be horizontal, vertical or diagonal, 2 positions from the starting one.
 * 
 * StartPosition -> Starting position.
 * EndPosition -> Ending position.
 */
valid_jump([SRow, SCol], [ERow, ECol]) :-
    ERow is SRow,
    ECol is SCol+2,
    valid_position([ERow, ECol]).
    
valid_jump([SRow, SCol], [ERow, ECol]) :-
    ERow is SRow,
    ECol is SCol-2,
    valid_position([ERow, ECol]).
    
valid_jump([SRow, SCol], [ERow, ECol]) :-
    ERow is SRow+2,
    ECol is SCol,
    valid_position([ERow, ECol]).
    
valid_jump([SRow, SCol], [ERow, ECol]) :-
    ERow is SRow-2,
    ECol is SCol,
    valid_position([ERow, ECol]).
    
valid_jump([SRow, SCol], [ERow, ECol]) :-
    ERow is SRow-2,
    ECol is SCol-2,
    valid_position([ERow, ECol]).
    
valid_jump([SRow, SCol], [ERow, ECol]) :-
    ERow is SRow+2,
    ECol is SCol-2,
    valid_position([ERow, ECol]).
    
valid_jump([SRow, SCol], [ERow, ECol]) :-
    ERow is SRow-2,
    ECol is SCol+2,
    valid_position([ERow, ECol]).
    
valid_jump([SRow, SCol], [ERow, ECol]) :-
    ERow is SRow+2,
    ECol is SCol+2,
    valid_position([ERow, ECol]).


/**
 * Middle Position
 * middle_position(+StartPos, +EndPosition, -MidPosition )
 * Determines the position between Start and End.
 * 
 * StartPosition -> Starting position.
 * EndPosition -> Ending position.
 * MidPosition -> Position between Start and End.
 */
middle_position([SRow,SCol], [ERow,ECol], [MRow,MCol]) :-
    MRow is (SRow+ERow)/2,
    MCol is (SCol+ECol)/2.


/**
 * Frog can jump
 * frog_can_jump(+Board, +FrogPosition)
 * Checks if a frog in a given position can jump in any direction.
 * 
 * Board -> Game board.
 * FrogPosition -> Frog position.
 */
frog_can_jump(Board, FrogPos) :-
    % look through all valid jumps
    valid_jump(FrogPos, Dest),

    % check if dest is empty
    get_position(Board, Dest, empty),

    % determine middle position
    middle_position(FrogPos, Dest, MidPos),

    % check if there is a frog in middle position
    get_position(Board, MidPos, MidFrog),
    player_frog(_, MidFrog).

/**
 * Jump
 * jump(+InputBoard, +StartPos, +MidPosition, +EndPosition, +Frog, -OutputBoard)
 * Jumps a frog from starting position to end position, updating all required cells.
 * Does not perform any validation.
 * 
 * InputBoard -> Initial board before jumping.
 * StartPosition -> Starting position.
 * MidPosition -> Position between Start and End.
 * EndPosition -> Ending position.
 * Frog -> Frog that is jumping.
 * OutputBoard -> Modified board after jumping.
 */
jump(InBoard, StartPos, MidPos, EndPos, Frog, OutBoard) :-
    
    % set middle position empty
    set_position(InBoard, MidPos, empty, NewBoard1),

    % set initial position empty
    set_position(NewBoard1, StartPos, empty, NewBoard2),

    % set end position to player frog
    set_position(NewBoard2, EndPos, Frog, OutBoard).


read_end_position(Board, InitPos, MidPos, EndPos) :-
    
    (
        read_position('Position to jump? ', EndPos),
        get_position(Board, EndPos, empty),
        valid_jump(InitPos, ValidPos),
        ValidPos = EndPos, 
        !;

        error_msg('Invalid jump destination')
    ),

    (
        middle_position(InitPos, EndPos, MidPos),
        get_position(Board, MidPos, MidFrog),
        player_frog(_, MidFrog),
        !;
        error_msg('Frogs must jump over other frogs!')    
    ).


/**
 * Read jump positions
 * read_jump_positions(+Board, +Player, -StartPos, -MidPosition, -EndPosition, -Frog)
 * Asks the user for input regarding the positions for the frog jump.
 * Asks for start position and end position, until a valid option is provided.
 * 
 * Board -> Game board.
 * Player -> Player of current turn.
 * StartPosition -> Starting position.
 * MidPosition -> Position between Start and End.
 * EndPosition -> Ending position.
 * Frog -> Frog that is jumping.
 */
read_jump_positions(Board, Player, InitPos, MidPos, EndPos, Frog) :-
    % starting position
    repeat,
        (
            read_position('Frog to jump? ', InitPos), 
            get_position(Board, InitPos, Frog),
            player_frog(Player, Frog),
            frog_can_jump(Board, InitPos),
            !;
            error_msg('Invalid start position!')
        ),

    % end position
    read_end_position(Board, InitPos, MidPos, EndPos).


/**
 * Find Jumpable Frog In Row
 * jumpable_frog_in_row(+Board, +Player, +Pos)
 * Checks if any frog in current Row can jump, iterating over columns.
 * 
 * Board -> Game board.
 * Player -> Player in question.
 * Pos -> Position currently checking.
 */
jumpable_frog_in_row(_, _, [_,8]) :- !, fail.

jumpable_frog_in_row(Board, Player, Pos) :-
    player_frog(Player, Frog),
    get_position(Board, Pos, Frog),
    frog_can_jump(Board, Pos), !.

jumpable_frog_in_row(Board, Player, [RowI, ColI]) :-
    NextColI is ColI+1,
    jumpable_frog_in_row(Board, Player, [RowI, NextColI]).



/**
 * Find Jumpable Frog
 * find_jumpable_frog(+Board, +Player, +RowI)
 * Searches for a frog of player Player that can jump in the board,
 * iterating over the rows.
 * 
 * Board -> Game board.
 * Player -> Player in question.
 * RowI -> Index of current row.
 */
find_jumpable_frog(_, _, 8) :- !, fail.

find_jumpable_frog(Board, Player, RowI) :-
    jumpable_frog_in_row(Board, Player, [RowI, 0]), !.

find_jumpable_frog(Board, Player, RowI) :-
    NextI is RowI+1,
    find_jumpable_frog(Board, Player, NextI).


/**
 * Game over
 * game_over(+Board, +LastPlayer, -Winner)
 * Checks if the game has ended and returns the Winner, given 
 * that the last player jumping was LastPlayer.
 * 
 * Board -> Game board.
 * LastPlayer -> Player which last played.
 * Winner -> Player who won.
 */
game_over(Board, 1, 1) :- \+find_jumpable_frog(Board, 2, 0), !.
game_over(Board, 2, 2) :- \+find_jumpable_frog(Board, 1, 0), !.
game_over(Board, 1, 2) :- \+find_jumpable_frog(Board, 1, 0), !.
game_over(Board, 2, 1) :- \+find_jumpable_frog(Board, 2, 0), !.


/**
 * Remove row outer frogs
 * remove_row_outer_frogs(+CurrRow, +Positon, -NewRow)
 * Iterates over the columns of a row, emptying the positions on the board edges.
 * 
 * CurrRow -> Initial row.
 * Positon -> Current position.
 * NewRow -> Modified row.
 */
remove_row_outer_frogs(_, [_, 8], []) :- !.

remove_row_outer_frogs([_ | Rest], [0, ColI], [empty | NewRow]) :-
    NextCol is ColI+1,
    remove_row_outer_frogs(Rest, [0, NextCol], NewRow).
    
remove_row_outer_frogs([_ | Rest], [7, ColI], [empty | NewRow]) :-
    NextCol is ColI+1,
    remove_row_outer_frogs(Rest, [7, NextCol], NewRow).

remove_row_outer_frogs([_ | Rest], [RowI, 0], [empty | NewRow]) :-
    remove_row_outer_frogs(Rest, [RowI, 1], NewRow).

remove_row_outer_frogs([_ | Rest], [RowI, 7], [empty | NewRow]) :-
    remove_row_outer_frogs(Rest, [RowI, 8], NewRow).

remove_row_outer_frogs([CurrVal | Rest], [RowI, ColI], [CurrVal | NewRow]) :-
    RowI > 0, RowI < 7,
    ColI > 0, ColI < 7,
    NextCol is ColI+1,
    remove_row_outer_frogs(Rest, [RowI, NextCol], NewRow).


/**
 * Remove outer frogs helper
 * remove_outer_frogs_helper(+InBoard, +RowI, -OutBoard)
 * Removes frogs from InBoard in outer positions, iterating over all the rows.
 * In each iteration, appends a modified row to the OutBoard.
 * 
 * InBoard -> Initial board.
 * RowI -> Current row.
 * OutBoard -> Modified board.
 */
remove_outer_frogs_helper([], 8, []) :- !.

remove_outer_frogs_helper([CurrRow | Rest], RowI, [NewRow | NewBoard]) :-
    remove_row_outer_frogs(CurrRow, [RowI, 0], NewRow),
    NextRow is RowI+1,
    remove_outer_frogs_helper(Rest, NextRow, NewBoard).

/**
 * Remove outer frogs
 * remove_outer_frogs(+InBoard, -OutBoard)
 * Removes frogs from InBoard in outer positions.
 * 
 * InBoard -> Initial board.
 * OutBoard -> Modified board.
 */
remove_outer_frogs(InBoard, OutBoard) :-
    remove_outer_frogs_helper(InBoard, 0, OutBoard).

/**
 * Continue Jumping
 * continue_jumping(+InBoard, +Player, +FrogPosition, +JumpN, -OutBoard)
 * Checks if the frog at position FrogPosition can continue jumping, and allows
 * the user to continue or not jumping with that frog.
 * Intended to be called after the frog has made 1 jump.
 *
 * InBoard -> Initial board.
 * Player -> Current player turn.
 * FrogPosition -> Position of frog that jumped.
 * JumpN -> Jump number in this turn.
 * OutBoard -> Modified board if continued jumping, initial board otherwise.
 */
continue_jumping(InBoard, Player, FrogPos, _, OutBoard) :-
    \+frog_can_jump(InBoard, FrogPos),

    write('Current frog can\'t jump again.'), nl,
    next_player(Player, NextPlayer),
    write('Player '), write(NextPlayer), write(' turn.'), nl, 

    OutBoard = InBoard,
    !,
    wait_for_input.

continue_jumping(InBoard, Player, [FrogRow, FrogCol], JumpN, OutBoard) :-
    repeat,
        nl, ask_yn_question('Jump again? (y/n) : ', Answer), nl,        
        (
            Answer = 'y', 
            % tell user where current frog is
            index_to_row(FrogRow, Row),
            index_to_col(FrogCol, Col),
            nl, display_position('Frog at position: ', [Row, Col]),

            % read jump destination
            read_end_position(InBoard, [FrogRow, FrogCol], MidPos, EndPos),
            get_position(InBoard, [FrogRow, FrogCol], Frog),
            
            jump(InBoard, [FrogRow, FrogCol], MidPos, EndPos, Frog, NewBoard),
            
            % display updated board
            display_game(NewBoard, Player, JumpN),

            % keep jumping
            NextJumpN is JumpN+1,
            continue_jumping(NewBoard, Player, EndPos, NextJumpN, OutBoard),                        
            !;

            Answer = 'n', 
            OutBoard = InBoard, 
            !,
            next_player(Player, NextPlayer),
            nl, write('Player '), write(NextPlayer), write(' turn.'), nl, 
            wait_for_input
        ).

/**
 * Player turn
 * player_turn(+InBoard, +Player, -OutBoard)
 * Performs a player controlled turn.
 *
 * InBoard -> Initial board.
 * Player -> Current player turn.
 * OutBoard -> Modified board after turn ends.
 */
player_turn(InBoard, Player, OutBoard) :-
    % read jump positions until valid
    repeat,
        read_jump_positions(InBoard, Player, InitPos, MidPos, EndPos, Frog), !,

    jump(InBoard, InitPos, MidPos, EndPos, Frog, NewBoard),

    display_game(NewBoard, Player, 1),

    continue_jumping(NewBoard, Player, EndPos, 2, OutBoard).

/**
 * Player vs Player game
 * pvp_game(+InBoard, +Player, -Winner)
 * Plays a pvp game with the given InBoard and starting player Player.
 *
 * InBoard -> Initial board.
 * Player -> Current player turn.
 * Winner -> Player who wins the game.
 */
pvp_game(InBoard, Player, Winner) :-
    display_game(InBoard, Player, 0),
    player_turn(InBoard, Player, MidBoard),
    remove_outer_frogs(MidBoard, FinalBoard),
    (
        game_over(FinalBoard, Player, Winner),
        display_game(FinalBoard, empty, 0);

        next_player(Player, NextPlayer),
        pvp_game(FinalBoard, NextPlayer, Winner)
    ), !.

/**
 * Player vs Player
 * player_vs_player
 * Starts a 2 human player game.
 */
player_vs_player :-
    random_between(1, 2, FirstPlayer),
    init_board(InitialBoard, FirstPlayer),
    pvp_game(InitialBoard, FirstPlayer, Winner),
    nl, 
    display_winner(Winner).


play_game :-
    player_vs_player.
    

%%%%%%%%%%%%%%%%%%%%
%                  %
%        AI        %
%                  %
%%%%%%%%%%%%%%%%%%%%

/**
 * generate_move(+Board, +Frog, -Move)
 *
 * Board -> Initial Board
 * Player -> Player Frog
 * Move -> List with all the jump positions of a move
 */
/*generate_move(Board, Frog, Move) :-
    get_position(Board, Pos, Frog)
    valid_position(Pos),*/


%TODO ACABAR