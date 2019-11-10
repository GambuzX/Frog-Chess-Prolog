:- use_module(library(lists)).
:- include('game_state.pl').
:- include('display.pl').
:- include('input.pl').
:- include('board.pl').

/** 
 * Valid game mode
 * valid_game_mode(+Mode)
 * Checks if a given mode is valid 
 *
 * Mode -> Mode to check
 */
valid_game_mode(Mode) :-
    Mode >= 0,
    Mode =< 2.

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
 * Initialize game dimensions
 * init_game_dimensions(-Rows, -Columns)
 * Asks the user to init game dimensions.
 *
 * Rows -> Number of rows in the board.
 * Columns -> Number of columns in the board.
 */
init_game_dimensions(Rows, Columns) :-
    ansi_format([fg(blue)], 'CONFIGURE THE BOARD DIMENSIONS', []), nl,
    write('8x8 board  is recommended.'), nl,
    repeat,
        nl, read_game_dimensions(Rows, Columns), !.

/**
 * Create empty row
 * append_rows(+Columns, -OutRow)
 * Creates an empty row of size Columns
 * 
 * Columns -> Number of columns in the row.
 * OutRow -> Variable to return created empty row.
 */
create_empty_row(0, []) :- !.
create_empty_row(Columns, [empty | EmptyRow]) :-
    Count is Columns-1,
    create_empty_row(Count, EmptyRow).

/**
 * Append rows
 * append_rows(+Rows, +EmptyRow, -OutBoard)
 * Creates a new board by appending EmptyRow in an empty list Rows times.
 * 
 * Rows -> Number of rows in the board.
 * EmptyRow -> Row to append.
 * OutBoard -> Variable to return created empty board.
 */
append_rows(0, _, []) :- !.
append_rows(Rows, EmptyRow, [EmptyRow | OutBoard]) :-
    Count is Rows-1,
    append_rows(Count, EmptyRow, OutBoard).

/**
 * Create empty board
 * create_empty_board(+Rows, +Columns, -OutBoard)
 * Creates a new empty board of given dimensions.
 * 
 * Rows -> Number of rows in the board.
 * Columns -> Number of columns in the board.
 * OutBoard -> Variable to return created empty board.
 */
create_empty_board(Rows, Columns, OutBoard) :-
    create_empty_row(Columns, EmptyRow),
    append_rows(Rows, EmptyRow, OutBoard).


/**
 * Initialize board
 * init_board(+FirstPlayer, +TypeOfGame, -Board, -Rows, -Columns)
 * Creates a new board filled with frogs.
 * 
 * B -> Variable to return created board.
 * FirstPlayer -> First player to put a frog.
 * TypeOfGame -> Indicates the type of game (0 - player vs player; 1 - player vs computer).
 * Rows -> Number of rows in the board.
 * Columns -> Number of columns in the board.
 */
init_board(FirstPlayer, TypeOfGame, B) :-
    init_game_dimensions(Rows, Columns),
    create_empty_board(Rows, Columns, InitBoard),
    ansi_format([fg(blue)], 'BEFORE THE GAME STARTS, THE BOARD MUST BE FILLED WITH FROGS', []), nl,
    write('Choose your positions!'), nl, nl, wait_for_input,
    fill_board(InitBoard, FirstPlayer, 0, TypeOfGame, B).

/**
 * Fill board
 * fill_board(+Board, +Player, +Frogs, +TypeOfGame, -NewBoard)
 * Fills the board with frogs.
 * 
 * Board -> Board to fill.
 * Player -> Number of the player that will fill that next position
 * Frogs -> Number of frogs that are already on the board
 * TypeOfGame -> Indicates the type of game (0 - player vs player; 1 - player vs cpu; 2 - cpu vs cpu)
 * NewBoard -> Board filled with frogs
 */
fill_board(Board, _, FrogCount, _, Board) :-
    [FirstRow | _] = Board,
    length(Board, NRows),
    length(FirstRow, NCols),
    FrogCount is (NCols-2)*(NRows-2),

    display_board(Board), !,
    nl, ansi_format([fg(blue)], 'STARTING THE GAME', []), nl, wait_for_input, nl.

fill_board(Board, Player, Frog, TypeOfGame, NewBoard) :-
    (
        TypeOfGame = 0; % In player vs player mode, there will always be a player choosing the frog position 
        TypeOfGame = 1, Player = 1 % In player vs cpu mode, the first player is the person that will choose the frog
    ),
    display_board(Board), !,
    display_fill_turn(Player), !,
    player_fill_choose(Board, Pos),
    player_frog(Player, Value),
    set_position(Board, Pos, Value, IntBoard),
    next_player(Player, NextPlayer),
    NextFrog is Frog + 1,
    fill_board(IntBoard, NextPlayer, NextFrog, TypeOfGame, NewBoard).    

fill_board(Board, Player, Frog, TypeOfGame, NewBoard) :-
    (
        TypeOfGame = 1, Player = 2; % In player vs cpu mode, the second player is the cpu
        TypeOfGame = 2 % Cpu vs cpu mode
    ),
    cpu_fill_choose(Board, Pos),
    display_cpu_fill_turn(Player, Pos),
    wait_for_input,
    player_frog(Player, Value),
    set_position(Board, Pos, Value, IntBoard),
    next_player(Player, NextPlayer),
    NextFrog is Frog + 1,
    fill_board(IntBoard, NextPlayer, NextFrog, TypeOfGame, NewBoard).    

/**
 * Player fill choose
 * player_fill_choose(+Board, -Pos)
 * Gets a human player position to fill with a frog
 *
 * Board -> Game board.
 * Pos -> Position choosed by the human player 
 */
player_fill_choose(Board, Pos) :- 
    repeat,
        (
            read_position(Board, 'Frog Position? ', Pos),
            get_position(Board, Pos, empty),
            valid_fill_position(Board, Pos),
            !;
            error_msg('Invalid position!')
        ).

/**
 * CPU fill choose
 * cpu_fill_choose(+Board, -Pos)
 * Gets a cpu position to fill with a frog
 *
 * Board -> Game board.
 * Pos -> Position choosed by the cpu 
 */
cpu_fill_choose(Board, Pos) :-
    setof(X, (valid_fill_position(Board, X), get_position(Board, X, empty)), Positions),
    random_member(Pos, Positions).

/**
 * Valid jump
 * valid_jump_position(+Board, +StartPosition, -EndPosition)
 * Generates all valid jumping positions from StartPosition.
 * Jumps can be horizontal, vertical or diagonal, 2 positions from the starting one.
 * 
 * Board -> Game board.
 * StartPosition -> Starting position.
 * EndPosition -> Ending position.
 */
valid_jump_position(Board, [SRow, SCol], [ERow, ECol]) :-
    ERow is SRow,
    ECol is SCol+2,
    valid_position(Board, [ERow, ECol]).
    
valid_jump_position(Board, [SRow, SCol], [ERow, ECol]) :-
    ERow is SRow,
    ECol is SCol-2,
    valid_position(Board, [ERow, ECol]).
    
valid_jump_position(Board, [SRow, SCol], [ERow, ECol]) :-
    ERow is SRow+2,
    ECol is SCol,
    valid_position(Board, [ERow, ECol]).
    
valid_jump_position(Board, [SRow, SCol], [ERow, ECol]) :-
    ERow is SRow-2,
    ECol is SCol,
    valid_position(Board, [ERow, ECol]).
    
valid_jump_position(Board, [SRow, SCol], [ERow, ECol]) :-
    ERow is SRow-2,
    ECol is SCol-2,
    valid_position(Board, [ERow, ECol]).
    
valid_jump_position(Board, [SRow, SCol], [ERow, ECol]) :-
    ERow is SRow+2,
    ECol is SCol-2,
    valid_position(Board, [ERow, ECol]).
    
valid_jump_position(Board, [SRow, SCol], [ERow, ECol]) :-
    ERow is SRow-2,
    ECol is SCol+2,
    valid_position(Board, [ERow, ECol]).
    
valid_jump_position(Board, [SRow, SCol], [ERow, ECol]) :-
    ERow is SRow+2,
    ECol is SCol+2,
    valid_position(Board, [ERow, ECol]).


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
 * frog_can_jump(+Board, +FrogPosition, -Dest)
 * Checks if a frog in a given position can jump in any direction.
 * Returns valid jump destinations.
 * 
 * Board -> Game board.
 * FrogPosition -> Frog position.
 * Dest -> Jump destination.
 */
frog_can_jump(Board, FrogPos, Dest) :-
    % look through all valid jumps
    valid_jump_position(Board, FrogPos, Dest),

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
        read_position(Board, 'Position to jump? ', EndPos),
        get_position(Board, EndPos, empty),
        valid_jump_position(Board, InitPos, ValidPos),
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
            read_position(Board, 'Frog to jump? ', InitPos), 
            get_position(Board, InitPos, Frog),
            player_frog(Player, Frog),
            frog_can_jump(Board, InitPos, _),
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
jumpable_frog_in_row([FirstRow | _], _, [_, LastCol]) :- 
    length(FirstRow, NCols),
    LastCol is NCols-1,
    !, fail.

jumpable_frog_in_row(Board, Player, Pos) :-
    player_frog(Player, Frog),
    get_position(Board, Pos, Frog),
    frog_can_jump(Board, Pos, _), !.

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
find_jumpable_frog(Board, _, L) :- 
    length(Board, L),
    !, fail.

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
 * remove_row_outer_frogs(+CurrRow, +Dimensions, +Position, -NewRow)
 * Iterates over the columns of a row, emptying the positions on the board edges.
 * 
 * CurrRow -> Initial row.
 * Dimensions -> Board dimensions, in the format [Rows, Columns].
 * Positon -> Current position.
 * NewRow -> Modified row.
 */

%first row
remove_row_outer_frogs([_ | Rest], Dimensions, [0, ColI], [empty | NewRow]) :-
    NextCol is ColI+1,
    remove_row_outer_frogs(Rest, Dimensions, [0, NextCol], NewRow).

%last row
remove_row_outer_frogs([_ | Rest], [NRows, NCols], [LastRow, ColI], [empty | NewRow]) :-
    LastRow is NRows-1,
    NextCol is ColI+1,
    remove_row_outer_frogs(Rest, [NRows, NCols], [LastRow, NextCol], NewRow).

%first column
remove_row_outer_frogs([_ | Rest], Dimensions, [RowI, 0], [empty | NewRow]) :-
    remove_row_outer_frogs(Rest, Dimensions, [RowI, 1], NewRow).

%last column
remove_row_outer_frogs(_, [_, NCols], [_, LastCol], [empty]) :-
    LastCol is NCols-1, !.

remove_row_outer_frogs([CurrVal | Rest], [NRows, NCols], [RowI, ColI], [CurrVal | NewRow]) :-
    RowI > 0, RowI < NRows-1,
    ColI > 0, ColI < NCols-1,
    NextCol is ColI+1,
    remove_row_outer_frogs(Rest, [NRows, NCols], [RowI, NextCol], NewRow).


/**
 * Remove outer frogs helper
 * remove_outer_frogs_helper(+InBoard, +NRows, +RowI, -OutBoard)
 * Removes frogs from InBoard in outer positions, iterating over all the rows.
 * In each iteration, appends a modified row to the OutBoard.
 * 
 * InBoard -> Initial board.
 * NRows -> Number of rows in the board.
 * RowI -> Current row.
 * OutBoard -> Modified board.
 */
remove_outer_frogs_helper([], NRows, NRows, []) :- !.

remove_outer_frogs_helper([CurrRow | Rest], NRows, RowI, [NewRow | NewBoard]) :-
    length(CurrRow, NColumns),
    remove_row_outer_frogs(CurrRow, [NRows, NColumns], [RowI, 0], NewRow),
    NextRow is RowI+1,
    remove_outer_frogs_helper(Rest, NRows, NextRow, NewBoard).

/**
 * Remove outer frogs
 * remove_outer_frogs(+InBoard, -OutBoard)
 * Removes frogs from InBoard in outer positions.
 * 
 * InBoard -> Initial board.
 * OutBoard -> Modified board.
 */
remove_outer_frogs(InBoard, OutBoard) :-
    length(InBoard, Rows),
    remove_outer_frogs_helper(InBoard, Rows, 0, OutBoard).

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
    \+frog_can_jump(InBoard, FrogPos, _),

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
 * CPU turn
 * cpu_turn(+InBoard, +Player, -OutBoard)
 * Performs a cpu turn.
 *
 * InBoard -> Initial board.
 * Player -> Current cpu turn.
 * OutBoard -> Modified board after turn ends.
 */
cpu_turn(InBoard, Player, OutBoard) :-
    choose_move(InBoard, Player, 2, Move), !,
    write('CPU move'), nl, wait_for_input,
    player_frog(Player, Frog),
    execute_move(InBoard, Frog, Move, true, OutBoard), !.

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
 * Player vs CPU game
 * pvc_game(+InBoard, +Player, -Winner)
 * Plays a pvc game with the given InBoard and starting player Player.
 *
 * InBoard -> Initial board.
 * Player -> Current player turn.
 * Winner -> Player who wins the game.
 */
pvc_game(InBoard, 1, Winner) :- %Player 1 is the human
    display_game(InBoard, 1, 0),
    player_turn(InBoard, 1, MidBoard),
    remove_outer_frogs(MidBoard, FinalBoard),
    (
        game_over(FinalBoard, 1, Winner),
        display_game(FinalBoard, empty, 0);

        pvc_game(FinalBoard, 2, Winner)
    ), !.

pvc_game(InBoard, 2, Winner) :- %Player 2 is the cpu
    display_game(InBoard, 2, 0),
    cpu_turn(InBoard, 2, MidBoard),
    remove_outer_frogs(MidBoard, FinalBoard),
    (
        game_over(FinalBoard, 2, Winner),
        display_game(FinalBoard, empty, 0);

        pvc_game(FinalBoard, 1, Winner)
    ), !.

/**
 * CPU vs CPU game
 * cvc_game(+InBoard, +Player, -Winner)
 * Plays a cvc game with the given InBoard and starting cpu Player.
 *
 * InBoard -> Initial board.
 * Player -> Current cpu turn.
 * Winner -> Player who wins the game.
 */
cvc_game(InBoard, Player, Winner) :-
    display_game(InBoard, Player, 0),
    cpu_turn(InBoard, Player, MidBoard),
    remove_outer_frogs(MidBoard, FinalBoard),
    (
        game_over(FinalBoard, Player, Winner),
        display_game(FinalBoard, empty, 0);

        next_player(Player, NextPlayer),
        pvc_game(FinalBoard, NextPlayer, Winner)
    ), !.

/**
 * Player vs Player
 * player_vs_player
 * Starts a 2 human player game.
 */
player_vs_player :-
    random_between(1, 2, FirstPlayer),
    init_board(FirstPlayer, 0, InitialBoard),
    %initialBoard(InitialBoard),
    pvp_game(InitialBoard, FirstPlayer, Winner),
    nl, 
    display_winner(Winner).

/**
 * Player vs CPU
 * player_vs_cpu
 * Starts a human player vs cpu game.
 */
player_vs_cpu :-
    random_between(1, 2, FirstPlayer),
    init_board(FirstPlayer, 1, InitialBoard),
    pvc_game(InitialBoard, FirstPlayer, Winner),
    nl,
    display_winner(Winner).

/**
 * CPU vs CPU
 * cpu_vs_cpu
 * Starts a 2 cpu player game.
 */
cpu_vs_cpu :-
    random_between(1, 2, FirstPlayer),
    init_board(FirstPlayer, 2, InitialBoard), 
    cvc_game(InitialBoard, FirstPlayer, Winner),
    nl, 
    display_winner(Winner).

/**
 * Play
 * play
 * Starts the game.
 */
play :-
    display_game_name,
    display_game_modes, !,
    repeat,
        (
            write('What mode do you want to choose? '), 
            read_game_mode(Mode), 
            M is Mode - 1,
            valid_game_mode(M),
            nl, nl,
            !;
            error_msg('Invalid mode!')
        ),
    play_game_mode(M).

/**
 * Play game mode
 * play_game_mode(+Mode)
 * Starts the choosen mode
 *
 * Mode -> Mode that will be played
 */
play_game_mode(0) :-
    player_vs_player.
    
play_game_mode(1) :-
    player_vs_cpu.

play_game_mode(2) :-
    cpu_vs_cpu.

%%%%%%%%%%%%%%%%%%%%
%                  %
%        AI        %
%                  %
%%%%%%%%%%%%%%%%%%%%


/**
 * Choose move
 * choose_move(+Board, +Player, +Level, -Move)
 * Chooses a move for the cpu to take.
 * A move is composed by the sequence of positions to jump to.
 *
 * Board -> Initial Board.
 * Player -> Player number.
 * Move -> List with all the jump positions of a move.
 */
choose_move(Board, Player, 1, Move) :-
    valid_moves(Board, Player, ListOfMoves),
    sort(0, @>=, ListOfMoves, [Move|_]).

choose_move(Board, Player, 2, Move) :-
    valid_moves(Board, Player, ListOfMoves),
    get_best_move(Board, Player, ListOfMoves, Move).


get_best_move(Board, Player, ListOfMoves, BestMove) :-
    get_best_move_helper(Board, Player, ListOfMoves, _, BestMove).

get_best_move_helper(_, _, [], -1, []).

get_best_move_helper(Board, Player, [FirstMove|OtherMoves], BestValue, BestMove) :-
    player_frog(Player, Frog),
    execute_move(Board, Frog, FirstMove, false, NewBoard),
    value(NewBoard, Player, NewBoardValue),
    get_best_move_helper(Board, Player, OtherMoves, NewBestValue, NewBestMove),
    choose_best_move(NewBoardValue, FirstMove, NewBestValue, NewBestMove, BestValue, BestMove).


choose_best_move(FirstValue, FirstMove, SecondValue, _, BestValue, BestMove) :-
    FirstValue > SecondValue,
    BestValue = FirstValue,
    BestMove = FirstMove.

choose_best_move(FirstValue, _, SecondValue, SecondMove, BestValue, BestMove) :-
    FirstValue < SecondValue,
    BestValue = SecondValue,
    BestMove = SecondMove.

choose_best_move(FirstValue, FirstMove, SecondValue, SecondMove, BestValue, BestMove) :-
    length(FirstMove, FirstMoveLength),
    length(SecondMove, SecondMoveLength),
    (
        FirstMoveLength > SecondMoveLength,
        BestValue = FirstValue,
        BestMove = FirstMove;

        BestValue = SecondValue,
        BestMove = SecondMove
    ).

/**
 * Valid Moves
 * valid_moves(+Board, +Player, -ListOfMoves)
 * Checks all valid moves and returns them in a list
 *
 * Board -> Initial Board.
 * Player -> Player number
 * ListOfMoves -> List of all the possible moves
 */
valid_moves(Board, Player, ListOfMoves) :-
    player_frog(Player, Frog), 
    bagof(Pos, (valid_position(Board, Pos), get_position(Board, Pos, Frog)), FrogList), !,%Get the list of frogs
    generate_jumps(Board, FrogList, ListOfMoves), !.

/**
 * Generate jumps
 * generate_jumps(+Board, +Frog, +ListOfPositions, -ListOfJumpPositions)
 * Generates a list of list with all the possible moves of the cpu frogs
 *
 * Board -> Initial Board.
 * Frog -> Player Frog.
 * ListOfPositions -> List of the initial positions of the frogs.
 * ListOfJumpPositions -> List with all the moves that can be done by the cpu.
 */
generate_jumps(_, [], []) :- !.

generate_jumps(Board, [CurrFrogPos | Rest], JumpList) :-
    get_jumps(Board, [CurrFrogPos], CurrFrogJumps),
    generate_jumps(Board, Rest, RestFrogsJumps),
    append(CurrFrogJumps, RestFrogsJumps, JumpList).

/**
 * Prepend value to lists
 * prepend_val_to_lists(+Value, +Lists, -NewLists)
 * Prepends a value to all sublists of a list
 *
 * Value -> The value to prepend to the lists
 * Lists -> A list with all the lists where the value will be added
 * NewLists -> List with the results
 */
prepend_val_to_lists(_, [], []) :- !.
prepend_val_to_lists(NewValue, [[FirstListEle | RestList] | OtherLists], [[NewValue, FirstListEle | RestList] | NewLists]) :-
    prepend_val_to_lists(NewValue, OtherLists, NewLists).


get_jumps(Board, PrevJumps, JumpList) :-
    last(PrevJumps, CurrPosition),
    (
        bagof(EndPos, frog_can_jump(Board, CurrPosition, EndPos), NewJumps);
        NewJumps = []
    ), !,

    (
        length(NewJumps, 0), JumpList = [];
        keep_jumping(Board, PrevJumps, NewJumps, JumpList)
    ), !.

keep_jumping(_, _, [], []) :- !.
keep_jumping(InBoard, PrevJumps, [CurrDest | Rest], [NewJumpSequence | JumpList]) :-
    % determine sequence of jumps until this one
    append(PrevJumps, [CurrDest], NewJumpSequence),

    % get board after the last jump
    last(PrevJumps, CurrPosition), % get current position
    get_position(InBoard, CurrPosition, Frog), % determine frog
    middle_position(CurrPosition, CurrDest, MidPos), % get middle position
    jump(InBoard, CurrPosition, MidPos, CurrDest, Frog, NewBoard), % jump

    % keep jumping from this position
    get_jumps(NewBoard, NewJumpSequence, JumpsFromThisPosition),

    % continue to the other jump destinations
    keep_jumping(InBoard, PrevJumps, Rest, JumpsFromNextPosition),

    % merge 2 lists of jumps
    append(JumpsFromThisPosition, JumpsFromNextPosition, JumpList).


/**
 * Execute Move
 * execute_move(+InputBoard, +Frog, +PositionsList, +DisplayMove, -OutputBoard)
 * Executes a cpu move
 * 
 * InputBoard -> Initial Board
 * Frog -> CPU Frog
 * PositionsList -> List of all the positions of a cpu move
 * DisplayMove -> Indicates if the move should be displayed
 * OutputBoard -> Final Board
 */
execute_move(Board, _, [_ | []], _, Board) :- !. % If there is only one position, there are no more jumps

execute_move(InBoard, Frog, [StartPos, EndPos| OtherPos], true, OutBoard) :-
    middle_position(StartPos, EndPos, MidPos),
    jump(InBoard, StartPos, MidPos, EndPos, Frog, NewBoard),
    display_board(NewBoard),
    %write message to inform about the jump here
    wait_for_input,
    execute_move(NewBoard, Frog, [EndPos| OtherPos], true, OutBoard).

execute_move(InBoard, Frog, [StartPos, EndPos| OtherPos], false, OutBoard) :-
    middle_position(StartPos, EndPos, MidPos),
    jump(InBoard, StartPos, MidPos, EndPos, Frog, NewBoard),
    execute_move(NewBoard, Frog, [EndPos| OtherPos], false, OutBoard).

/*
DEBUG STUFF
*/
print_position([Row, Col]) :-
    write('('), write(Row), write(','), write(Col), write(')').

print_list([]) :- wait_for_input.
print_list([Curr | Rest]) :-
    print_position(Curr),
    write(' - '), print_list(Rest).

print_move_list([]).
print_move_list([CurrMove | NextMoves]) :-
    print_list(CurrMove), nl,
    print_move_list(NextMoves).