:- use_module(library(lists)).
:- include('game_state.pl').
:- include('display.pl').
:- include('input.pl').
:- include('board.pl').
:- include('ai.pl').

/** 
 * Valid game mode
 * valid_game_mode(+Mode)
 * Checks if a given mode is valid 
 *
 * Mode -> Mode to check.
 */
valid_game_mode(Mode) :-
    Mode >= 0,
    Mode =< 2.

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
 * Initialize game dimensions
 * init_game_dimensions(-Rows, -Columns)
 * Asks the user to init game dimensions.
 *
 * Rows -> Number of rows in the board.
 * Columns -> Number of columns in the board.
 */
init_game_dimensions(Rows, Columns) :-
    ansi_format([fg(blue)], 'CONFIGURE THE BOARD DIMENSIONS', []), nl,
    write('The original board is 8x8.'), nl,
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
 * Player -> Number of the player that will fill that next position.
 * Frogs -> Number of frogs that are already on the board.
 * TypeOfGame -> Indicates the type of game (0 - player vs player; 1 - player vs cpu; 2 - cpu vs cpu).
 * NewBoard -> Board filled with frogs.
 */
fill_board(Board, _, FrogCount, _, Board) :-
    [FirstRow | _] = Board,
    length(Board, NRows),
    length(FirstRow, NCols),
    FrogCount is (NCols-2)*(NRows-2),

    display_board(Board), !,
    nl, ansi_format([fg(blue)], 'GAME WILL START!', []), nl, wait_for_input, nl.

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
 * Pos -> Position choosed by the human player.
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
 * cpu_turn(+InBoard, +Player, +Level, -OutBoard)
 * Performs a cpu turn.
 *
 * InBoard -> Initial board.
 * Player -> Current cpu turn.
 * Level -> AI level.
 * OutBoard -> Modified board after turn ends.
 */
cpu_turn(InBoard, Player, Level, OutBoard) :-
    choose_move(InBoard, Player, Level, Move), !,    
    ansi_format([fg(blue)], 'CPU MOVE', []), nl, wait_for_input,
    player_frog(Player, Frog), !,
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
pvc_game(InBoard, 1, Level, Winner) :- %Player 1 is the human
    display_game(InBoard, 1, 0),
    player_turn(InBoard, 1, MidBoard),
    remove_outer_frogs(MidBoard, FinalBoard),
    (
        game_over(FinalBoard, 1, Winner),
        display_game(FinalBoard, empty, 0);

        pvc_game(FinalBoard, 2, Level, Winner)
    ), !.

pvc_game(InBoard, 2, Level, Winner) :- %Player 2 is the cpu
    display_game(InBoard, 2, 0),
    cpu_turn(InBoard, 2, Level, MidBoard),
    remove_outer_frogs(MidBoard, FinalBoard),
    (
        game_over(FinalBoard, 2, Winner),
        display_game(FinalBoard, empty, 0);

        pvc_game(FinalBoard, 1, Level, Winner)
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
    cpu_turn(InBoard, Player, 2, MidBoard),
    remove_outer_frogs(MidBoard, FinalBoard),
    (
        game_over(FinalBoard, Player, Winner),
        display_game(FinalBoard, empty, 0);

        next_player(Player, NextPlayer),
        cvc_game(FinalBoard, NextPlayer, Winner)
    ), !.

/**
 * Player vs Player
 * player_vs_player
 * Starts a 2 human player game.
 */
player_vs_player :-
    random_between(1, 2, FirstPlayer),
    init_board(FirstPlayer, 0, InitialBoard),
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
    repeat,
    (
        display_ai_levels,
        read_ai_level(Level), 
        nl, nl, !;
        error_msg('Invalid level!')
    ),
    init_board(FirstPlayer, 1, InitialBoard),
    pvc_game(InitialBoard, FirstPlayer, Level, Winner),
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
play_game :-
    display_game_modes, !,
    repeat,
        (
            write('What mode do you want to play? '), 
            read_game_mode(Mode), 
            M is Mode - 1,
            valid_game_mode(M),
            nl, nl,
            !;
            error_msg('Invalid mode!')
        ),
    play_game_mode(M).

choose_menu_option(1) :-
    play_game, !, 
    game_menu, !.

choose_menu_option(2) :-
    %display_instructions, !,
    game_menu, !.

choose_menu_option(3) :-
    %display_credits, !,
    game_menu, !.

choose_menu_option(4). %4 is the exit option

game_menu :-
    repeat,
        (
            display_menu_options, 
            read_menu_option(Option), nl, !,
            choose_menu_option(Option), !;
            error_msg('Invalid option!')
        ).

/**
 * Play
 * play
 * Starts the game menu.
 */
play :-
    display_game_name,
    game_menu,
    display_thank_you_msg.
   

/**
 * Play game mode
 * play_game_mode(+Mode)
 * Starts the choosen mode
 *
 * Mode -> Mode that will be played.
 */
play_game_mode(0) :-
    player_vs_player.
    
play_game_mode(1) :-
    player_vs_cpu.

play_game_mode(2) :-
    cpu_vs_cpu.

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
