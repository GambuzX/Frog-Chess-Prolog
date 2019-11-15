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
 * Level -> Level of the AI.
 * Move -> List with all the jump positions of a move.
 */
choose_move(Board, Player, 1, Move) :-
    valid_moves(Board, Player, ListOfMoves), 
    random_member(Move, ListOfMoves).

choose_move(Board, Player, 2, Move) :-
    valid_moves(Board, Player, ListOfMoves),
    sort(0, @>=, ListOfMoves, [Move|_]), !.

choose_move(Board, Player, 3, Move) :-
    valid_moves(Board, Player, ListOfMoves),
    get_best_move(Board, Player, ListOfMoves, _, Move), !.

choose_move(Board, Player, 4, Move) :-
    valid_moves(Board, Player, ListOfMoves),
    minimax(Board, Player, ListOfMoves, Move), !.


/**
 * Minimax
 * minimax(+Board, +Player, +ListOfMoves, -Choice)
 * Chooses a move from a ListOfMoves using Minimax algorithm.
 * Chooses the move which will lead to the worst best case scenario for the opponent.
 * Calls minimax_helper.
 *
 * Board -> Initial Board.
 * Player -> Player number.
 * ListOfMoves -> List with all possible moves.
 * Choice -> Choosen move.
 */
minimax(Board, Player, ListOfMoves, Choice) :-
    minimax_helper(Board, Player, ListOfMoves, _, Choice).

/**
 * Minimax helper
 * minimax(+Board, +Player, +ListOfMoves, -BestValue, -BestMove)
 * Chooses a move from a ListOfMoves using Minimax algorithm.
 * Chooses the move which will lead to the worst best case scenario for the opponent.
 * In each step, BestValue and BestMove represent the best options for the remaining ListOfMoves.
 *
 * Board -> Initial Board.
 * Player -> Player number.
 * ListOfMoves -> List with all possible moves.
 * BestValue -> Best value found so far.
 * BestMove -> Best move found so far.
 */
minimax_helper(Board, Player, [LastMove | []], LastMoveValue, LastMove) :-
    % execute last move and check its value
    player_frog(Player, Frog),
    execute_move(Board, Frog, LastMove, false, MidBoard),
    remove_outer_frogs(MidBoard, CurrBoard),
    value(CurrBoard, Player, CurrBoardValue),
    (
        % choose current move value if it wins
        CurrBoardValue = 5000,
        LastMoveValue = CurrBoardValue;        

        % otherwise, see opponent moves
        next_player(Player, Opponent),
        valid_moves(CurrBoard, Opponent, OpponentMoves),
        get_best_move(CurrBoard, Opponent, OpponentMoves, BestOponnentMoveValue, _),
        LastMoveValue is BestOponnentMoveValue * (-1)
    ), !.

minimax_helper(Board, Player, [CurrMove | RestMoves], BestValue, BestMove) :-
    % execute current move
    player_frog(Player, Frog),
    execute_move(Board, Frog, CurrMove, false, MidBoard),
    remove_outer_frogs(MidBoard, CurrBoard),

    % check current board value and handle it
    value(CurrBoard, Player, CurrBoardValue),
    (
        % choose current move if it wins
        CurrBoardValue = 5000,
        BestValue = CurrBoardValue,
        BestMove = CurrMove;

        % check best move among the remaining moves
        minimax_helper(Board, Player, RestMoves, RestBestValue, RestBestMove),
        (
            % already found a winner move in remaining moves
            RestBestValue = 5000,
            BestValue = RestBestValue,
            BestMove = RestBestMove;

            % check best move opponent can make in the current board
            next_player(Player, Opponent),
            valid_moves(CurrBoard, Opponent, OpponentMoves),
            get_best_move(CurrBoard, Opponent, OpponentMoves, BestOponnentMoveValue, _),
            % decide between current options
            CurrBestValue is BestOponnentMoveValue * (-1),
            choose_best_move(CurrBestValue, CurrMove, RestBestValue, RestBestMove, BestValue, BestMove)
        ), !
    ), !.

/**
 * Get best move
 * get_best_move(+Board, +Player, +ListOfMoves, ?BestMoveValue, -BestMove)
 * Gets the best move of a list of moves.
 *
 * Board -> Game board.
 * Player -> Player number.
 * ListOfMoves -> List with all the possible moves of Player.
 * BestMoveValue -> The value of the best move. If it is a winning move, its equal to 5000.
 * BestMove -> The best move for the Player.
 */
get_best_move(InBoard, Player, [LastMove | []], LastMoveValue, LastMove) :-
    % execute last move and calculate its value
    player_frog(Player, Frog),
    execute_move(InBoard, Frog, LastMove, false, MidBoard),
    remove_outer_frogs(MidBoard, CurrBoard),
    value(CurrBoard, Player, LastMoveValue), !.

get_best_move(InBoard, Player, [CurrMov | RestMoves], BestValue, BestMove) :-
    % execute current move
    player_frog(Player, Frog),
    execute_move(InBoard, Frog, CurrMov, false, MidBoard),
    remove_outer_frogs(MidBoard, CurrBoard),

    % calculate resulting board's value and handle it
    value(CurrBoard, Player, CurrBoardValue), !,
    (
        % current move is a winning move
        CurrBoardValue = 5000,
        BestValue = CurrBoardValue,
        BestMove = CurrMov;
        
        % search for next moves
        get_best_move(InBoard, Player, RestMoves, RestBestValue, RestBestMove), !,
        (
            % found a winning move in RestMoves, return it
            RestBestValue = 5000,
            BestValue = RestBestValue,
            BestMove = RestBestMove;

            % choose between current move and the best found in RestMoves
            choose_best_move(CurrBoardValue, CurrMov, RestBestValue, RestBestMove, BestValue, BestMove)
        ), !
    ), !.

/**
 * Choose best move
 * choose_best_move(+FirstValue, +FirstMove, +SecondValue, +SecondMove, -BestValue, -BestMove)
 * Chooses the best of two moves
 *
 * FirstValue -> The value of the first move.
 * FirstMove -> The first move.
 * SecondValue -> The value of the second move.
 * SecondMove -> The second move.
 * BestValue -> The value of the best move.
 * BestMove -> The best of the two moves.
 */
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
    ), !.

/**
 * Valid Moves
 * valid_moves(+Board, +Player, -ListOfMoves)
 * Checks all valid moves and returns them in a list
 *
 * Board -> Initial Board.
 * Player -> Player number.
 * ListOfMoves -> List of all the possible moves.
 */
valid_moves(Board, Player, ListOfMoves) :-
    player_frog(Player, Frog), 
    (
        % get list of Player frogs
        bagof(Pos, (valid_position(Board, Pos), get_position(Board, Pos, Frog)), FrogList), !,

        % generate jumps for those frogs
        generate_jumps(Board, FrogList, ListOfMoves);

        ListOfMoves = []
    ), !.

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
 * Get Jumps
 * get_jumps(+Board, +PrevJumps, -JumpList)
 * Gets all the new jumps possible given the current board and the previous jumps
 *
 * Board -> Game Board.
 * PrevJumps -> Previous Jumps.
 * JumpList -> The List of New Jumps.
 */
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

/**
 * Keep Jumping
 * keep_jumping(+InBoard, +PreviousJumps, +NewDestinations, -JumpList)
 * Calls get_jump for each element of the NewDestinations list
 *
 * InBoard -> Initial Board.
 * PreviousJumps -> List with all the positions in this jump sequence.
 * NewDestinations -> List with the new possible destinations.
 * JumpList -> List with all the jumps.
 */
keep_jumping(_, _, [], []) :- !.
keep_jumping(InBoard, PrevJumps, [CurrDest | Rest], [NewJumpSequence | JumpList]) :-
    % determine sequence of jumps until this one
    append(PrevJumps, [CurrDest], NewJumpSequence),

    % get board after the last jump
    last(PrevJumps, CurrPosition), % get current position
    move([CurrPosition, CurrDest], InBoard, NewBoard), % jump

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
 * InputBoard -> Initial Board.
 * Frog -> CPU Frog.
 * PositionsList -> List of all the positions of a cpu move.
 * DisplayMove -> Indicates if the move should be displayed.
 * OutputBoard -> Final Board.
 */
execute_move(InBoard, Frog, PositionList, true, OutputBoard) :-
    execute_move_helper(InBoard, Frog, PositionList, 1, OutputBoard).

execute_move(InBoard, Frog, PositionList, false, OutputBoard) :-
    execute_move_helper(InBoard, Frog, PositionList, OutputBoard).

/**
 * Execute Move Helper
 * execute_move_helper(+InputBoard, +Frog, +PositionsList, +JumpNumber, -OutputBoard)
 * Executes a cpu move with display
 * 
 * InputBoard -> Initial Board.
 * Frog -> CPU Frog.
 * PositionsList -> List of all the positions of a cpu move.
 * JumpNumber -> Number of the jump that is being executed.
 * OutputBoard -> Final Board.
 */
execute_move_helper(Board, _, [_ | []], _, Board) :- !. % If there is only one position, there are no more jumps

execute_move_helper(InBoard, Frog, [StartPos, EndPos| OtherPos], JumpN, OutBoard) :-
    move([StartPos, EndPos], InBoard, NewBoard),
    player_frog(Player, Frog),
    display_game(NewBoard, Player, JumpN),
    display_cpu_jump(StartPos, EndPos),
    wait_for_input,
    NextJumpN is JumpN +1,
    execute_move_helper(NewBoard, Frog, [EndPos| OtherPos], NextJumpN, OutBoard).


/**
 * Execute Move Helper
 * execute_move_helper(+InputBoard, +Frog, +PositionsList, -OutputBoard)
 * Executes a cpu move without display
 * 
 * InputBoard -> Initial Board.
 * Frog -> CPU Frog.
 * PositionsList -> List of all the positions of a cpu move.
 * OutputBoard -> Final Board.
 */
execute_move_helper(Board, _, [_| []], Board) :- !. % If there is only one position, there are no more jumps

execute_move_helper(InBoard, Frog, [StartPos, EndPos| OtherPos], OutBoard) :-
    move([StartPos, EndPos], InBoard, NewBoard),
    execute_move_helper(NewBoard, Frog, [EndPos| OtherPos], OutBoard).