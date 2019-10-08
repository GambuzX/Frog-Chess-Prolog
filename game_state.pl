/*
    64 empty cells
*/
emptyBoard([
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty]
]).


/*
    18 yellow frogs
    18 pink frogs
    28 empty cells, the board edges
*/
initialBoard([
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, pink, yellow, yellow, pink, pink, yellow, empty],
    [empty, pink, pink, pink, yellow, yellow, yellow, empty],
    [empty, pink, pink, yellow, pink, yellow, yellow, empty],
    [empty, pink, pink, yellow, pink, yellow, yellow, empty],
    [empty, yellow, yellow, pink, yellow, pink, pink, empty],
    [empty, pink, yellow, pink, yellow, pink, yellow, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty]
]).

/*
    The game is still going on, any player can jump.
*/
intermediateBoard([
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, pink, pink, empty, empty, empty, pink, empty],
    [empty, pink, pink, empty, yellow, empty, empty, empty],
    [empty, pink, empty, empty, yellow, yellow, yellow, empty],
    [empty, pink, pink, empty, pink, empty, empty, empty],
    [empty, yellow, pink, empty, empty, empty, pink, empty],
    [empty, empty, yellow, empty, yellow, pink, yellow, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty]
]).

/*
    The only yellow frog in the bottom-right corner is isolated.
    Pink player wins.
*/
isolatedPiece([
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, pink, pink, empty, empty, pink, pink, empty],
    [empty, pink, pink, empty, empty, empty, pink, empty],
    [empty, pink, empty, empty, empty, empty, empty, empty],
    [empty, pink, pink, empty, empty, empty, empty, empty],
    [empty, empty, pink, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, yellow, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty]
]).

/*
    Pink player has no frogs left and yellow player made the last move.
    Yellow player wins.
*/
noPinkFrogsLeft([
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, yellow, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, yellow, empty, empty, yellow, empty, empty],
    [empty, empty, empty, empty, empty, yellow, yellow, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty]
]).