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
    18 blue frogs
    28 empty cells, the board edges
*/
initialBoard([
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, blue, yellow, yellow, blue, blue, yellow, empty],
    [empty, blue, blue, blue, yellow, yellow, yellow, empty],
    [empty, blue, blue, yellow, blue, yellow, yellow, empty],
    [empty, blue, blue, yellow, blue, yellow, yellow, empty],
    [empty, yellow, yellow, blue, yellow, blue, blue, empty],
    [empty, blue, yellow, blue, yellow, blue, yellow, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty]
]).

/*
    The game is still going on, any player can jump.
*/
intermediateBoard([
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, blue, blue, empty, empty, empty, blue, empty],
    [empty, blue, blue, empty, yellow, empty, empty, empty],
    [empty, blue, empty, empty, yellow, yellow, yellow, empty],
    [empty, blue, blue, empty, blue, empty, empty, empty],
    [empty, yellow, blue, empty, empty, empty, blue, empty],
    [empty, empty, yellow, empty, yellow, blue, yellow, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty]
]).

/*
    The only yellow frog in the bottom-right corner is isolated.
    blue player wins.
*/
isolatedPiece([
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, blue, blue, empty, empty, blue, blue, empty],
    [empty, blue, blue, empty, empty, empty, blue, empty],
    [empty, blue, empty, empty, empty, empty, empty, empty],
    [empty, blue, blue, empty, empty, empty, empty, empty],
    [empty, empty, blue, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, yellow, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty]
]).

/*
    blue player has no frogs left and yellow player made the last move.
    yellow player wins.
*/
noblueFrogsLeft([
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, yellow, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, yellow, empty, empty, yellow, empty, empty],
    [empty, empty, empty, empty, empty, yellow, yellow, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty]
]).