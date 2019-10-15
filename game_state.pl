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
    18 green frogs
    18 pink frogs
    28 empty cells, the board edges
*/
initialBoard([
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, pink, green, green, pink, pink, green, empty],
    [empty, pink, pink, pink, green, green, green, empty],
    [empty, pink, pink, green, pink, green, green, empty],
    [empty, pink, pink, green, pink, green, green, empty],
    [empty, green, green, pink, green, pink, pink, empty],
    [empty, pink, green, pink, green, pink, green, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty]
]).

/*
    The game is still going on, any player can jump.
*/
intermediateBoard([
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, pink, pink, empty, empty, empty, pink, empty],
    [empty, pink, pink, empty, green, empty, empty, empty],
    [empty, pink, empty, empty, green, green, green, empty],
    [empty, pink, pink, empty, pink, empty, empty, empty],
    [empty, green, pink, empty, empty, empty, pink, empty],
    [empty, empty, green, empty, green, pink, green, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty]
]).

/*
    The only green frog in the bottom-right corner is isolated.
    Pink player wins.
*/
isolatedPiece([
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, pink, pink, empty, empty, pink, pink, empty],
    [empty, pink, pink, empty, empty, empty, pink, empty],
    [empty, pink, empty, empty, empty, empty, empty, empty],
    [empty, pink, pink, empty, empty, empty, empty, empty],
    [empty, empty, pink, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, green, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty]
]).

/*
    Pink player has no frogs left and green player made the last move.
    green player wins.
*/
noPinkFrogsLeft([
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, green, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, green, empty, empty, green, empty, empty],
    [empty, empty, empty, empty, empty, green, green, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty]
]).