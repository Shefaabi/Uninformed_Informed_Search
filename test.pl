cycle_of_same_color(Board) :-
    member(cell(X,Y,Color), Board), % Take a cell from the visited cells
    search(X, Y, Color, [(X,Y)], Board). % Start cycle check from this cell

% Base case: Cycle of the same color is found if the next cell is the starting cell and the cycle length is at least 4
search(X, Y, Color, [(X0,Y0)|Visited], Board) :-
    member(cell(X,Y,Color), Board), % Next cell is in visited cells
    length([(X0,Y0)|Visited], Length),
    Length >= 4, % Ensure cycle length is at least 4
    move(X,Y , X0,Y0), !, % Next cell is the starting cell
    \+ (member((X1,Y1), [(X0, Y0)|Visited]), member(cell(X1,Y1,Color1), Board), Color \= Color1), % Ensure all cells in the cycle have the same color
    write("Cycle is found!"), nl,
    write([(X0, Y0)|Visited]), nl.

% Recursive case: Continue exploring moveing cells
search(X, Y, Color, [(X0,Y0)|Visited], Board) :-
    member(cell(X,Y,Color), Board), % Next cell is in visited cells
    move(X0, Y0, X1, Y1), % Get moveing cell
    member(cell(X1,Y1,_), Board),
    \+member((X1,Y1), Visited), % Ensure move is not already visited
    search(X, Y, Color, [(X1,Y1),(X0,Y0)|Visited], Board). % Recursively check the move


% Rule to define moves of a cell (up, down, left, right)
move(X, Y, X1, Y) :- X1 is X+1.
move(X, Y, X, Y1) :- Y1 is Y+1.
move(X, Y, X1, Y) :- X1 is X-1, X1 >= 0.
move(X, Y, X, Y1) :- Y1 is Y-1, Y1 >= 0.