% 
% Input query design:
% search([[cell(0,0,yellow),null]],[],[cell(0,0,yellow), cell(0,1,yellow), cell(0,2,yellow),cell(0,3,red),cell(1,0,blue), cell(1,1,yellow), cell(1,2,blue),cell(1,3,yellow),cell(2,0,blue), cell(2,1,blue), cell(2,2,blue),cell(2,3,yellow),cell(3,0,blue), cell(3,1,blue), cell(3,2,blue),cell(3,3,yellow)],4).
%


move(CurrenCell, NextCell, State, N):-
    left(CurrenCell, NextCell, State, N); right(CurrenCell, NextCell, State, N);
    up(CurrenCell, NextCell, State, N); down(CurrenCell, NextCell, State, N).

left(cell(X, Y, Color), NextCell, State, N):-
    length(State,L),
    M is L / N,
    NewY is Y - 1,
    NewY >= 0,
    member(cell(X, NewY, NextColor), State),
    NextCell = cell(X, NewY, NextColor).

right(cell(X, Y, Color), NextCell, State, N):-
    length(State,L),
    M is L / N,
    NewY is Y + 1,
    NewY < M,
    member(cell(X, NewY, NextColor), State),
    NextCell = cell(X, NewY, NextColor).

up(cell(X, Y, Color), NextCell, State, N):-
    length(State,L),
    M is L / N,
    NewX is X - 1,
    NewX >= 0,
    member(cell(NewX, Y, NextColor), State),
    NextCell = cell(NewX, Y, NextColor).


down(cell(X, Y, Color), NextCell, State, N):-
    length(State,L),
    M is L / N,
    NewX is X + 1,
    NewX < N,
    member(cell(NewX, Y, NextColor), State),
    NextCell = cell(NewX, Y, NextColor).

% -------------------------------------
% Rule to check if there is a cycle of the same color including 4 cells or more
% Rule to check if there is a cycle where all cells have the same color

cycle_of_same_color(VisitedCells) :-
    member([cell(X,Y,Color),_], VisitedCells), % Take a cell from the visited cells
    cycle_check(X, Y, Color, [(X,Y)], VisitedCells). % Start cycle check from this cell

% Base case: Cycle of the same color is found if the next cell is the starting cell and the cycle length is at least 4
cycle_check(X, Y, Color, [(X0,Y0)|Visited], VisitedCells) :-
    member([cell(X,Y,Color),_], VisitedCells), % Next cell is in visited cells
    length([(X0,Y0)|Visited], Length),
    Length >= 4, % Ensure cycle length is at least 4
    neighbor(X,Y , X0,Y0), !, % Next cell is the starting cell
    \+ (member((X1,Y1), [(X0, Y0)|Visited]), member([cell(X1,Y1,Color1),_], VisitedCells), Color \= Color1), % Ensure all cells in the cycle have the same color
    write("Cycle is found!"), nl,
    write([(X0, Y0)|Visited]), write(" form a cycle of "), write(Color), nl.

% Recursive case: Continue exploring neighboring cells
cycle_check(X, Y, Color, [(X0,Y0)|Visited], VisitedCells) :-
    member([cell(X,Y,Color),_], VisitedCells), % Next cell is in visited cells
    neighbor(X0, Y0, X1, Y1), % Get neighboring cell
    member([cell(X1,Y1,_),_], VisitedCells),
    \+member((X1,Y1), Visited), % Ensure neighbor is not already visited
    cycle_check(X, Y, Color, [(X1,Y1),(X0,Y0)|Visited], VisitedCells). % Recursively check the neighbor


% Rule to define neighbors of a cell (up, down, left, right)
neighbor(X, Y, X1, Y) :- X1 is X+1.
neighbor(X, Y, X, Y1) :- Y1 is Y+1.
neighbor(X, Y, X1, Y) :- X1 is X-1, X1 >= 0.
neighbor(X, Y, X, Y1) :- Y1 is Y-1, Y1 >= 0.

% -------------------------------------

% base case to terminate when there is a cycle found and print it
search(Open, Closed, Board, N):-
    getState(Open, [CurrentState,Parent], _), % Step 1 Pop a state from the open list so that it becomes the current state.
    cycle_of_same_color(Closed) ,!.

% base case to terminate if there is no cycles found
search([], _, _, _) :-
    write("Search is complete!"), nl,
    write("No Cycle is found!"), !.

search(Open, Closed, Board, N):-
    getState(Open, CurrentNode, TmpOpen),
    getAllValidChildren(CurrentNode,TmpOpen,Closed,Children, Board, N), % Step 2 Get the list of all valid next states (children) from the current state.
    addChildren(Children, TmpOpen, NewOpen), % Step 3 Add the next states to the open list.
    append(Closed, [CurrentNode], NewClosed), % Step 4.1 Add the current state to the list of visited states (closed list)
    search(NewOpen, NewClosed, Board, N). % Step 4.2 then go to step 1.

getNextState([CurrenCell,_], Open, Closed, [NextCell, CurrenCell], Board, N):-
    move(CurrenCell, NextCell, Board, N),
    not(member([NextCell,_], Open)),
    not(member([NextCell,_], Closed)). 

% Implementation of getState and addChildren determine the search alg.
% BFS
getState([],[_,_],[]):-!.
getState([CurrentNode|Rest], CurrentNode, Rest).

addChildren(Children, Open, NewOpen):-
    append(Open, Children, NewOpen). 

getAllValidChildren(Node, Open, Closed, Children, Board, N):-
    findall(Next, getNextState(Node, Open, Closed, Next, Board, N), Children).

