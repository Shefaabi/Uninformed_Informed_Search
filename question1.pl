% input : [cell(X,Y,Color), ...] , N = row
% col = len / row
% X : row
% Y : col


move(CurrenCell, NextCell, State, N):-
    left(CurrenCell, NextCell, State, N); right(CurrenCell, NextCell, State, N);
    up(CurrenCell, NextCell, State, N); down(CurrenCell, NextCell, State, N).

left(cell(X, Y, Color), NextCell, State, N):-
    M is length(State) / N,
    NewY is Y - 1,
    NewY >= 0,
    member(cell(X, NewY, NextColor), State),
    NextCell = cell(X, NewY, NextColor).

right(cell(X, Y, Color), NextCell, State, N):-
    M is length(State) / N,
    NewY is Y + 1,
    NewY < M,
    member(cell(X, NewY, NextColor), State),
    NextCell = cell(X, NewY, NextColor).

up(cell(X, Y, Color), NextCell, State, N):-
    M is length(State) / N,
    NewX is X - 1,
    NewX >= 0,
    member(cell(NewX, Y, NextColor), State),
    NextCell = cell(NewX, Y, NextColor).


down(cell(X, Y, Color), NextCell, State, N):-
    M is length(State) / N,
    NewX is X + 1,
    NewX < N,
    member(cell(NewX, Y, NextColor), State),
    NextCell = cell(NewX, Y, NextColor).

% -------------------------------------
% Rule to check if there is a cycle of the same color including 4 cells or more
cycle_of_same_color(VisitedCells) :-
    member(cell(X,Y,Color), VisitedCells), % Take a cell from the visited cells
    cycle_check(X, Y, Color, [(X,Y)], VisitedCells). % Start cycle check from this cell

% Base case: Cycle of same color is found if the next cell is the starting cell and the cycle length is at least 4
cycle_check(X, Y, Color, [(X0,Y0)|Visited], VisitedCells) :-
    member(cell(X,Y,Color), VisitedCells), % Next cell is in visited cells
    length([(X0,Y0)|Visited], Length),
    Length >= 4, % Ensure cycle length is at least 4
    (X,Y) = (X0,Y0). % Next cell is the starting cell

% Recursive case: Continue exploring neighboring cells
cycle_check(X, Y, Color, Visited, VisitedCells) :-
    member(cell(X,Y,Color), VisitedCells), % Next cell is in visited cells
    neighbor(X, Y, X1, Y1), % Get neighboring cell
    + member((X1,Y1), Visited), % Ensure neighbor is not already visited
    cycle_check(X1, Y1, Color, [(X,Y)|Visited], VisitedCells). % Recursively check the neighbor

% Rule to define neighbors of a cell (up, down, left, right)
neighbor(X, Y, X1, Y) :- X1 is X+1.
neighbor(X, Y, X, Y1) :- Y1 is Y+1.
neighbor(X, Y, X1, Y) :- X1 is X-1, X1 >= 0.
neighbor(X, Y, X, Y1) :- Y1 is Y-1, Y1 >= 0.
% -------------------------------------
% [[cell(x,y,c), parent] ]
search(Open, Closed, Board, N):-
    getState(Open, [CurrentState,Parent], _), % Step 1 Pop a state from the open list so that it becomes the current state.
    cycle_of_same_color(Closed), !, % Step 2 Check whether the current state is a goal state.
    write("Search is complete!"), nl,
    printSolution([CurrentState,Parent], Closed).

search(Open, Closed, Board, N):-
    getState(Open, CurrentNode, TmpOpen),
    getAllValidChildren(CurrentNode,TmpOpen,Closed,Children, Board, N), % Step3 Get the list of all valid next states (children) from the current state.
    addChildren(Children, TmpOpen, NewOpen), % Step 4 Add the next states to the open list.
    append(Closed, [CurrentNode], NewClosed), % Step 5.1 Add the current state to the list of visited states (closed list) 
    search(NewOpen, NewClosed, Board, N). % Step 5.2 then go to step 1.

getNextState([CurrenCell,_], Open, Closed, [NextCell, CurrenCell], Board, N):-
    move(CurrenCell, NextCell, Board, N),
    not(member([NextCell,_], Open)),
    not(member([NextCell,_], Closed)).

% Implementation of getState and addChildren determine the search alg.
% BFS
getState([CurrentNode|Rest], CurrentNode, Rest).

addChildren(Children, Open, NewOpen):-
    append(Open, Children, NewOpen). % bfs

% Implementation of step 3 to get the next states
% valid = = > the state is not repeated and 
% does not violate the rules of the problem
getAllValidChildren(Node, Open, Closed, Children, Board, N):-
    findall(Next, getNextState(Node, Open, Closed, Next, Board, N), Children).

% Implementation of printSolution to print the actual solution path
printSolution([State, null],_):-
    write(State), nl.

printSolution([State, Parent], Closed):-
    member([Parent, GrandParent], Closed),
    printSolution([Parent, GrandParent], Closed),
    write(State), nl.

