% problem specific
%
%
%
% [cell(0,0,c), ] , N = row
% col = len / row
% x = row
% y = column

move(CurrenCell, NextCell, State, N):-
    left(CurrenCell, NextCell, State, N); right(CurrenCell, NextCell, State, N);
    up(CurrenCell, NextCell, State, N); down(CurrenCell, NextCell, State, N).

left(cell(X, Y, _), NextCell, State, _):-
    NewY is Y - 1,
    NewY >= 0,
    member(cell(X, NewY, NextColor), State),
    NextCell = cell(X, NewY, NextColor).

right(cell(X, Y, _), NextCell, State, N):-
    length(State,L),
    M is L / N,
    NewY is Y + 1,
    NewY < M,
    member(cell(X, NewY, NextColor), State),
    NextCell = cell(X, NewY, NextColor).

up(cell(X, Y, _), NextCell, State, _):-
    NewX is X - 1,
    NewX >= 0,
    member(cell(NewX, Y, NextColor), State),
    NextCell = cell(NewX, Y, NextColor).


down(cell(X, Y, _), NextCell, State, N):-
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
     write([(X0, Y0)|Visited]), nl.

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
% [[cell(x,y,c), parent] ]
search(Open, Closed, Board, N):-
    getState(Open, [CurrentState,Parent], _), % Step 1 Pop a state from the open list so that it becomes the current state.
    cycle_of_same_color(Closed) ,!.

search([], _, _, _) :-
    write("Search is complete!"), nl,
    write("No Cycle is found!"), !.

search(Open, Closed, Board, N):-
    getState(Open, CurrentNode, TmpOpen),
    getAllValidChildren(CurrentNode,TmpOpen,Closed,Children, Board, N), % Step3 Get the list of all valid next states (children) from the current state.
    addChildren(Children, TmpOpen, NewOpen), % Step 4 Add the next states to the open list.
    append(Closed, [CurrentNode], NewClosed), % Step 5.1 Add the current state to the list of visited states (closed list)
    search(NewOpen, NewClosed, Board, N). % Step 5.2 then go to step 1.

getNextState([CurrenCell,_], Open, Closed, [NextCell, CurrenCell], Board, N):-
    move(CurrenCell, NextCell, Board, N),
    not(member([NextCell,_], Open)),
    not(member([NextCell,_], Closed)). % Step 2 Check whether the current state is a goal state.

% Implementation of getState and addChildren determine the search alg.
% BFS
getState([],[_,_],[]):-!.
getState([CurrentNode|Rest], CurrentNode, Rest).

addChildren(Children, Open, NewOpen):-
    append(Open, Children, NewOpen). % bfs

% Implementation of step 3 to get the next states
% valid = = > the state is not repeated and
% does not violate the rules of the problem
getAllValidChildren(Node, Open, Closed, Children, Board, N):-
    findall(Next, getNextState(Node, Open, Closed, Next, Board, N), Children).





