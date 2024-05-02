calculateH(cell(X,Y,Color),cell(Goalx,Goaly,_),H):-
    H is abs(X - Goalx) + abs(Y - Goaly).

% -------------------------------------

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


search(Open, Closed, Goal,Board,N):-
    getBestState(Open, [CurrentState,Parent,G,H,F], _), % Step 1
    CurrentState = Goal, % Step 2
    append(Closed, [[CurrentState,Parent,G,H,F]], NewClosed), % Step 5.1
    write("Search is complete!"), nl,
    findSolution(NewClosed, Goal, Acc, Solution), 
    printSolution(Solution), !.
    
search([], _, _, _,_) :-
    write("Search is complete!"), nl,
    write("No Path is found!"), !.

search(Open, Closed, Goal,Board,N):-
    getBestState(Open, CurrentNode, TmpOpen),
    getAllValidChildren(CurrentNode,TmpOpen,Closed,Goal,Children,Board,N), % Step 3
    addChildren(Children, TmpOpen, NewOpen), % Step 4
    append(Closed, [CurrentNode], NewClosed), % Step 5.1
    search(NewOpen, NewClosed, Goal,Board,N). % Step 5.2

% Implementation of step 3 to get the next states
getAllValidChildren(Node, Open, Closed, Goal, Children,Board,N):-
    findall(Next, getNextState(Node,Open,Closed,Goal,Next,Board,N),Children).


% Implementation of addChildren and getBestState
addChildren(Children, Open, NewOpen):-
    append(Open, Children, NewOpen).

getBestState(Open, BestChild, Rest):-
    findMin(Open, BestChild),
    delete(Open, BestChild, Rest).


findMin([X], X):- !.
findMin([Head|T], Min):-
    findMin(T, TmpMin),
    Head = [_,_,_,HeadH,HeadF],
    TmpMin = [_,_,_,TmpH,TmpF],
    (TmpF < HeadF  -> Min = TmpMin ; Min = Head).

getNextState([State,_,G,_,_],Open,Closed,cell(X,Y,Colorg),[Next,State,NewG,NewH,NewF],Board,N):-
    move(State, Next,Board ,N),
    calculateH(Next, cell(X,Y,Colorg), NewH),
    NewG is G + 1,
    NewF is NewG + NewH,
    Next = cell(_,_,Colorg),
    (not(member([Next,_,_,_,_], Open));memberButBetter(Next,Open,NewF)),
    (not(member([Next,_,_,_,_],Closed));memberButBetter(Next,Closed,NewF)).

memberButBetter(Next, List, NewF):-
    findall(F, member([Next,_,_,_,F], List), Numbers),
    min_list(Numbers, MinOldF),
    MinOldF > NewF.


findSolution(_, null, Solution, Solution).
findSolution(List, Cell, Accum, Solution):-
    member([Cell, Parent, _, _, _], List),
    findSolution(List, Parent, [Cell|Accum], Solution).

printSolution([Cell]):- write(Cell), !.
printSolution([Cell|Rest]) :-
    write(Cell), write(' -> '),
    printSolution(Rest).
