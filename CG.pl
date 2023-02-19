
:- include('KB.pl').
%grid(4,4).
%agent_loc(0,2).
%ships_loc([[1,2], [3,2]]).
%station(1,1).
%capacity(2).

%grid(3,3).
%agent_loc(0,1).
%ships_loc([[2,2],[1,2]]).
%station(1,1).
%capacity(1).

%checks if the cell is valid and within the grid dimenions.
validCell(X, Y):-
    grid(R,C),
    (X < R),
    \+ (X < 0),
    (Y < C),
    \+ (Y < 0).

takeAction(X,Y,C,0,L,N,s0):-
   agent_loc(X,Y), capacity(C), ships_loc(L),length(L,N).

takeAction(X,Y,C,PC,L,N,result(Action, State)):-
    takeAction(X1,Y1,C,PC1,L1,N1,State),
    validCell(X1,Y1),
    ((station(X1,Y1), Action = drop, PC1>0, PC = 0, X = X1, Y = Y1,N=N1,L=L1 );

     (N1=1, L1=[H|_],Action= pickup, X = X1, Y = Y1, H=[Sx,Sy], Sx=X1,Sy=Y1,
      PC1<C, PC is PC1+1, L=[], N=0 );

     (N1=2, L1=[H1,H2|_], Action= pickup, X = X1, Y = Y1, H1=[Sx,Sy], Sx=X1,Sy=Y1,
      PC1<C, PC is PC1+1, L=[H2], N=1 );

     (N1=2, L1=[H1,H2|_], Action= pickup, X = X1, Y = Y1, H2=[Sx,Sy], Sx=X1,Sy=Y1,
      PC1<C, PC is PC1+1, L=[H1],N=1 );

     (Action = up,X is X1-1,Y=Y1,validCell(X,Y),PC=PC1, L =L1 ,N=N1);

     (Action = down,X is X1+1,Y=Y1,validCell(X,Y),PC=PC1, L=L1, N=N1);

     (Action =right, Y is Y1+1,X=X1 ,validCell(X,Y),PC=PC1, L=L1 ,N=N1);

     (Action = left, Y is Y1-1,X=X1,validCell(X,Y),PC=PC1, L=L1 ,N=N1)).

goal(S):-
    ids(S,1).

ids(X,L):-
    (call_with_depth_limit(goal2(X),L,R), number(R));
    (call_with_depth_limit(goal2(X),L,R), R=depth_limit_exceeded,
        L1 is L+1, ids(X,L1)).

goal2(S):-
    takeAction(_,_,_,PC,L,N,S),
    L=[], PC=0, N=0.
