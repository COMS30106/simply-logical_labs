
:-consult(library).


%% The 8-puzzle %%
%% Adapted from the tiles-program on p.120 %%

puzzle(C,N):-		% return cost C and number of searched nodes N
	puzzle(M,C,N),
	nl,nl,nl,
	show_ps(M).	% write moves M

% puzzle(M,C,N) <- moves M lead to a goal position at cost C and N nodes searched
%                  (best-first search)
puzzle(Moves,Cost,N):-
	start(Start),
	eval(Start,Value),
	puzzle_a([v(Value,Start)],Final,[],Visited),
	construct_moves(Final,Visited,[],Moves,Cost),
	length(Visited,N).

% puzzle_a(A,M,V0,V) <- goal position can be reached from one of the
%                       positions on A with last move M (best-first)
%                       (V is list of visited nodes, V0 is accumulator)
puzzle_a([v(V,LastMove)|Rest],LastMove,Visited,Visited):-
	goal(LastMove),!. % no backtracking
puzzle_a([v(V,LastMove)|Rest],Goal,Visited0,Visited):-
	show_move(LastMove,V),
	setof0(v(Value,NextMove),
               (move(LastMove,NextMove),eval(NextMove,Value)),
	       Children),
	merge(Children,Rest,NewAgenda),	% best-first search
	!,puzzle_a(NewAgenda,Goal,[LastMove|Visited0],Visited). % no backtracking

merge([],Agenda,Agenda).
merge([C|Cs],[],[C|Cs]).
merge([v(V1,Move1)|Rest1],[v(V2,Move2)|Rest2],[v(V1,Move1)|Rest3]):-
        V1<V2,
        merge(Rest1,[v(V2,Move2)|Rest2],Rest3).
merge([v(V1,Move1)|Rest1],[v(V2,Move2)|Rest2],[v(V2,Move2)|Rest3]):-
        V1>=V2, % first-in first-out for equal values
        merge([v(V1,Move1)|Rest1],Rest2,Rest3).


%%% move predicates %%%

% A move is represented by a term m(Pos,NewPos,Cost), 
% meaning that the move is from position Pos to position NewPos
% Cost is the depth-count g(NewPos)

% reconstruct total cost and path from list of visited nodes
construct_moves(m(Parent,FinalPos,Cost),Visited,Moves0,Moves,Cost):-
	construct_moves(m(Parent,FinalPos,Cost),Visited,Moves0,Moves).

construct_moves(m(noparent,Start,0),Visited,Moves,[Start|Moves]):-!. % no backtracking
construct_moves(m(Parent,Pos,C),Visited,Moves0,Moves):-
	C1 is C-1,
	remove_one(m(GP,Parent,C1),Visited,RestVisited),
	!,construct_moves(m(GP,Parent,C1),RestVisited,[Pos|Moves0],Moves). % no backtracking

% move(m(X,P,Y),m(P,NP,C)) <- position NP can be reached from current 
%                             position P in one move at total cost C=g(NP)
move(m(OldPos,Pos,OldCost),m(Pos,NewPos,NewCost)):-
	move_p(Pos,NewPos),
	NewCost is OldCost + 1.	% cost of one move is 1

same_pos(m(_,Pos,_),m(_,Pos,_)).

start(m(noparent,Start,0)):-
	start_p(Start).

goal(m(P,Goal,C)):-
	goal_p(Goal).

eval(m(P,Pos,G),F):-	% f(Pos) = g(Pos) + h(Pos)
	eval_p(Pos,H),
	F is G + H.	

show_move(m(P,Pos,C),Value):-
	write(Value),ttyflush.	% don't display move, just write f(n)


%%% position predicates %%%

% A position is represented by a list 
%	[XE/YE,X1/Y1,X2/Y2,X3/Y3,X4/Y4,X5/Y5,X6/Y6,X7/Y7,X8/Y8]
% where the first element denotes the coordinates of the emtpy square,
% X1/Y1 denotes the coordinates of tile 1, etc.
% Coordinates 1/1 denote the square in the lower left corner. 

% Here are a number of different starting positions

start_p([2/2,1/1,1/3,2/3,3/3,3/1,1/2,2/1,3/2]). 	
% 2 3 4	
% 6   8
% 1 7 5

start_p([2/2,1/1,2/3,1/2,3/3,3/1,1/3,2/1,3/2]). 	
% 6 2 4	
% 3   8
% 1 7 5

start_p([2/2,2/3,1/3,3/2,3/3,3/1,2/1,1/1,1/2]). 	
% 2 1 4	
% 8   3
% 7 6 5

start_p([2/2,1/3,2/3,1/2,1/1,3/1,2/1,3/2,3/3]). 	
% 1 2 8
% 3   7
% 4 6 5



goal_p([2/2,1/3,2/3,3/3,3/2,3/1,2/1,1/1,1/2]).

move_p([Empty|Tiles],[Tile|Tiles1]):-
        replace(Empty,Tile,Tiles,Tiles1).

replace(Empty,Tile,[Tile|Tiles],[Empty|Tiles]):-
        mandist(Empty,Tile,1).
replace(Empty,Tile,[T|Ts],[T|Ts1]):-
        replace(Empty,Tile,Ts,Ts1).

mandist(X1/Y1,X2/Y2,D):-
	D is abs(X1-X2) + abs(Y1-Y2).

show_ps([]):-nl.
show_ps([P|Ps]):-
	show_p(P),nl,
	show_ps(Ps).

show_p(Pos):-
	y_coordinate(Y),nl,x_coordinate(X),
	show_tile(X/Y,Pos),
	fail.	% failure-driven loop, backtracks over all coordinates
show_p(Pos).	% no more coordinates

show_tile(X/Y,[Empty,T1,T2,T3,T4,T5,T6,T7,T8]):-
	element(N-X/Y,[' '-Empty,1-T1,2-T2,3-T3,4-T4,5-T5,6-T6,7-T7,8-T8]),
	write(N),write(' ').

x_coordinate(X):-
	element(X,[1,2,3]).

y_coordinate(Y):-
	element(Y,[3,2,1]).


%%% heuristic %%%

eval_p(Pos,Value):-
        goal_p(Goal),
        totdist(Pos,Goal,Value).

totdist([E1|T1],[E2|T2],D):-
        totdist(T1,T2,0,D).

totdist([],[],D,D).
totdist([T1|T1s],[T2|T2s],D0,D):-
        mandist(T1,T2,DT),
        D1 is D0+DT,
        totdist(T1s,T2s,D1,D).


insequence([Empty,Tile1|Tiles],S):-
	insequence([Tile1|Tiles],Tile1,S).

insequence([Tile8],Tile1,S):-
	score(Tile8,Tile1,S).
insequence([Ta,Tb|Tiles],Tile1,S):-
	score(Ta,Tb,S1),
	insequence([Tb|Tiles],Tile1,S2),
	S is S1+S2.

score(2/2,_,1):-!.	      % tile in centre scores 1
score(T1,T2,0):-
	successor(T1,T2),!.   % successor tile in clockwise direction is OK
score(T1,T2,2).               % non-successor tile scores 2

successor(1/1,1/2).
successor(1/2,1/3).
successor(1/3,2/3).
successor(2/3,3/3).
successor(3/3,3/2).
successor(3/2,3/1).
successor(3/1,2/1).
successor(2/1,1/1).
