:-compile(aux_sicstus).

%%% Question 1 %%%

powerset([],[[]]).
powerset([H|T],PowerSet):-
	powerset(T,PowerSetOfT),
	extend_pset(H,PowerSetOfT,PowerSet).

extend_pset(_,[],[]).
extend_pset(H,[List|MoreLists],[List,[H|List]|More]):-
	extend_pset(H,MoreLists,More).

powerset1([],[[]]).
powerset1([H|T],PowerSet):-
	extend_pset(H,PowerSetOfT,PowerSet),
	powerset1(T,PowerSetOfT).

% use this for testing

biglist(L):-bagof(X,between(1,15,X),L).

between(Low,High,Low).
between(Low,High,Number):-
	Low<High,
	NewLow is Low+1,
	between(NewLow,High,Number).


%%% Question 3 %%%

:-op(700,xfy,because).
:-op(600,xfy,and).

% prove_e(G,E) <- goal G can be proved with explanation E
prove_e(true,given):-!.
prove_e((First,Rest),E1 and E2):-!,
	prove_e(First,E1),
	prove_e(Rest,E2).
prove_e(Goal,Goal because E):-
	clause(Goal,Body),
	prove_e(Body,E).

:-dynamic student_of/2,follows/2,teaches/2.

student_of(X,T):-follows(X,C),teaches(T,C).
follows(paul,computer_science).
follows(paul,expert_systems).
follows(maria,ai_techniques).
teaches(adrian,expert_systems).
teaches(peter,ai_techniques).
teaches(peter,computer_science).

:-dynamic sublist1/2,sublist2/2,sublist3/2.

sublist1(B,ABC) :-
  append(A,BC,ABC),
  append(B,C,BC).

sublist2(B,ABC) :-
  append(B,C,BC),
  append(A,BC,ABC).

sublist3(DL,L):-
  append(DL,Suffix,L).
sublist3(DL,[X|L]):-
  sublist3(DL,L).

:-dynamic append/3.

append([],Ys,Ys).
append([X|Xs],Ys,[X|Zs]):-
  append(Xs,Ys,Zs).

:-dynamic bachelor/1,man/1,married/1.

bachelor(X):-man(X),not(married(X)).
man(fred).
man(peter).
married(fred).

