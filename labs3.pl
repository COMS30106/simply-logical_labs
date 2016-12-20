
:-compile(library).

%%% Question 1 %%%

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

