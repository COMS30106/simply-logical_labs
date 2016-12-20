
/*
 * Example of a relational language
 */


constant(peter).
constant(maria).

% rel_atom(A,L) <- A is a relational atom with arguments L
rel_atom(likes(X,Y),[X,Y]).
rel_atom(student_of(X,Y),[X,Y]).


% Constants are the ground terms in relational clausal logic
ground_term(T):-
	constant(T).

ground_terms([]).
ground_terms([T|Ts]):-
	ground_term(T),
	ground_terms(Ts).


% An atom is ground if its arguments are ground terms
ground_atom(A):-
	rel_atom(A,Args),
	ground_terms(Args).


:-consult(herbrand).

