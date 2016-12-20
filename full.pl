
/*
 * Example of a full clausal language
 */


% full_atom(A,L) <- A is an atom with arguments L
full_atom(plus(X,Y,Z),[X,Y,Z]).

% full_term(T,L) <- T is a term with arguments L
full_term(0,[]).      % constant
full_term(s(X),[X]).  % functor


% A term is ground if its arguments are ground terms
ground_term(T):-
	full_term(T,Args),
	ground_terms(Args).

ground_terms([]).
ground_terms([T|Ts]):-
	ground_term(T),
	ground_terms(Ts).


% An atom is ground if its arguments are ground terms
ground_atom(A):-
	full_atom(A,Args),
	ground_terms(Args).


:-consult(herbrand).

