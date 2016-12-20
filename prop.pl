
/* 
 * Example of a propositional language 
 */


prop_atom(married).
prop_atom(bachelor).
prop_atom(man).
prop_atom(adult).


% Propositional atoms are trivially ground 
ground_atom(A):-prop_atom(A).


:-consult(herbrand).

