:- module(state_life, [its_a_life/3]).
/** <module> recursive version of life

*/
its_a_life(Life, Born, Present) :-
	congenital(Infant, Born),
	live([present(Present), born(Born) | Infant], Born, Life).


congenital(Infant, BornYr) :-
        pre_congenital(BornYr, Utero),
	phrase(congenital(BornYr, Utero), Infant).

pre_congenital(BornYr, [sexuality(Orientation),
			gender(Gender)
		       | ParentStatus]) :-
	parental_status(BornYr, ParentStatus),
	one_of(gender, [m-0.5, f-0.5], Gender),
	one_of(orientation, [gay-0.1, straight-0.9], Orientation).

parental_status(_BornYr, [family_econ(Econ)]) :-
	one_of(Econ, [rich-0.1, middle_class-0.6, poor-0.3]).

congenital(Born, Utero) -->
	Utero,
	sexuality(Born),
	parents(Born),
	one_of(intelligence, [bright-0.05, normal-0.9, stupid-0.05]),
	one_of(infant_health, [normal-0.9, sick-0.1]).

one_of(

