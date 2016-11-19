a(0).
a(B) :-
	B > 0,
	NB #= B - 1,
	a(NB).

test :- between(1,1000, _),a(10000),fail.
test.

