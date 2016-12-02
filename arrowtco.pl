test(N) :-
	(   N =:= 0
	 ;
	    NN is N - 1,
	    test(NN)
	).


test2(N) :-
	N < 0.
test2(N) :-
	NN is N - 1,
	blah(NN).

blah(N) :-
	NN is N - 2,
	test2(NN).


