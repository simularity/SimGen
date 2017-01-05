main :-
    reset(p, foo(3), Cont),
    writeln(main_c),
    reset(Cont, bar(4), NewCont),
    writeln(main_d),
    call(NewCont).

p :-
	writeln(p_e),
	q,
	writeln(p_f),
        fail.

p :-
	writeln(p_g),
	call(q),
	writeln(p_h).

q :-
	writeln(q_a),
	shift(foo(X)),
	writeln(X),
	shift(bar(Y)),
	writeln(Y).
other :-
    reset(p, foo(3), Cont),
    reset(p, foo(3), FooCont),
    reset(FooCont, bar(4), BarCont),
    call(BarCont),
    reset(Cont, bar(4), Final),
    call(Final).



% hey annie, are you not calling a continuation you're given?
%


% try metapredicates
