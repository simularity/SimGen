:- module(functions , [
	      get_functor_ok/2,
	      do_func/4
	  ]).



:- use_module(simgen(clocks)).
:- use_module(simgen(print_system)).


get_functor_ok(lastval, levy_flight).
get_functor_ok(lastval, wander).
get_functor_ok(lastval, F) :- any_get(F).
get_functor_ok(getval, F) :- any_get(F).
get_functor_ok(ezval, F) :- any_get(F).

% functors that are not prolog built-ins
any_get(clock).
any_get(pow).
any_get(lshift).
any_get(rshift).
any_get(bitor).
any_get(bitand).
any_get(X) :- prolog_function(X/_).


do_func(_, levy_flight, [LastVal, Lo, Hi], Val) :-
	map64k(LastVal, Lo, Hi, LastValMapped), % map to range [0-64k)
	levy_flight(LastValMapped, NewValMapped),
	map64k(Val, Lo, Hi, NewValMapped).
do_func(_, wander, [LastVal, Lo, Hi, Dist], Val) :-
	bt_debug(bt(flow,wander), 'in wander ~w, ~w, ~w, ~w',
	      [LastVal, Lo, Hi, Dist]),
	Bias is 2 * (LastVal - Lo) / (Hi - Lo) ,
	random(R),
	Del is 2 * Dist * R - Dist * Bias,
	Val is min(Hi, max(Lo, LastVal + Del)),
	bt_debug(bt(flow,wander), 'out', []).
do_func(C-N, clock, [], Val) :-
	bt_debug(bt(flow,clock), 'function clock(~w), ~w', [C, N]),
	get_clock(C-N, Val).
do_func(_, pow, [Old, Exp], Val) :- Val is Old^Exp.
do_func(_, lshift, [Old, Bits], Val) :- Val is Old << Bits.
do_func(_, rshift, [Old, Bits], Val) :- Val is Old >> Bits.
do_func(_, bitor, [Old, Bits], Val) :- Val is Old \/ Bits.
do_func(_, bitand, [Old, Bits], Val) :- Val is Old /\ Bits.

% those that are prolog functions
do_func(_, Functor, [], Val) :-
	prolog_function(Functor/0),
	Blah =.. [Val, is , Functor],
	call(Blah).
do_func(_, Functor, [Arg], Val) :-
	prolog_function(Functor/1),
	Blah =.. [Functor, Arg],
	Val is Blah.
do_func(_, Functor, [L, R], Val) :-
	prolog_function(Functor/2),
	Blah =.. [Functor, L, R],
	Val is Blah.

prolog_function(mod/2).
prolog_function(rem/2).
prolog_function(div/2).
prolog_function(rdiv/2).
prolog_function(gcd/2).
prolog_function(abs/1).
prolog_function(sign/1).
prolog_function(copysign/2).
prolog_function(max/2).
prolog_function(min/2).
prolog_function(random/1).
prolog_function(random_float/0).
prolog_function(round/1).
prolog_function(integer/1).
prolog_function(float/1).
prolog_function(rational/1).
prolog_function(rationalize/1).
prolog_function(float_fractional_part/1).
prolog_function(float_integer_part/1).
prolog_function(truncate/1).
prolog_function(floor/1).
prolog_function(ceiling/1).
prolog_function(xor/2).
prolog_function(sqrt/1).
prolog_function(sin/1).
prolog_function(cos/1).
prolog_function(tan/1).
prolog_function(asin/1).
prolog_function(acos/1).
prolog_function(atan/1).
prolog_function(atan2/2).
prolog_function(atan/2).
prolog_function(sinh/1).
prolog_function(cosh/1).
prolog_function(tanh/1).
prolog_function(asinh/1).
prolog_function(acosh/1).
prolog_function(atanh/1).
prolog_function(log/1).
prolog_function(log10/1).
prolog_function(exp/1).
prolog_function(lgamma/1).
prolog_function(erf/1).
prolog_function(erfc/1).
prolog_function(pi/0).
prolog_function(e/0).
prolog_function(epsilon/0).
prolog_function(inf/0).
prolog_function(nan/0).
prolog_function(cputime/0).
prolog_function(eval/1).
prolog_function(msb/1).
prolog_function(lsb/1).
prolog_function(popcount/1).
prolog_function(getbit/2).

map64k(N, Lo, Hi, Mapped) :-
	ground(Mapped),
	N is Lo + (Hi - Lo) * Mapped / 65536.0 .
map64k(N, Lo, Hi, Mapped) :-
	ground(N),
	Mapped is round((N - Lo) * 65536.0 / (Hi - Lo)).

% compute new mapped value from old mapped value
levy_flight(LastVal, NewVal) :-
	random_between(0, 0xf, BitsToFlip),
	Bit is ((1 << BitsToFlip) >> 1),
	levy_flight(LastVal, NewVal, Bit).

levy_flight(V, V, 0).
levy_flight(LastVal, Val, Bit) :-
	bt_debug(bt(flow, levy_flight),
	      'levy_flight(~w, ~w, ~w)',
	      [LastVal, Val, Bit]),
	random_between(0,1, Flip),
	(   Flip =:= 0
	->  NewVal is LastVal /\ xor(0xffff , Bit)
	;   NewVal is LastVal \/ Bit
	),
	NewBit is Bit >> 1,
	levy_flight(NewVal, Val, NewBit).





















