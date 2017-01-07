:- module(pdq, []).

:- dynamic running/1, to_do/2, first_tick_done/1.

:- use_module(bt_impl, [emit/1]).

bt_impl:make_cn_impl(~? , C-N, _-_) :-
	running(C-N),
	!.
bt_impl:make_cn_impl(~?, C-N, CParent-NParent) :-
	asserta(running(C-N)),
	listen(C-N, terminate(C-N), pdq:terminate(C-N)),
	listen(C-N, terminate_if_child(CParent-NParent),
	       pdq:terminate(C-N)),
	listen(C-N, tick_start, tick_start(C-N)),
	listen(C-N, more, has_more(C-N)),
	listen(C-N, propagate, propagate(C-N)),
	listen(C-N, tick_end, tick_end(C-N)),
	emit(starting(C-N)).

stop_me(C-N, How) :-
	unlisten(C-N, _, _),
	retractall(to_do(C-N, _)),
	retractall(first_tick_done(C-N)),
	retractall(running(C-N)),
	emit(stopped(C-N, How)).

terminate(C-N) :-
	unlisten(C-N, _, _),
	retractall(running(C-N)),
	emit(stopped(C-N, terminated)).

tick_start(C-N) :-
	bt_impl:node_(_, N, '!', [FirstTick, OtherTicks, _Conds], _),
	retractall(to_do(C-N, _)),
	(   first_tick_done(C-N)
	->  asserta(to_do(C-N, OtherTicks))
	;   asserta(to_do(C-N, FirstTick))
	).

tick_end(C-N) :-
	(   first_tick_done(C-N)
	;   asserta(first_tick_done(C-N))
	),
	bt_impl:node_(_, N, '!', [_, _, Conds], _),
	(   conds(C, Conds)
	->  true
	;   stop_me(C-N, fail)
	).

has_more(C-N) :-
	to_do(C-N, ToDo),
	ToDo \= [].

propagate(C-N) :-
	to_do(C-N, List),
	(   List = []
	;   eval(C, List, NewList),
	    retractall(to_do(C-N, _)),
	    asserta(to_do(C-N, NewList))
	).

		 /*******************************
		 * Continuous evaluator
		 *******************************/

eval(_Context, [], []).
eval(C, [H | T], Remain) :-
	(   eval(C, H)
	->  eval(C, T, Remain)
	;   eval(C, T, R),
	    Remain = [H | R]
	).

eval(C, ':='(LVAL, RVAL)) :-
	eval_rval(C, lastval, RVAL, Value),
	setval(C, LVAL, Value).
eval(C, '='(LVAL, RVAL)) :-
	eval_rval(C, getval, RVAL, Value),
	setval(C, LVAL, Value).

/*     ================= DONE TO HERE FRIDAY 8pm ======== */

conds(X) :- once(conds_(X)).
conds_([]).
conds_([H | T]) :-
	H =.. [CompareOp, Left, Right],
	eval_rval(C, ezval, Left, LeftVal),
        eval_rval(C, ezval, Right, RightVal),
	!,
        Compo =.. [CompareOp, LeftVal, RightVal],
	call_if_avail(Compo, LeftVal, RightVal),
	debug(bt(ticks, cond), 'cond ~w ~w ~w passed', [CompareOp, LeftVal, RightVal]),
	conds(T).
conds_([H | _]) :-
	debug(bt(ticks, cond), 'cond ~w failed', [H]),
	fail.

call_if_avail(_, '$not_avail$', _).
call_if_avail(_, _, '$not_avail$').
call_if_avail(Goal, A, B) :-
	A \= '$not_avail$',
	B \= '$not_avail$',
	call(Goal).

eval_rval(C, GetFunctor, RVal , Value) :-
	RVal =.. [F, A, B],
	eval_rval(C, GetFunctor, A, AVal),
	eval_rval(C, GetFunctor, B, BVal),
	e(F, AVal, BVal, Value).
eval_rval(C, GetFunctor, -A , Value) :-
	eval_rval(C, GetFunctor, A, AVal),
	Value is -AVal.
eval_rval(C, GetFunctor, eval(C, RVal) , Value) :-
	RVal =.. [F | Args],
	get_functor_ok(GetFunctor, F),
	maplist(eval_rval(C, GetFunctor), Args, ArgVals),
	do_func(F, ArgVals, Value).
eval_rval(C, GetFunctor, eval(C, F), Value) :-
	atom(F),
	get_functor_ok(GetFunctor, F),
	do_func(F, [], Value).
eval_rval(C, _, const(Val), Val).
% we must call GetFunctor in a loop. This is how we do
% dataflow logic
eval_rval(C, GetFunctor, var(Name), Val) :-
	Func =.. [GetFunctor, Name, Val],
	shift(Func),
	(   var(Val)
	->  eval_rval(C, GetFunctor, var(Name), Val)
	;   true
	).

emit_val(LVAL, Value) :-
	current_context(Context),
	shift(getclock(simgen, Time)),
	shift(getclock(Context, ContextTime)),
	broadcast(reading(Time, ContextTime, Context, LVAL, Value)).

% TODO make this do somethin
get_functor_ok(_,_).

e( '+', A, B, C) :- C is A + B.
e( '-', A, B, C) :- C is A - B.
e( '*', A, B, C) :- C is A * B.
e( '/', A, B, C) :- C is A / B.

do_func(levy_flight, [LastVal, Lo, Hi], Val) :-
	map64k(LastVal, Lo, Hi, LastValMapped), % map to range [0-64k)
	levy_flight(LastValMapped, NewValMapped),
	map64k(Val, Lo, Hi, NewValMapped).
do_func(wander, [LastVal, Lo, Hi, Dist], Val) :-
	Bias is 2 * (LastVal - Lo) / (Hi - Lo) ,
	random(R),
	Del is 2 * Dist * R - Dist * Bias,
	Val is min(Hi, max(Lo, LastVal + Del)).
do_func(clock, [], Val) :-
	current_context(Context),
	debug(bt(flow,clock), 'function clock(~w)', [Context]),
	shift(getclock(Context, Val)).

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
	random_between(0,1, Flip),
	(   Flip =:= 0
	->  NewVal is LastVal /\ xor(0xffff , Bit)
	;   NewVal is LastVal \/ Bit
	),
	NewBit is Bit >> 1,
	levy_flight(NewVal, Val, NewBit).

% TODO make good messages
%
%


