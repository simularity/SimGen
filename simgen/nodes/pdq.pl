:- module(pdq, []).

:- dynamic running/1, to_do/2, first_tick_done/1.

:- use_module(simgen(bt_impl), [emit/1]).
:- use_module(simgen(valuator)).
:- use_module(simgen(print_system)).
:- use_module(simgen(functions)).

:-listen(simulation_starting, reset).

reset :-
	retractall(running(_)),
	retractall(to_do(_, _)),
	retractall(first_tick_done(_)).

:- multifile bt_impl:make_cn_impl/3.

bt_impl:make_cn_impl( '!' , C-N, _-_) :-
	running(C-N),
	!,
	bt_debug(bt(pdq, make_cn_impl),
		 '~w-~w already running', [C,N]).
bt_impl:make_cn_impl( '!' , C-N, CParent-NParent) :-
	asserta(running(C-N)),
	retractall(first_tick_done(C-N)),
	retractall(to_do(C-N, _)),
	listen(C-N, terminate(C-N), pdq:terminate(C-N)),
	listen(C-N, terminate_if_child(CParent-NParent),
	       pdq:terminate(C-N)),
	listen(C-N, tick_start, tick_start(C-N)),
	listen(C-N, more, has_more(C-N)),
	listen(C-N, propagate, propagate(C-N)),
	listen(C-N, tick_end, tick_end(C-N)),
	bt_debug(bt(pdq, make_cn_impl), 'start ~w-~w', [C,N]),
	emit(starting(C-N)).

stop_me(C-N, How) :-
	unlisten(C-N, _, _),
	retractall(to_do(C-N, _)),
	retractall(first_tick_done(C-N)),
	retractall(running(C-N)),
	bt_debug(bt(pdq, stop_me), 'stop ~w-~w', [C,N]),
	emit(stopped(C-N, How)).

terminate(C-N) :-
	unlisten(C-N, _, _),
	retractall(running(C-N)),
	retractall(to_do(C-N, _)),
	retractall(first_tick_done(C-N)),
	bt_debug(bt(pdq, terminate), 'terminated ~w-~w', [C,N]),
	emit(stopped(C-N, terminated)).

tick_start(C-N) :-
	bt_impl:node_(_, N, '!', [FirstTick, OtherTicks, _Conds], _),
	retractall(to_do(C-N, _)),
	(   first_tick_done(C-N)
	->  asserta(to_do(C-N, OtherTicks))
	;   asserta(to_do(C-N, FirstTick)),
	    asserta(first_tick_done(C-N))
	).

tick_end(C-N) :-
	first_tick_done(C-N),
	!,
	bt_impl:node_(_, N, '!', [_, _, Conds], _),
	(   conds(C-N, Conds)
	->  true
	;   stop_me(C-N, fail)
	).
% always succeed if we haven't done the first tick by end of tick
% fixes the mystery unavail values
tick_end(C-N) :-
	\+ first_tick_done(C-N).

has_more(C-N) :-
	to_do(C-N, ToDo),
	ToDo \= [],
	bt_debug(bt(flow,propagate), '~w-~w needs more', [C,N]).

propagate(C-N) :-
	to_do(C-N, List),
	bt_debug(bt(flow,propagate),
	      'propagating ~w-~w tasks ~w',
	      [C,N,List]),
	(   List = []
	;   eval(C-N, List, NewList),
	    retractall(to_do(C-N, _)),
	    asserta(to_do(C-N, NewList))
	),
	to_do(C-N, L),
	bt_debug(bt(flow,propagate),
	      'after list is ~w', [L]).

		 /*******************************
		 *      Continuous evaluator
		 *******************************/

eval(_CN, [], []).
eval(C-N, [H | T], Remain) :-
	(   eval(C-N, H)
	->  eval(C-N, T, Remain)
	;   eval(C-N, T, R),
	    Remain = [H | R]
	).

eval(C-N, ':='(LVAL, RVAL)) :-
	eval_rval(C-N, lastval, RVAL, Value),
	setval(C, LVAL, Value).
eval(C-N, '='(LVAL, RVAL)) :-
	eval_rval(C-N, getval, RVAL, Value),
	setval(C, LVAL, Value).

conds(C-N, X) :- once(conds_(C-N, X)).
conds_(_, []).
conds_(C-N, [H | T]) :-
	H =.. [CompareOp, Left, Right],
	eval_rval(C-N, ezval, Left, LeftVal),
        eval_rval(C-N, ezval, Right, RightVal),
	!,
        Compo =.. [CompareOp, LeftVal, RightVal],
	call_if_avail(Compo, LeftVal, RightVal),
	bt_debug(bt(ticks, cond), 'cond ~w ~w ~w passed', [CompareOp, LeftVal, RightVal]),
	conds(C-N, T).
conds_(C-N, [H | _]) :-
	bt_debug(bt(ticks, cond), 'context ~w cond ~w failed', [C-N, H]),
	fail.

call_if_avail(_, '$not_avail$', _).
call_if_avail(_, _, '$not_avail$').
call_if_avail(Goal, A, B) :-
	A \= '$not_avail$',
	B \= '$not_avail$',
	call(Goal).

/*
eval_rval(C-N, GetFunctor, eval(wander(V, Lo, Hi, Dist)) , Value) :-
	gtrace,
	fail.
*/
eval_rval(C-N, GetFunctor, RVal , Value) :-
	RVal =.. [F, A, B],
	eval_rval(C-N, GetFunctor, A, AVal),
	eval_rval(C-N, GetFunctor, B, BVal),
	e(F, AVal, BVal, Value).
eval_rval(C-N, GetFunctor, -A , Value) :-
	eval_rval(C-N, GetFunctor, A, AVal),
	Value is -AVal.
eval_rval(C-N, GetFunctor, eval(RVal) , Value) :-
	RVal =.. [F | Args],
	get_functor_ok(GetFunctor, F),
	maplist(eval_rval(C-N, GetFunctor), Args, ArgVals),
	do_func(C-N, F, ArgVals, Value).
eval_rval(C-N, GetFunctor, eval(C, F), Value) :-
	atom(F),
	(   get_functor_ok(GetFunctor, F)
	->  do_func(C-N, F, [], Value)
	 ;  get_functor_ok(Legal, F),
	    gf_name_symbol(GetFunctor, GetFunctorSymbol),
	    gf_name_symbol(Legal, LegalSymbol),
	    bt_debug(error(pdq, bad_get), 'Illegal to use ~w in ~w, try ~w',
		  [GetFunctorSymbol, F, LegalSymbol]),
	    Value = 0
	).
eval_rval(_, _, const(Val), Val).
eval_rval(C-_N, GetFunctor, var(Name), Val) :-
	!, % this is nondet
	Func =.. [GetFunctor, C, Name, Val],
	call(valuator:Func).

gf_name_symbol(getval, '=').
gf_name_symbol(lastval, ':=').
gf_name_symbol(ezval, 'in cond').

e( '+', A, B, C) :- C is A + B.
e( '-', A, B, C) :- C is A - B.
e( '*', A, B, C) :- C is A * B.
e( '/', A, B, C) :- C is A / B.
