:- module(check_guard, []).
/** <module> Check a guard. Fail when it's reset
 *
 */
:- dynamic running/1.

:- use_module(simgen(bt_impl), [emit/1]).
:- use_module(simgen(guard_manager)).

:-listen(simulation_starting, reset).

reset :-
	retractall(running(_)).

:- multifile bt_impl:make_cn_impl/3.

bt_impl:make_cn_impl( '?' , C-N, _-_) :-
	running(C-N), % TODO should this be evil?
	!.
bt_impl:make_cn_impl( '?' , C-N, CParent-NParent) :-
	\+ running(C-N),
	bt_impl:node_(_, N, '?', [GuardName], _),
	guard(C, GuardName),
	asserta(running(C-N)),
	listen(C-N, terminate(C-N), terminate(C-N)),
	listen(C-N, terminate_if_child(CParent-NParent),
	       terminate(C-N)),
	listen(C-N, tick_start, tick_start(C-N)),
	emit(starting(C-N)).
bt_impl:make_cn_impl( '?' , C-N, _) :-
	bt_impl:node_(_, N, _, [GuardName], _),
	\+ guard(C, GuardName),
	emit(starting(C-N)),
	emit(stopped(C-N, fail)).

terminate(C-N) :-
	unlisten(C-N, _, _),
	retractall(running(C-N)),
	emit(stopped(C-N, terminated)).

tick_start(C-N) :-
	bt_impl:node_(_, N, '?', [GuardName], _),
	\+ guard(C, GuardName),
	unlisten(C-N, _, _),
	retractall(running(C-N)),
	emit(stopped(C-N, fail)).


