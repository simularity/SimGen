:- module(dur, []).
/** <module> Check a guard. Succeed when it's set
 *
 */
:- dynamic running/2.  % C-N, DoneTime (context)

:- use_module(bt_impl, [emit/1]).
:- use_module(simgen(guard_manager)).
:- use_module(simgen(clocks), [get_clock/2]).

:-listen(simulation_starting, reset).

reset :-
	retractall(running(_, _)).

:- multifile bt_impl:make_cn_impl/3.

bt_impl:make_cn_impl( dur , C-N, _-_) :-
	running(C-N, _), % TODO should this be evil?
	!.
bt_impl:make_cn_impl( dur , C-N, CParent-NParent) :-
	\+ running(C-N, _),
	bt_impl:node_(_, N, dur, [Dur], _),
	get_clock(C, Time),
	EndTime is Time + Dur,
	asserta(running(C-N, EndTime)),
	listen(C-N, terminate(C-N), terminate(C-N)),
	listen(C-N, terminate_if_child(CParent-NParent),
	       terminate(C-N)),
	listen(C-N, tick_start, tick_start(C-N)),
	emit(starting(C-N)).

terminate(C-N) :-
	unlisten(C-N, _, _),
	retractall(running(C-N, _)),
	emit(stopped(C-N, terminated)).

tick_start(C-N) :-
	running(C-N, EndTime),
	get_clock(C, Time),
	(   Time < EndTime
	->  true
	;
	    unlisten(C-N, _, _),
	    retractall(running(C-N, _)),
	    emit(stopped(C-N, done))
	).


