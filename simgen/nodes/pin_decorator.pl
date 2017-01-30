:- module(pin_decorator, []).
/** <module> pin operator.
 *
 * If the child succeeds, succeeds
 *
 * If the child fails, emits pin_drop(Context, Time, event)
 * and pin_drop(Context, Time, -event)
 * pair to mark boundaries
 */

:- dynamic running/2. % C-N, StartContextTime

:- use_module(simgen(bt_impl), [make_cn/2, emit/1]).
:- use_module(simgen(clocks), [get_clock/2]).

:-listen(simulation_starting, reset).

reset :-
	retractall(running(_, _)).

:- multifile bt_impl:make_cn_impl/3.

bt_impl:make_cn_impl(pin , C-N, _-_) :-
	running(C-N, _),
	!.
bt_impl:make_cn_impl(pin, C-N, CParent-NParent) :-
	get_clock(C, Time),
	asserta(running(C-N, Time)),
	listen(C-N, terminate(C-N), pin_decorator:terminate(C-N)),
	listen(C-N, terminate_if_child(CParent-NParent),
	       pin_decorator:terminate(C-N)),
	emit(starting(C-N)),
	start_child(C-N).

% start the only child of C-N, which is a pin decorator
start_child(C-N) :-
	bt_impl:node_(_M, N, pin, _Args, [Kiddo]),
	make_cn(C-Kiddo, C-N),
	% stop the parent if the child stops
	listen(C-N, stopped(C-Kiddo, Reason),
	       stop_me(C-N, Reason)).

stop_me(C-N, fail) :-
	unlisten(C-N, _, _),
	running(C-N, Time),
	emit(pin_drop(C, Time, event)),
	get_clock(C, Now),
	emit(pin_drop(C, Now, -event)),
	retractall(running(C-N, _)),
	emit(stopped(C-N, done)).
stop_me(C-N, done) :-
	unlisten(C-N, _, _),
	retractall(running(C-N, _)),
	emit(stopped(C-N, done)).

terminate(C-N) :-
	unlisten(C-N, _, _),
	emit(terminate_if_child(C-N)),
	retractall(running(C-N, _)),
	emit(stopped(C-N, terminated)).

