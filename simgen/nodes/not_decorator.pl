:- module(not_decorator, []).

:- dynamic running/1.

:- use_module(simgen(bt_impl), [make_cn/2, emit/1]).

:-listen(simulation_starting, reset).

reset :-
	retractall(running(_)).

:- multifile bt_impl:make_cn_impl/3.

bt_impl:make_cn_impl(not , C-N, _-_) :-
	running(C-N),
	!.
bt_impl:make_cn_impl(not, C-N, CParent-NParent) :-
	asserta(running(C-N)),
	listen(C-N, terminate(C-N), not_decorator:terminate(C-N)),
	listen(C-N, terminate_if_child(CParent-NParent),
	       not_decorator:terminate(C-N)),
	emit(starting(C-N)),
	start_child(C-N).

% start the only child of C-N, which is a not decorator
start_child(C-N) :-
	bt_impl:node_(_M, N, not, _Args, [Kiddo]),
	make_cn(C-Kiddo, C-N),
	% stop the parent if the child stops
	listen(C-N, stopped(C-Kiddo, Reason),
	       stop_me(C-N, Reason)).

stop_me(C-N, fail) :-
	unlisten(C-N, _, _),
	retractall(running(C-N)),
	emit(stopped(C-N, done)).
stop_me(C-N, done) :-
	unlisten(C-N, _, _),
	retractall(running(C-N)),
	emit(stopped(C-N, fail)).

terminate(C-N) :-
	unlisten(C-N, _, _),
	emit(terminate_if_child(C-N)),
	retractall(running(C-N)),
	emit(stopped(C-N, terminated)).

