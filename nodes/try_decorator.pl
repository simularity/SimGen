:- module(try_decorator, []).

:- dynamic running/1.

:- use_module(bt_impl, [make_cn/2, emit/1]).

:-listen(simulation_starting, reset).

reset :-
	retractall(running(_)).

:- multifile bt_impl:make_cn_impl/3.

bt_impl:make_cn_impl(try , C-N, _-_) :-
	running(C-N),
	!.
bt_impl:make_cn_impl(try, C-N, CParent-NParent) :-
	asserta(running(C-N)),
	listen(C-N, terminate(C-N), try_decorator:terminate(C-N)),
	listen(C-N, terminate_if_child(CParent-NParent),
	       try_decorator:terminate(C-N)),
	start_child(C-N),
	emit(starting(C-N)).

start_child(C-N) :-
	bt_impl:node_(_M, N, try, _Args, [Kiddo]),
	make_cn(C-Kiddo, C-N),
	% stop the parent if the child stops
	listen(C-Parent, stopped(C-Kiddo, Reason),
	       stop_me(C-Parent, Reason)).

stop_me(C-N, How) :-
	member(How, [fail, done]),
	unlisten(C-N, _, _),
	retractall(running(C-N)),
	emit(stopped(C-N, done)).

terminate(C-N) :-
	unlisten(C-N, _, _),
	emit(terminate_if_child(C-N)),
	retractall(running(C-N)),
	emit(stopped(C-N, terminated)).

