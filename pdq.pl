:- module(pdq, []).

:- dynamic running/1.

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
	retractall(running(C-N)),
	emit(stopped(C-N, How)).

terminate(C-N) :-
	unlisten(C-N, _, _),
	retractall(running(C-N)),
	emit(stopped(C-N, terminated)).


