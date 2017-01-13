:- module(sequence, []).

:- dynamic running/2.

:- use_module(bt_impl, [make_cn/2, emit/1]).

:-listen(simulation_starting, reset).

reset :-
	retractall(running(_, _)).

:- multifile bt_impl:make_cn_impl/3.

bt_impl:make_cn_impl('->' , C-N, _-_) :-
	running(C-N, _),
	!.
bt_impl:make_cn_impl('->', C-N, CParent-NParent) :-
	bt_impl:node_(_M, N, '->', _A, [FirstChild | RestOfChildren]),
	emit(starting(C-N)),
	asserta(running(C-N, RestOfChildren)),
	listen(C-N, terminate(C-N), sequence:terminate(C-N)),
	listen(C-N, terminate_if_child(CParent-NParent),
	       sequence:terminate(C-N)),
	start_a_child(C-N, FirstChild).

start_a_child(C-N, Child) :-
	make_cn(C-Child, C-N),
	% stop the parent if the child stops
	listen(C-N, stopped(C-Child, Reason),
	       next_child(C-N, Reason)).

next_child(_, terminated).
next_child(C-N, fail) :-
	unlisten(C-N, _, _),
	retractall(running(C-N, _)),
	emit(stopped(C-N, fail)).
next_child(C-N, done) :-
	running(C-N, []),
	unlisten(C-N, _, _),
	retractall(running(C-N, _)),
	emit(stopped(C-N, done)).
next_child(C-N, done) :-
	running(C-N, [H | T]),
	retractall(running(C-N, _)),
	asserta(running(C-N, T)),
	make_cn(C-H, C-N),
	unlisten(C-N, stopped(_, _)),
	listen(C-N, stopped(C-H, Reason), next_child(C-N, Reason)).

terminate(C-N) :-
	unlisten(C-N, _, _),
	emit(terminate_if_child(C-N)),
	retractall(running(C-N, _)),
	emit(stopped(C-N, terminated)).

