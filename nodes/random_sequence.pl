:- module(random_sequence, []).

:- dynamic running/2.

:- use_module(bt_impl, [make_cn/2, emit/1]).
:- use_module(simgen(print_system)).

:-listen(simulation_starting, reset).

reset :-
	retractall(running(_, _)).

:- multifile bt_impl:make_cn_impl/3.

bt_impl:make_cn_impl('~>' , C-N, _-_) :-
	running(C-N, _),
	!,
	bt_debug(bt(random_sequence, make_cn_impl),
		 '~w-~w already running', [C,N]).
bt_impl:make_cn_impl('~>', C-N, CParent-NParent) :-
	bt_impl:node_(_M, N, '~>', _A, OriginalChildren),
	random_permutation(OriginalChildren, [FirstChild | RestOfChildren]),
	emit(starting(C-N)),
	asserta(running(C-N, RestOfChildren)),
	listen(C-N, terminate(C-N), random_sequence:terminate(C-N)),
	listen(C-N, terminate_if_child(CParent-NParent),
	       random_sequence:terminate(C-N)),
	bt_debug(bt(random_sequence, make_cn_impl),
		 'Starting random_sequence ~w-~w and child ~w',
		 [C,N, FirstChild]),
	start_a_child(C-N, FirstChild).

start_a_child(C-N, Child) :-
	make_cn(C-Child, C-N),
	% stop the parent if the child stops
	listen(C-N, stopped(C-Child, Reason),
	       next_child(C-N, Reason)).

next_child(C-N, terminated) :-
        bt_debug(bt(random_sequence, terminated), '~w-~w terminated', [C,N]).
next_child(C-N, fail) :-
	bt_debug(bt(random_sequence, next_child),
		 '~w-~w says current child failed',
		 [C,N]),
	unlisten(C-N, _, _),
	retractall(running(C-N, _)),
	emit(stopped(C-N, fail)).
next_child(C-N, done) :-
	running(C-N, []),
	bt_debug(bt(random_sequence, next_child),
		 '~w-~w says current child done with no more children',
		 [C,N]),
	unlisten(C-N, _, _),
	retractall(running(C-N, _)),
	emit(stopped(C-N, done)).
next_child(C-N, done) :-
	running(C-N, [H | T]),
	bt_debug(bt(random_sequence, next_child),
		 '~w-~w says current child done, will run ~w',
		 [C,N,H]),
	retractall(running(C-N, _)),
	asserta(running(C-N, T)),
	make_cn(C-H, C-N),
	unlisten(C-N, stopped(_, _)),
	listen(C-N, stopped(C-H, Reason), next_child(C-N, Reason)).

terminate(C-N) :-
	bt_debug(bt(random_sequence, terminated),
	   '~w-~w is being terminated',
		 [C,N]),
	unlisten(C-N, _, _),
	emit(terminate_if_child(C-N)),
	retractall(running(C-N, _)),
	emit(stopped(C-N, terminated)).

