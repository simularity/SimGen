:- module(paraselect, []).

:- dynamic running/2.

:- use_module(bt_impl, [make_cn/2, emit/1]).
:- use_module(simgen(print_system)).

:-listen(simulation_starting, reset).

reset :-
	retractall(running(_, _)).

:- multifile bt_impl:make_cn_impl/3.

bt_impl:make_cn_impl('=?' , C-N, _-_) :-
	running(C-N, _),
	!,
	bt_debug(bt(paraselect, make_cn_impl),
		 '~w-~w already running', [C,N]).
bt_impl:make_cn_impl('=?', C-N, CParent-NParent) :-
	bt_impl:node_(_M, N, '=?', _A, Children),
	emit(starting(C-N)),
	asserta(running(C-N, Children)),
	listen(C-N, terminate(C-N), paraselect:terminate(C-N)),
	listen(C-N, terminate_if_child(CParent-NParent),
	       paraselect:terminate(C-N)),
	bt_debug(bt(paraselect, make_cn_impl),
		 'Starting paraselect ~w-~w and children ~w',
		 [C,N, Children]),
	start_children(C-N, Children).

start_children(_, []).
start_children(C-N, [Child | Rest]) :-
	make_cn(C-Child, C-N),
	% stop the parent if the child stops
	listen(C-N, stopped(C-Child, fail), child_failed(C-N, C-Child)),
	listen(C-N, stopped(C-Child, done), child_done(C-N, C-Child)),
	start_children(C-N, Rest).


%!	child_failed(CNPair:cnpair, How:atom) is det
%
%	respond to a child failing by terminating others and reporting
%	fail
%
child_done(C-N, C-Child) :-
	emit(terminate_if_child(C-N)),
	bt_debug(bt(paraselect, done), '~w-~w: child ~w-~w done, succeeding',
		 [C,N,C,Child]),
	unlisten(C-N, _, _),
	retractall(running(C-N, _)),
	emit(stopped(C-N, done)).

child_failed(C-N, C-Child) :-
	bt_debug(bt(paraselect, child_done),
		 '~w-~w: child ~w-~w failed',
		 [C,N,C,Child]),
	running(C-N, RunningChildren),
	select(Child, RunningChildren, NewRunningChildren),
	(   NewRunningChildren = []
	->
	    unlisten(C-N, _, _),
	    retractall(running(C-N, _)),
	    emit(stopped(C-N, fail))
	;
	    retractall(running(C-N, _)),
	    asserta(running(C-N, NewRunningChildren))
	).

terminate(C-N) :-
	bt_debug(bt(sequence, terminated),
	   '~w-~w is being terminated',
		 [C,N]),
	unlisten(C-N, _, _),
	emit(terminate_if_child(C-N)),
	retractall(running(C-N, _)),
	emit(stopped(C-N, terminated)).

