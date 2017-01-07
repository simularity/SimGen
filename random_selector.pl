:- module(random_selector, []).

:- dynamic running/1.

:- use_module(bt_impl, [make_cn/2, emit/1]).

:-listen(simulation_starting, reset).

reset :-
	retractall(running(_)).

:- multifile bt_impl:make_cn_impl/3.

bt_impl:make_cn_impl(~? , C-N, _-_) :-
	running(C-N),
	!.
bt_impl:make_cn_impl(~?, C-N, CParent-NParent) :-
	asserta(running(C-N)),
	listen(C-N, terminate(C-N), random_selector:terminate(C-N)),
	listen(C-N, terminate_if_child(CParent-NParent),
	       random_selector:terminate(C-N)),
	start_a_child(C-N),
	emit(starting(C-N)).

start_a_child(C-N) :-
	bt_impl:node_(_M, N, ~?, Args, Kiddos),
	sum_list(Args, Total),
	Select is random_float * Total,
	run_random(C-N, Select, Args, Kiddos).

run_random(C-N, _Select, _, [Child]) :-
	start_this(C-Child, C-N).
run_random(C-N, Select, [A |_], [Child | _]) :-
	Select < A,
	start_this(C-Child, C-N).
run_random(C-N, Select, [A |T], [_ | Kids]) :-
	Select >= A,
	NS is Select - A,
	run_random(C-N, NS, T, Kids).
run_random(C-N, _, [], _) :-
	print_message(warning, bt_nonfatal_error(node_error(no_child_to_run), culprit(C-N))).

start_this(C-ToStart, C-Parent) :-
	% start one of the children
	make_cn(C-ToStart, C-Parent),
	% stop the parent if the child stops
	listen(C-Parent, stopped(C-ToStart, Reason),
	       stop_me(C-Parent, Reason)).

stop_me(C-N, How) :-
	unlisten(C-N, _, _),
	retractall(running(C-N)),
	emit(stopped(C-N, How)).

terminate(C-N) :-
	unlisten(C-N, _, _),
	emit(terminate_if_child(C-N)),
	retractall(running(C-N)),
	emit(stopped(C-N, terminated)).

