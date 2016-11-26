:- module(bt_impl, [
	      reset_nodes_for_module/1,
	      set_current_bt_module/0,
	      def_node/4,   % node(+Head, +Oper, +Args, +Children)
	      start_context/4, % start_context(+Root, +Context, +Time, :Sim)
%	      end_context/1, % end_context(+Context),
	      start_simulation/4, % start_simulation(+StartTime, +TimeUnit, +TickLength, +External)
%	      bt_time/1, % get the time of the global clock
%	      bt_context_time/2, % get the time of the context clock
	      check_nodes/0 % check_nodes
	 ]).

:- use_module(library(pce)).

:- dynamic node_/5.   % node_(Module, Head, Operator, Args, Children)

:- module_transparent set_current_bt_module/0.

set_current_bt_module :-
	writeln('in set_current_bt_module\n'),
	context_module(Module),
	nb_setval(current_bt_module, Module).

%!	reset_nodes_for_module(+Module:atom) is det
%
%	reset the nodes for the passed in module
%
reset_nodes_for_module(Module) :-
	retractall(node_(Module, _, _, _, _)).

%! def_node(+Head:atom, +Oper:atom, +Args:args, +Children:list) is det
%
%	Define a node.
%	A _node_ is a cell in an behavior tree
%
%	@arg Head the name of the node, an atom
%	@arg Oper the name of the operation type for the node
%	@arg Args a single item or list of items. Meaning depends on
%	Oper
%	@arg Children a list of
%	@tbd change this to a term_expansion
%
def_node(Head, _, _, _) :-
	node_(_, Head, _, _, _),
	line_count(current_input, Line),
	format(atom(Msg), '~w is multiply defined on line ~d.', [Head, Line]),
	print_message(error, error(permission_error(modify, procedure, Head),
				   context(Head, Msg))).
def_node(Head, Oper, Args, Children) :-
	\+ node_(_, Head, _, _, _),
	debug(bt, 'node ~w ~w ~w ~w~n', [Head, Oper, Args, Children]),
	nb_getval(current_bt_module, Module),
	assertz(node_(Module, Head, Oper, Args, Children)),
	debug(bt, 'asserted~n', []).

%!	check_nodes is semidet
%
%	Succeeds if all referenced nodes are defined
%	emits messages if not.
%
check_nodes :-
	\+ node_(_, _, _, _, _),
	!.
check_nodes :-
	setof(Node, a_used_node(Node), Used),
	maplist(check_def, Used).

a_used_node(Node) :-
	node_(_, _, _, _, Children),
	member(Node, Children).

check_def(Node) :- node_(_, Node, _, _, _).
check_def(Node) :-
	(   setof(Head, M^O^A^(node_(M, Head, O, A, Children), member(Node, Children)), Heads)
	; Heads = []),
	maplist(print_no_def(Node), Heads).

print_no_def(Node, Head) :-
	format(atom(Msg), 'Node ~w is used in node ~w but is not defined', [Node, Head]),
	print_message(error, error(existance_error(procedure, Node),
					      context(node:Head, Msg))).

%!     start_simulation(
%! +StartTime:number, +TimeUnit:number, +TickLength:number,
%! +External:term) is det
%
%	run a new simulation
%
%	@arg StartTime time in user units to run the first tick at
%	@arg TimeUnit how long is one user unit in nanos?
%	@arg TickLength how long is a tick in user units?
%	@arg external data for use by event listeners
%
start_simulation(StartTime, TimeUnit, TickLength, External) :-
	Sim =
		sim{
		    current_time: StartTime,
		    time_unit: TimeUnit,
		    tick_length: TickLength,
		    running: [],   % list of context=closure pairs
		    context: [],    % context dicts
		    external: External
		},
	do_tick(Sim).

get_context(Sim, Context, ContextDict) :-
	member(ContextDict, Sim.context),
        ContextDict.context == Context.

% ! start_context(+Root:atom, +Context:integer, +Time:number, :Sim:dict)
% is det
%
%	Start the node Root in a new context Context
%	with local clock Time
%
start_context(_Root, Context, Time, Sim) :-
	get_context(Sim, Context, _),
	format(atom(Msg), 'At ~w attempt to modify context ~w, which already exists', [Time, Context]),
	throw(error(permission_error(modify, context, Context), context(Context, Msg))).
start_context(Root, Context, Time, Sim) :-
	\+ get_context(Sim, Context, _),
	node_(_, Root, _, _, _),
	NewContextDict = context{
			     context: Context,
			     time: Time
			 },
	nb_set_dict(context, Sim, [NewContextDict | Sim.context]),
	with_context(Context, start_node(Sim, Root)).

% TODO done to here, converting to use destructively assigned dict for
% sim and context and delimited continuations to run

:- meta_predicate with_context(+, 0).

with_context(Context, Goal) :-
	b_getval(current_context, OldContext),
	b_setval(current_context, Context),
	call(Goal),
	b_setval(current_context, OldContext).

%!	do_n_ticks(+N:integer) is det
%
%	call do_tick N times
%
do_n_ticks(N) :-
	between(1, N, _),
	do_tick,
	fail.
do_n_ticks(_).

%!	do_tick is det
%
%	run the system, advancing the global clock one tick
do_tick :-
	run_all_contexts,
	advance_tick.

run_all_contexts :-
	current_context(Context, ContextData),
	_{ running: Nodes } :< ContextData,
	with_context(Context, run_nodes_to_exhaustion(Nodes)),
	fail.
run_all_contexts.



:- discontiguous start_node/1, tick_node/6.

% Clock, Context, and TickVal will be b_setval'ed
%
% TODO should we reset the simulation on make?
%

%!	start_node(+Name:atom) is det
%
%	start node Name on the current context
%
%	does not emit event.
%	the first tick will do that if appropriate
%
%	Nodes are not inherently restartable. If a context
%	is already running a node, the call is ignored.
%
/* start_node(Name) :-
	(   node_running(Name)
	->
	true
	;
	node_(_, Node, Op, Args, _),
	node_start_state(Op, Args, Start),
	asserta(
start_node(Name), Args)
	node_(_, Name, Op, Args, _),

. */	% DONE TO HERE

	    % TODO reset contexts and running notes on start

%!	tick_node(+Oper:atom, -Status:status) is det
%
/*
tick_node(Name, Status)  :-
	context_name(CName),
tick_node( -? , _, _, _, _, [], [], succeed).
tick_node( -? ,
	   Context,
	   Clock,
	   TickLen,
	   Args,
	   [Head | Tail],
	   OutState,
	   Status) :-
	tick_node(Head, Context, Clock, TickLen, Arg

*/

