:- module(bt_impl, [
	      reset_nodes_for_module/1,
	      set_current_bt_module/0,
	      def_node/4,   % node(+Head, +Oper, +Args, +Children)
	      start_context/3, % start_context(+Root, +Context, +Time)
%	      end_context/1, % end_context(+Context),
	      start_simulation/3, % start_simulation(+StartTime, +TimeUnit, +TickLength)
%	      do_tick/0,   % do_tick
%	      do_n_ticks/1, % do_n_ticks
	      time/1, % get the time of the global clock
%	      time/2, % get the time of the context clock
	      check_nodes/0 % check_nodes
	 ]).

:- use_module(library(pce)).

:- dynamic node_/5,   % node_(Module, Head, Operator, Args, Children)
	current_time/1,
	time_unit/1,
	tick_length/1.

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
	nb_getval(current_bt_module, Module),
	assertz(node_(Module, Head, Oper, Args, Children)).

%!	check_nodes is semidet
%
%	Succeeds if all referenced nodes are defined
%	emits messages if not.
%
check_nodes :-
	setof(Node, M^H^O^A^(node_(M, H, O, A, Children), member(Node, Children)), Used),
	maplist(check_def, Used).

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
%!              +StartTime:number, +TimeUnit:number, +TickLength:number)
%
%	Set up a simulation to run
%
%	Call to re/set simulation to initial state
%
%	@arg StartTime time in user units to run the first tick at
%	@arg TimeUnit how long is one user unit in nanos?
%	@arg TickLength how long is a tick in user units?
%
start_simulation(StartTime, TimeUnit, TickLength) :-
	retractall(current_time(_)),
	asserta(current_time(StartTime)),
	retractall(time_unit(_)),
	asserta(time_unit(TimeUnit)),
	retractall(tick_length(_)),
	asserta(tick_length(TickLength)).

:- dynamic current_context/2.

%! start_context(+Root:atom, +Context:integer, +Time:number) is det
%
%	Start the node Root in a new context Context
%	with local clock Time
%
start_context(_Root, Context, Time) :-
	current_context(Context, _),
	format(atom(Msg), 'At ~w attempt to modify context ~w, which already exists', [Time, Context]),
	throw(error(permission_error(modify, context, Context), context(Context, Msg))).
start_context(Root, Context, Time) :-
	\+ current_context(Context, _),
	asserta(current_context(Context, _{
					     running: [],
					     time: Time
					 })),
	node_(_, Root, _, Args, _),
	with_context(Context, start_node(Root, Args)).

with_context(Context, Goal) :-
	b_getval(current_context, OldContext),
	b_setval(current_context, Context),
	call(Goal),
	b_setval(current_context, OldContext).



:- discontiguous start_node/2, tick_node/6.

% Clock, Context, and TickVal will be b_setval'ed
%
		 /*******************************
		 *  priority selector
		 *
		 *******************************/

%!	start_node(+Name:atom, -State:term) is det
%
%	does not emit event
%
/* start_node(Name, Args) :-
	get_oper(Name, -? ),
	get_args(Name, Args).
*/

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

