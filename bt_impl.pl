:- module(bt_impl, [
	      def_node/4,   % node(+Head, +Oper, +Args, +Children)
	      start_context/2, % start_context(+Root, +Context)
	      end_context/2, % end_context(+Root, +Context)
	      start_simulation/3, % start_simulation(+StartTime, +TimeUnit, +TickLength)
	      do_tick/0,   % do_tick
	      check_nodes/0 % check_nodes
	 ]).

:-license(proprietary).

:- dynamic node_/4,
	current_time/1,
	time_unit/1,
	tick_length/1.

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
	node_(Head, _, _, _),
	line_count(current_input, Line),
	format(atom(Msg), '~w is multiply defined on line ~d.', [Head, Line]),
	print_message(error, error(permission_error(modify, procedure, Head),
				   context(node:Head, Msg))).
def_node(Head, Oper, Args, Children) :-
	\+ node_(Head, _, _, _),
	asserta(node_(Head, Oper, Args, Children)).

%!	check_nodes is semidet
%
%	Succeeds if all referenced nodes are defined
%	emits messages if not.
%
check_nodes :-
	setof(Node, H^O^A^(node_(H, O, A, Children), member(Node, Children)), Used),
	maplist(check_def, Used).

check_def(Node) :- node_(Node, _, _, _).
check_def(Node) :-
	(   setof(Head, O^A^(node_(Head, O, A, Children), member(Node, Children)), Heads)
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

%! start_context(Root, Context)
%	is det
%
%	Start the node Root at the current clocktime

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
start_node(Name, Args) :-
	get_oper(Name, -? ),
	get_args(Name, Args).

%!	tick_node(+Oper:atom, -Status:status) is det
%
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



