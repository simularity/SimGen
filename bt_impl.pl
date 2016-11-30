:- module(bt_impl, [
	      reset_nodes_for_module/1,
	      set_current_bt_module/0,
	      def_node/4,   % node(+Head, +Oper, +Args, +Children)
	      start_context/4, % start_context(+Root, +Context, +Time, :Sim)
%	      end_context/1, % end_context(+Context),
	      start_simulation/4, % start_simulation(+StartTime, +TimeUnit, +TickLength, +External)
	      end_simulation/0,
%	      bt_time/1, % get the time of the global clock
%	      bt_context_time/2, % get the time of the context clock
	      check_nodes/0 % check_nodes
	 ]).
/** <module> run time support for bt
 *
 * "If you have done something twice, you are likely to do it again."
 *
 * Brian Kernighan and Bob Pike
*/

		 /*******************************
		 * Compilation support          *
		 *******************************/

:- dynamic node_/5.   % node_(Module, Head, Operator, Args, Children)

:- module_transparent set_current_bt_module/0.

%!	set_current_bt_module is det
%
%	module_transparent predicate that
%	records the calling module.
%	def_node/4 uses this info
%
%	Users usually won't call this.
%	It's for the compiler
%
set_current_bt_module :-
	writeln('in set_current_bt_module\n'),
	context_module(Module),
	nb_setval(current_bt_module, Module).

%!	reset_nodes_for_module(+Module:atom) is det
%
%	reset the nodes for the passed in module
%
%       Users usually won't call this, it's for the compiler
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
%       Called at end of module compilation
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

		 /*******************************
		 *          User API            *
		 *******************************/

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
		    run_simulator: true,
		    current_time: StartTime,
		    time_unit: TimeUnit,
		    tick_length: TickLength,

		    % TODO need separate lists of nodes that are running
		    % and current tasks
		    running: [],   % list of context=closure pairs
		    context: [],    % context dicts
		    to_be_started: [],
		    to_be_terminated: [],
		    frozen_flow_nodes: [],
		    external: External
		},
	do_cycles(Sim).

%!	end_simulation is det
%
%	Stop a running simulation at the
%	end of the tick
%
%   Must be called from the simulation thread
%   (usually a message listener)
%
end_simulation :-
	shift(end_simulation).


%
		 /*******************************
		 * Support for User API         *
		 *******************************/

		 /*******************************
		 *	   Simulator            *
		 *******************************/

/* the interpreter runs in cycles.
 * A cycle
0. send the tick_start message
1. get messages from an external message queue and add the associated
tasks to the task queue.
2. Run tasks, responding to shifts. The balls:
    * qtask(Task)  - Queue the argument to be run in this tick
    * qtnt(Task) - Queue the argument for the next tick
    * end_simulation - stop the simulation
    * getval(Name, Val) - bind Val if you can
    * setval(Name, Val) - record val
    * getclock(Name, Time) - Name is a Context for context clocks
3. increment the clocks
4. start another tick, using the task_next_tick queue as the tick queue


The Sim object is evil. Have a list of clocks, qtask, and qtnt


Task format

task(Context, Node, Goal)




1. Remove all tasks to be terminated, calling terminate on each.
2. Start all tasks scheduled to be started. If there is a scheduled
task with same context and node already running, ignore the restart.
foreach context:
   2.1, send the tick message
   2.2. Run tasks
3. If there are frozen flow nodes, print an error message and
halt the simulation.
4. increment the time for all clocks


to run ticks:
with all running context-node pairs
    run the node.
    if you get a continuation back with a ball of form end_simulation
    stop the simulator.
    if you get a continuation back with a ball of
    form keep_running(Context, Node) shove the continuation on the list
    if you get a continuation back with a ball of form
    terminate(Context, Node) eliminate this node from the run list, then
    recursively call the continuation. % have to be explicit about
    context, it could change below if you get a continuation back with
    set_dynamic(Context, VarName, Value) set the value, check for newly
    runnable flow nodes, run them, and remove from frozen flow notdes,
    and run the continuation. if you get a continuation back with
    get_dynamic(Context, VarName, Value) and you have a value, bind
    Value to it and call the continuation if you get a continuation back
    with get_dynamic and don't have the value, put it on the list of
    non-runnable flow nodes

*/

/*
 *  Queue to send messages into the system during simulation
 */
:- initialization message_queue_create(_, [alias(simgen)]).

%!	do_cycles(+Sim:dict) is det
%
%	Run the simulator
%	Implements the basic simulator construct
%
do_cycles(Sim) :-
	_{ run_simulator: false } :< Sim,
	!.
do_cycles(Sim) :-
	broadcast(tick_start(Sim.current_time)),
	process_pending_messages(Sim),
	terminate
	Sim.running
	terminate_terminated(Sim.running, Sim.to_be_terminated, StillRunning),
	start_started(StillRunning, Sim.to_be_started, NowRunning),
	nb_set_dict(running, Sim, NowRunning),
	do_context_dependent(Sim),
	Sim.frozen_flow_nodes == [],
	increment_clocks(Sim),
	do_cycles(Sim).

%!	do_context_dependent(+Sim:dict) is det
%
%	Do the context dependent part of the tick
%
% with all running context-node pairs
%    run the node.
%    if you get a continuation back with a ball of form end_simulation
%    stop the simulator.
%    if you get a continuation back with a ball of
%    form keep_running(Context, Node) shove the continuation on the list
%    if you get a continuation back with a ball of form
%    terminate(Context, Node) eliminate this node from the run list,
%    then
%    recursively call the continuation. % have to be explicit about
%    context, it could change below if you get a continuation back with
%    set_dynamic(Context, VarName, Value) set the value, check for newly
%    runnable flow nodes, run them, and remove from frozen flow notdes,
%    and run the continuation. if you get a continuation back with
%    get_dynamic(Context, VarName, Value) and you have a value, bind
%    Value to it and call the continuation if you get a continuation
%    back
%    with get_dynamic and don't have the value, put it on the list of
%    non-runnable flow nodes
%
do_context_dependent(Sim) :-
   run_nodes(Sim, Sim.running).

run_nodes(_Sim, []).
run_nodes(Sim, [H | T]) :-
	  task{node: H.node,
		   context: H.context,
		   first_tick: true
		  } :< H,





%!	terminate_terminated(+InTasks:list, +Terminate:list, -OutTasks)
%!	is det
%
%	Terminate and remove terminated tasks from the list.
%
%	@arg InTasks list of tasks
%	@arg Terminate list to terminate, of dicts of form
%            _{ node:Node,
%	        context: Context
%	        }
%	@arg OutTasks  list of tasks with with terminated ones removed
%
terminate_terminated([], _, []).
terminate_terminated([H | InTasks], Terminate, OutTasks) :-
	member(T, Terminate),
	T :< H,
	!,
	terminate_task(H),
	terminate_terminated(InTasks, Terminate, OutTasks).
terminate_terminated([H | InTasks], Terminate, [H | OutTasks]) :-
	terminate_terminated(InTasks, Terminate, OutTasks).

%!	start_started(+AlreadyRunning:list, +ToStart:list,
%!	-NowRunning:list) is det
%
%	add the started tasks to the list, starting each
%
start_started(AlreadyRunning, [], AlreadyRunning).
start_started(AlreadyRunning, [H | ToStart], NowRunning) :-
	member(T, AlreadyRunning),
	_{ node: H.node,
	   context: H.context
	 } :< T,
	 !,
	 start_started(AlreadyRunning, ToStart, NowRunning).
start_started(AlreadyRunning, [H | ToStart], NowRunning) :-
	do_start_node(H, HTask),
	start_started([HTask | AlreadyRunning], ToStart, NowRunning).


% for now this just makes the task dict
do_start_node(H,
	      task{node: H.node,
		   context: H.context,
		   first_tick: true
		  }).

% for now this just quits giving cycles
% % maybe a broadcast event?
terminate_task(_).

get_context(Sim, Context, ContextDict) :-
	member(ContextDict, Sim.context),
        ContextDict.context == Context.


% TODO fix this

% ! start_context(+Root:atom, +Context:integer, +Time:number, :Sim:dict)
% is det
%
%	Start the node Root in a new context Context
%	with local clock Time
%
%	Must only be called within a listener
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
