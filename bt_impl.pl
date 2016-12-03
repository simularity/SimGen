:- module(bt_impl, [
	      reset_nodes_for_module/1,
	      set_current_bt_module/0,
	      def_node/4,   % node(+Head, +Oper, +Args, +Children)
	      start_context/3, % start_context(+Root, +Context, +Time)
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
	do_tasks(config(TimeUnit, TickLength, External),
		 [clock(simgen, StartTime)],   % only clock in list is simgen
		 [], % vals empty at start of tick
		 [], % no oldvals, no previous tick
		 [], % qtasks is empty, start of tick
		 [], % qtnt is empty
		External).

%!	end_simulation is det
%
%	Stop a running simulation at the
%	end of the tick
%
%   Must be called from the simulation thread
%   (usually a message listener)
%
end_simulation :-
	thread_send_message(simgen, end_simulation).

start_context(Root, Context, Time) :-
	thread_send_message(simgen, task(Context, Root, start_context_clock(Context, Time))),
	thread_send_message(simgen, task(Context, Root, run_node(Context, Root))). % this IS first tick

%
		 /*******************************
		 * Support for User API         *
		 *******************************/

		 /*******************************
		 *	   Simulator            *
		 *******************************/

/* the interpreter runs in cycles.
 * A cycle
0. send the tick message, and replace the Extern with NewExtern
1. get messages from an external message queue and add the associated
tasks to the task queue.
2. Run tasks, responding to shifts. The balls:
    * qtask(Task)  - Queue the argument to be run in this tick
    * qtnt(Task) - Queue the argument for the next tick
    * end_simulation - stop the simulation
    * getval(Name, Val) - bind Val if you can
    * setval(Name, Val) - record val
    * getclock(Name, Time) - Name is a Context for context clock
    * terminate(Node, Context) - remove all tasks from qtask and
      qtnt that unify with Node and Context
3. increment the clocks
4. start another tick, using the task_next_tick queue as the tick queue
and with a fresh set of vals

The Sim object is evil. Have a list of clocks, extern, values, qtask,
and qtnt

Task format  =|task(Context, Node, Goal)|=

do_tick and do_task mutually recursive? No - have do_task that gets
[H|T]. When it gets [], it starts a new cycle. Might have a timeout.

*/

/*
 *  Queue to send messages into the system during simulation
 */
:- initialization (  message_queue_property(_, alias(simgen)),
		     message_queue_destroy(simgen)
		  ;
		     true
		  ),
		  message_queue_create(_, [alias(simgen)]).


%!	do_tasks(+Config:term, +Clocks:list, +Vals:list,
%!	+OldVals:list, +QTasks:list, +QTNT:list, +External:term) is det
%
%	perform simgen tasks. This is the big kahuna.
%
%	@arg Config term with misc stuff we need
%	@arg Clocks a list of terms clock(Name, Time)
%	@arg Vals a list of continuous values known for this tick
%	@arg OldVals a list of continuous values known for last tick
%	@arg QTasks a list of tasks to run this tick
%	@arg QTNT a list of tasks to run next tick
%
do_tasks(_, _, _, _, _, _, _) :-
	end_simulation_message_exists.
do_tasks(Config, Clocks, Vals, _, [], [], QTNT) :-
	% no more tasks, move to next tick
	memberchk(clock(simgen, Time), Clocks),
	Config = config(TimeUnit, TickLength, Extern),
	broadcast(tick(Extern, Time, NewExtern)),
	get_message_tasks(MessageTasks),
	append(QTNT, MessageTasks, NewTasks),
	increment_clocks(Config, Clocks, NewClocks),
	do_tasks(config(TimeUnit, TickLength, NewExtern),
		 NewClocks,
		 [],
		 Vals,
		 NewTasks,
		 []).
do_tasks(Config, Clocks, Vals, OldVals, [task(Context, Node, Goal) | Rest], QTNT) :-
    with_context(Context, reset(Goal, Ball, Continuation)),
    handle_the_ball(Ball,
		    Config,
		    Clocks,
		    Vals,
		    OldVals,
		    Rest,
		    QTNT,
		    Context,
		    Node,
		    Continuation).

%!	handle_the_ball(+Ball:ball,
%!           +Config:term,
%!           +Clocks:list,
%!           +Vals:list,
%!           +OldVals:list,
%!           +QTasks:list,
%!           +QTNT:list,
%!           +Context:integer,
%!           +Node:atom,
%!           +Continuation) is det
%
%           Passed a ball from a task, handle it and
%           continue simulating.
%
%   @arg Ball the ball passed by the task
%   @arg Config term of form config(TimeUnit, TickLength, Extern)
%   @arg Clocks list of clocks
%   @arg Vals list of value terms of the form val(VariableName, Context,
%          Node, Val)
%   @arg OldVals list of value terms from the previous cycle
%   @arg QTasks list of queued tasks
%   @arg QTNT list of tasks queud for next tick
%   @arg Context the context of the current ball
%   @arg Node the node that threw the current ball
%   @arg Continuation the continuation to call to resume processing
%

% I am out of balls for this task
handle_the_ball(    0,
		    Config,
		    Clocks,
		    Vals,
		    OldVals,
		    QTasks,
		    QTNT,
		    _Context,
		    _Node,
		    _) :-
	do_tasks(Config, Clocks, Vals, OldVals, QTasks, QTNT).
% Queue a task for execution this tick
handle_the_ball(    qtask(Task),
		    Config,
		    Clocks,
		    Vals,
		    OldVals,
		    QTasks,
		    QTNT,
		    Context,
		    Node,
		    Continuation) :-
	append(QTasks, [Task], NewTasks),
	with_context(Context, reset(Continuation, Ball, NewContinuation)),
	handle_the_ball(Ball,
		    Config,
		    Clocks,
		    Vals,
		    OldVals,
		    NewTasks,
		    QTNT,
		    Context,
		    Node,
		    NewContinuation).
% Queue a task to continue next tick
handle_the_ball(    next_tick(NodeContext, NodeName),
		    Config,
		    Clocks,
		    Vals,
		    OldVals,
		    QTasks,
		    QTNT,
		    Context,
		    Node,
		    Continuation) :-
	append(QTNT, [task(NodeContext, NodeName, Continuation)], NewQTNT),
	with_context(Context, reset(Continuation, Ball, NewContinuation)),
	handle_the_ball(Ball,
		    Config,
		    Clocks,
		    Vals,
		    OldVals,
		    QTasks,
		    NewQTNT,
		    Context,
		    Node,
		    NewContinuation).

% Queue a task for execution next tick
handle_the_ball(    qtnt(Task),
		    Config,
		    Clocks,
		    Vals,
		    OldVals,
		    QTasks,
		    QTNT,
		    Context,
		    Node,
		    Continuation) :-
	with_context(Context, reset(Continuation, Ball, NewContinuation)),
	handle_the_ball(Ball,
		    Config,
		    Clocks,
		    Vals,
		    OldVals,
		    QTasks,
		    [Task | QTNT],
		    Context,
		    Node,
		    NewContinuation).
% End the simulation.
handle_the_ball(end_simulation, _, _, _, _, _, _, _, _, _).
% Get the value this tick
% Caller must check value. If it's ground, life is good.
% if not, Must be called again
% til you get grounded value
handle_the_ball(    getval(Name, Val),
		    Config,
		    Clocks,
		    Vals,
		    OldVals,
		    QTasks,
		    QTNT,
		    Context,
		    Node,
		    Continuation) :-
	(   get_the_value(Context, Name, Vals, Val, _),
	    with_context(Context, reset(Continuation, Ball, NewContinuation)),
	    handle_the_ball(Ball,
		    Config,
		    Clocks,
		    Vals,
		    OldVals,
		    QTasks,
		    QTNT,
		    Context,
		    Node,
		    NewContinuation)
	;
	    append(QTasks, [task(Context, Node, Continuation)], NewQTasks),
	    do_tasks(Config, Clocks, Vals, OldVals, NewQTasks, QTNT)
	).

% get the value last tick
handle_the_ball(    lastval(Name, Val),
		    Config,
		    Clocks,
		    Vals,
		    OldVals,
		    QTasks,
		    QTNT,
		    Context,
		    Node,
		    Continuation) :-
	(   get_the_value(Context, Name, OldVals, Val, _)
	;   print_message(error,
		bt_fatal_error(flow_error(no_source_last_cycle), culprit(Node, Context, Name, none)))
        ),
	with_context(Context, reset(Continuation, Ball, NewContinuation)),
	handle_the_ball(Ball,
		    Config,
		    Clocks,
		    Vals,
		    OldVals,
		    QTasks,
		    QTNT,
		    Context,
		    Node,
		    NewContinuation).
% Set the value
handle_the_ball(    setval(Name, Val),
		    Config,
		    Clocks,
		    Vals,
		    OldVals,
		    QTasks,
		    QTNT,
		    Context,
		    Node,
		    Continuation) :-
	(   get_the_value(Context, Name, OldVals, _AVal, By),
	    print_message(error, bt_fatal_error(flow_error(multiple_sources), culprit(Node, Context, Name, By)))
	;
	    with_context(Context, reset(Continuation, Ball, NewContinuation)),
	    handle_the_ball(Ball,
		    Config,
		    Clocks,
		    [val(Name, Context, Node, Val) | Vals],
		    OldVals,
		    QTasks,
		    QTNT,
		    Context,
		    Node,
		    NewContinuation)
	).
% get the clock
%
% clocks are named simgen for the master clock, the context for the
% context clock, and Context-Node-Something for other clocks
%
handle_the_ball(    getclock(Name, Val),
		    Config,
		    Clocks,
		    Vals,
		    OldVals,
		    QTasks,
		    QTNT,
		    Context,
		    Node,
		    Continuation) :-
	(   member(clock(Name, Val), Clocks)
	;
	    print_message(error, bt_fatal_error(flow_error(no_clock), culprit(Node, Context, Name)))
	),
	with_context(Context, reset(Continuation, Ball, NewContinuation)),
        handle_the_ball(Ball,
		    Config,
		    Clocks,
		    [val(Name, Context, Node, Val) | Vals],
		    OldVals,
		    QTasks,
		    QTNT,
		    Context,
		    Node,
		    NewContinuation).
handle_the_ball(    newclock(Name, Time),
		    Config,
		    Clocks,
		    Vals,
		    OldVals,
		    QTasks,
		    QTNT,
		    Context,
		    Node,
		    Continuation) :-
	(   member(clock(Name, _), Clocks),
	    print_message(error, bt_fatal_error(flow_error(restart_clock), culprit(Node, Context, Node)))
	;
	   true
	),
	with_context(Context, reset(Continuation, Ball, NewContinuation)),
        handle_the_ball(Ball,
		    Config,
		    [clock(Name, Time) | Clocks],
		    Vals,
		    OldVals,
		    QTasks,
		    QTNT,
		    Context,
		    Node,
		    NewContinuation).

% terminate(Node, Context) - remove all tasks from qtask and
%      qtnt that unify with Node and Context
handle_the_ball(    terminate(Node, Context),
		    Config,
		    Clocks,
		    Vals,
		    OldVals,
		    QTasks,
		    QTNT,
		    Context,
		    Node,
		    Continuation) :-
	    with_context(Context, reset(Continuation, Ball, NewContinuation)),
	    select(task(Context, Node, _), QTasks, NewQTasks),
	    select(task(Context, Node, _), QTNT, NewQTNT),
	    handle_the_ball(Ball,
		    Config,
		    Clocks,
		    Vals,
		    OldVals,
		    NewQTasks,
		    NewQTNT,
		    Context,
		    Node,
		    NewContinuation).

with_context(Context, Goal) :-
	b_getval(context, OldContext),
	b_setval(context, Context),
	call(Goal),
	b_setval(context, OldContext).

current_context(Context) :-
	b_getval(context, Context).

with_node(Node, Goal) :-
	b_getval(current_node, OldNode),
	b_setval(current_node, Node),
	call(Goal),
	b_setval(current_node, OldNode).

current_node(Node) :-
	b_getval(current_node, Node).


get_message_tasks([task(Context, Node, Goal) | Rest]) :-
	thread_get_message(simgen, task(Context, Node, Goal), [timeout(0)]),
	get_message_tasks(Rest).
get_message_tasks([]).

end_simulation_message_exists :-
	thread_get_message(simgen, end_simulation, [timeout(0)]).

increment_clocks(_, [], []).
increment_clocks(config(TimeUnit, TickLength, External),
		 [clock(Name, Time) | Clocks],
		 [clock(Name, NewTime) | NewClocks]) :-
	NewTime is Time + TickLength,
	increment_clocks(config(TimeUnit, TickLength, External), Clocks, NewClocks).

get_the_value(Context, Name, Vals, Val, Node) :-
	member(val(Name, Context, Node, Val), Vals).

		 /*******************************
		 * Run Time Library	       *
		 *******************************/

start_context_clock(Context, Time) :-
	shift(newclock(Context, Time)).

run_node(Context, Node) :-
	node_(_M, Node, Op, Args, Children),
	with_context(Context, with_node(Node, run_node(Op, Args, Children))).

run_node(Node) :-
	current_context(Context),
	run_node(Context, Node).

run_node(~? , Args, Children) :-
	sum_list(Args, Total),
	Select is random_float * Total,
	run_random(Select, Args, Children).
run_node('!' , [FirstTick, OtherTicks], _) :-
	eval(FirstTick),
	current_context(Context),
	current_node(Node),
	shift(next_tick(Context, Node)),
	more_eval(OtherTicks).

more_eval(Statements) :-
	eval(Statements),
	current_context(Context),
	current_node(Node),
	shift(next_tick(Context, Node)),
	more_eval(Statements).

run_random(_Select, _, [Child]) :-
	run_node(Child).
run_random(Select, [A |_], [Child | _]) :-
	Select < A,
	run_node(Child).
run_random(Select, [A |T], [_ | Kids]) :-
	Select >= A,
	NS is Select - A,
	run_random(NS, T, Kids).
run_random(_, [], _) :-
	current_node(Node),
	print_message(warning, bt_nonfatal_error(node_error(no_child_to_run), culprit(Node))).

		 /*******************************
		 * Continuous evaluator
		 *******************************/

eval([]).
eval([H | T]) :-
	eval(H),
	eval(T).
eval(':='(LVAL, RVAL)) :-
	eval_rval(lastval, RVAL, Value),
	shift(setval(LVAL, Value)).
eval('='(LVAL, RVAL)) :-
	eval_rval(getval, RVAL, Value),
	shift(setval(LVAL, Value)).

% note you have to call getval in a loop - make convenience preds for
% getval and lastval


% TODO make good messages
