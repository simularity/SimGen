:- module(behavior_tree,
	  [
	      op(1200, xfx, => ),   % sequence
	      op(1200, xfx, >> ),   % selector
	      op(200, fy, ?),       % variable
	      op(200, fy, ??),      % flag
	      op(700, xfx, ??=),    % set/reset flag
	      op(200, fy, #),       % action
	      op(200, fy, @@),       % probabilistic sample
	      tick/1
	  ]).
/** <module> behavior trees for SWI-Prolog

*/
:- use_module(library(dcg/basics)).

:- use_module(bt_impl).

tick(_Node) :-
	true.

behavior_tree -->
	statement, behavior_tree.
behavior_tree --> eos.

statement -->
	head,
	bt_operator(ArgType),
	args(ArgType),
	['.'].

head --> anatom.

anatom --> [X], {atom(X)}.

args(arg_list) --> arg.
args(arg_list) --> arg,
	[','],
	args(arg_list).
args(atom) --> anatom.
args(context_node_pair) -->
	integer(_Context),



arg -->	anatom.
arg --> decoration, arg.
arg --> ['{', head, bt_operator(ArgType), args(ArgType), '}'].


:-discontiguous bt_operator/3.

		 /*******************************
		 *   Selectors
		 *******************************/

%@	priority_selector
%
%	run tasks in order. If the first succeeds,
%	succeed.  if the first fails, run the second.
%	If it succeeds, succeed. If it fails, run the
%	third and so on. If the last fails, fail.
%
bt_operator(arg_list) --> [-?].

%@	probabiliy_selector
%
%	attached to each task is a weight.
%	select randomly according to the weighting
%       among the tasks and
%	run it, terminating when it terminates.
%	If any task has no probability, it is given weight 1.0
%
bt_operator(prob_arg_list) --> [~?].

%@	parallel
%
%	Start all tasks.
%	When one fails, terminate all others
%	and fail.
%	If all succeed, succeed
%
bt_operator(arg_list) --> [=>].

		 /*******************************
		 *  Sequencers
		 *******************************/

%@	sequence
%
%       At starting, start the leftmost task.
%       run the leftmost task to success, then
%       run the second task, and so on, and then succeed
%       If any task fails, fail
%
bt_operator(arg_list) --> [>>].

%@	scrambler
%
%	Start the tasks in L to R fashion,
%	as for sequence, but randomly
%	permute the order prior to doing so
%
bt_operator(arg_list) --> [~>>].

%@	parallel
%
%	Start all tasks. When any fail,
%	terminate the others and fail.
%	When the last succeeds, succeed.
bt_operator(arg_list) --> [==>].

%@	timer
%
%	the argument list is an alternating sequence
%	of times and tasks
%	If the list starts with a task,
%	Start the first task immediately.
%	Run until it succeeds, or the next listed time.
%	If it fails, fail.
%	If it is still running at the next listed time, interrupt
%	Then start the next task
%	If the list ends with a task, run this task to completion and
%	return it's completion status
%	If the list ends with a time and the final task is interruped,
%	return success
%
bt_operator(timed_arg_list) --> [**>].

		 /*******************************
		 *  Actions
		 *******************************/
bt_operator(event) --> [&*].    % emit event and succeed
bt_operator(atom) --> [&>>].  % broadcast message to prolog and succeed
bt_operator(atom) --> [&<<].  % runs until it gets this message from prolog
bt_operator(float) --> [&~]. % immediately succeed or fail, chance of success is the float
bt_operator(context_node_pair) --> [^].  % runs as long as a node in another context is running, then does what it does. fails if node not running.
bt_operator(atom) --> [^]. % as above, but for this context
bt_operator(float) --> [wait]. % wait for duration arg and succeed
bt_operator(float) --> [until]. % wait until clock time arg and succeed
bt_operator(null) --> [reset_clock]. % set the clock time to 0 and succeed
bt_operator(null) --> [true]. % succeed immediately
bt_operator(null) --> [fail]. % fail immediately

		 /*******************************
		 *  Decorations
		 *******************************/
decoration --> [true].  % run until argument terminates, then succeed
decoration --> [fail].  % run until argument terminates, then fail
decoration --> [while]. % repeatedly run argument until it fails
decoration --> [at], Time, [','], [run], {number(Time)}. % run 2nd arg at time specified by first arg
decoration --> [stop, at], number(_Time).   % interrupt task at clock time t
decoration --> [repeat, until], Time, [',']. % repeatedly run until clock time t or failure, then interrupt and succeed
decoration --> [start, until], Time, [',']. % repeatedly start until clock time t or it fails, let last iteration run to completion
decoration --> [clock].  % establish a new clock context
decoration --> event_on_start. % insert event on start
decoration --> event_on_success. % insert event on success
decoration --> event_on_failure. % insert event on failure
decoration --> set_context. % run the tasks below in new context. clock is inherited from caller.

		 /*******************************
		 * smaller stuff
		 *******************************/

arg_list -->
	arg_list_,
	".".


