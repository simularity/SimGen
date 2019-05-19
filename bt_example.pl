:- module(bt_example, [test/2, test/3]).
/** <module> Example that runs the test simulation

?- test(root, 'examples/hwclient', 3).

produces file tests.simai

 */

path_target.
% This is required, needed for internal glue.
% fix it so it points at your simgen directory
%
:-
	source_file(bt_example:path_target, Path),
	directory_file_path(Dir, _, Path),
	absolute_file_name(Dir, AbsPath),
	atomic_list_concat(
	    [
	    AbsPath,
	    '/simgen'], SGP),
	asserta(user:file_search_path(simgen, SGP)),
	atomic_list_concat(
	    [
	    AbsPath,
	    '/examples'], SGPE),
	asserta(user:file_search_path(examples, SGPE)).

% Grab SimGen
:- use_module(simgen(simgen)).

%!	test(+Root:atom, +N:integer) is nondet
%
%	run the test.bt test file, making N-1 contexts
%	with root Root
%
test(Root, N) :-
	test(Root, tests, N).

%!	test(+Root:atom, +FileBase:atom, +N:integer) is nondet
%
%	run a bt file, making N-1 contexts
%	with root Root
%
%	The file name is like consult, you don't need the .bt
%	extension
%
test(Root, File, N) :-
	use_bt(File),
	setup_call_cleanup(
	    open('tests.simai', write, Stream),
	    (	b_setval(test_stream, Stream),
		b_setval(test_root, Root),
		!, % sanity measure,
		start_simulation(
		    0,                 % the start time, in 'our' units
		    60_000_000_000,    % how long our units are in nanos
		    1,                 % how long a tick is in our units
		    extern{
			next_context: N,
			add_context_on_tick: 0
		    })
	    ),
	    close(Stream)
	).

% we register to listen for ticks
% and add a new context randomly every 1 to 20 ticks
% we number them going down
% when we are asked to make context 0, we instead
% end the simulation
:- listen(tick(Extern, Tick, NewExtern),
	  consider_adding_context(Extern, Tick, NewExtern)).

consider_adding_context(Extern, Tick, Extern) :-
	Extern.add_context_on_tick > Tick,
	!.
consider_adding_context(Extern, _, Extern) :-
	Extern.next_context = 0,
	!,
	end_simulation.

consider_adding_context(Extern, Tick, NewExtern) :-
	b_getval(test_root, Root),
	Extern.add_context_on_tick =< Tick,
	succ(NN, Extern.next_context),
%	random_between(1, 20, R),
R = 300,
	NewTick is R + Tick,
	NewExtern  = extern{
			 next_context: NN,
			 add_context_on_tick: NewTick
		     },
	start_context(Root, Extern.next_context, 0).

% Now we handle reading, starting, and stopped events
% by writing lines to the csv file

% listen for reading/5 events and write reading events to csv
:- listen(reading(Time, _, Context, Type, Value),
	  write_event(reading, Time, Context, Type, Value)).

% write text events to csv in response to starting
:- listen(starting(Context-Type),
	  (   get_clock(simgen, Time),
	      write_event(text, Time, Context, Type, start)
	  )
	 ).

% We only listen to done and fail stopping. When a node stops
% it terminates all nodes under it. We aren't usually interested in
% those.
:- listen(stopped(Context-Type, done),
	  (   get_clock(simgen, Time),
	      (	  ground(Type) ; gtrace),   % DEBUG
	      write_event(text, Time, Context, Type, success)
	  )
	 ).

:- listen(stopped(Context-Type, fail),
	  (   get_clock(simgen, Time),
	      write_event(text, Time, Context, Type, fail)
	  )
	 ).

:- listen(pin_drop(Context, Time, event),
       write_event(text, Time, Context, pin_drop, event)).
:- listen(pin_drop(Context, Time, -event),
       write_event(text, Time, Context, pin_drop, '-event')).

write_event(Class, Time, Context, Type, Value) :-
	Nanos is Time * 60_000_000_000,
	b_getval(test_stream, Stream),
	format(Stream, 'unit,~d,~d,~w,~w,~w~n',
	       [Context, Nanos, Class, Type, Value]),
	flush_output(Stream).

