:- module(bt_example, [test/2]).
/** <module> Example that runs the test simulation


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

%!	test(+File:atom, +N:integer) is nondet
%
%	run a bt file, making N contexts
%	with root root. File is the file
%
%	The file name is like consult, you don't need the .bt
%	extension
%
test(File, N) :-
	use_bt(File),
	setup_call_cleanup(
	    open('tests.simai', write, Stream),
	    (	b_setval(test_stream, Stream),
		b_setval(test_root, root),
		!, % sanity measure,
		start_simulation(
		    0,                 % the start time, in 'our' units
		    1_000_000_000,    % how long our units are in nanos
		    1,                 % how long a tick is in our units
		    N)   % our extern is just how many contexts to make
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

consider_adding_context(Extern, 0, Extern) :-
	add_contexts(Extern).
consider_adding_context(Extern, 3600, Extern) :-    % # of ticks
	!,
	end_simulation.
consider_adding_context(Extern, _, Extern).   % # of ticks

add_contexts(0).
add_contexts(N) :-
	start_context(root, N, 0),
	succ(NN, N),
	add_contexts(NN).

% Now we handle reading, starting, and stopped events
% by writing lines to the csv file

% listen for reading/5 events and write reading events to csv
:- listen(reading(Time, _, Context, Type, Value),
	  write_event(reading, Time, Context, Type, Value)).

write_event(Class, Time, Context, Type, Value) :-
	Nanos is Time * 60_000_000_000,
	b_getval(test_stream, Stream),
	format(Stream, 'unit,~d,~d,~w,~w,~w~n',
	       [Context, Nanos, Class, Type, Value]),
	flush_output(Stream).

