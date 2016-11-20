:- module(bt_example, [wawei/1]).
/** <module> Examples that define some test behaviors
 *
 * wawei
 *
 * Each context is one of two types.
 *
 * Type a speed is 500 - P - T
 * Type b speed is 500 + time - P - T
 *
 * Type b fails after a random time
 *
 * generator
 *
 * This example simulates in-service failure control of a gas powered
 * generator set.
 *
 * A manufacturer of gas powered generators runs the devices for
 * a 24 hour period to eliminate those that will fail within the first
 * 24 hours of use,
 *
 * Doing this is expensive for the manufacturer. They'd like to identify
 * failures before they happen to save gasoline, reduce
 * in-house wear on units, and be able to rework the bad generators
 * instead of scrapping them.
 */
:- use_module(behavior_tree).

:- writeln('after use_module').

{|bt||
wawei ~?
 type_a,
 type_b.

type_a !
 p = 100,
 t = 100;
 p := f_corr(p, 0, 200),
 t := wander(t, 0, 200, 3),
 speed = 500 - p - t.

type_b !
 p = 100,
 t = 100;
 p := f_corr(p, 0, 200),
 t := wander(t, 0, 200, 3),
 speed = 500 + clock() - p - t.
|}.

:- writeln('after quasiquote').

wawei(N) :-
	start_simulation(0, 60_000_000_000, 60_000_000_000),
	wawei_contexts(N).

wawei_contexts(0).
wawei_contexts(N) :-
	N > 0,
	NN is N - 1,
	start_context(wawei, N, 0),
	random_between(0, 100, T),
%	do_n_ticks(T),
	wawei_contexts(NN).


