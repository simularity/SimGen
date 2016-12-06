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
 p := levy_flight(p, 0, 200),
 t := wander(t, 0, 200, 3),
 speed = 500 - p - t.

type_b !
 p = 100,
 t = 100;
 p := levy_flight(p, 0, 200),
 t := wander(t, 0, 200, 3),
 speed = 500 + clock() - p - t.
|}.


:- writeln('after quasiquote').

wawei(N) :-
	start_simulation(0, 60_000_000_000, 60_000_000_000,
	   extern{
		  next_context: N,
		  add_context_on_tick: 0
	   }).

:- listen(tick(Extern, Tick, NewExtern), consider_adding_context(Extern, Tick, NewExtern)).

consider_adding_context(Extern, Tick, Extern) :-
	Extern.add_context_on_tick > Tick,
	!.
consider_adding_context(Extern, _, _) :-
	Extern.next_context = 0,
	!,
	end_simulation.
consider_adding_context(Extern, Tick, NewExtern) :-
	Extern.add_context_on_tick =< Tick,
	succ(NN, Extern.next_context),
	random_between(1, 20, R),
	NewTick is R + Tick,
	NewExtern  = extern{
			 next_context: NN,
			 add_context_on_tick: NewTick
		     },
	start_context(wawei, Extern.next_context, 0).

