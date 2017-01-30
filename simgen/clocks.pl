:- module(clocks, [abolish_clocks/1,
		   new_clock/2,
		  clock_units/2,
		  update_clocks/0,
		  get_clock/2]).

:- use_module(simgen(bt_impl), [bad_thing_happened/0]).
:- use_module(simgen(print_system)).

:- dynamic clock/2, time_unit/1, tick_length/1.

abolish_clocks(Name) :-
	retractall(clock(Name, _)).

new_clock(Name, _Start) :-
	clock(Name, _),
	!,
	bt_debug(bt(clock, duplicate_clock), 'Error, attempt to create duplicate clock ~w', [Name]),
	bad_thing_happened.
new_clock(Name, Start) :-
	bt_debug(bt(clock, new_clock), 'created clock ~w ~w', [Name, Start]),
	asserta(clock(Name, Start)).

/** <module> Clock related bt stuff
 *
 */
clock_units(TimeUnit, TickLength) :-
	retractall(time_unit(_)),
	retractall(tick_length(_)),
	asserta(time_unit(TimeUnit)),
	asserta(tick_length(TickLength)).

update_clocks :-
	bagof(Name, Val^clock(Name, Val), Clocks),
	maplist(update_clock , Clocks).

update_clock(Name) :-
	tick_length(Len),
	clock(Name, Time),
	retractall(clock(Name, _)),
	NewTime is Len + Time,
	asserta(clock(Name, NewTime)).


get_clock(Name, Time) :-
	clock(Name, Time).
