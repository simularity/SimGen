:- module(valuator, [valuator/0,
		     cycle_values/0,
		     setval/3,
		    broadcast_values/0,
		    getval/3,
		    lastval/3,
		    ezval/3]).

:- use_module(simgen(print_system)).

valuator :-
	debug_vals('pre valuator'),
	valuator(20),
	debug_vals('post valuator').
valuator :-
	bt_debug(error(valuator, fails), '***** Valuator fails',[]),
	debug_vals('failure'),
	gtrace,
	bt_impl:end_simulation.

valuator(0) :-
	bt_debug(error(valuator, circular), '***** Valuator cannot resolve after 20 cycles', []),
	bt_impl:end_simulation.
valuator(N) :-
	N > 0,
	broadcast_request(more),
	!,
	broadcast(propagate),
	!,
	NN is N - 1,
	valuator(NN).
valuator(N) :-
	N > 0.

:- dynamic val/3, old_val/3.

debug_vals(Msg) :-
	findall(C-Name-Val, old_val(C, Name, Val), Old),
	findall(CN-NameN-ValN, val(CN, NameN, ValN), New),
	bt_debug(bt(valuator, debug_vals),
		 '#### ~w~nold_val: ~q~nval: ~q', [Msg, Old, New]).
cycle_values :-
	findall(asserta(old_val(C, N, V)), val(C,N,V), List),
	retractall(val(_, _, _)),
	retractall(old_val(_, _, _)),
	maplist(call , List).

:- use_module(clocks, [get_clock/2]).

broadcast_values :-
	get_clock(simgen, Time),
	forall(
	   val(Context, Name, Val),
	    (   get_clock(Context, ContextTime),
		broadcast(reading(Time, ContextTime, Context, Name, Val))
	    )
	).

% note that multiple sources error succeeds.
setval(Context, Name, _) :-
	val(Context, Name, _),
	!,
	bt_debug(error(valuator, multiple_sources),
	      'In context ~w value ~w has multiple sources',
	      [Context, Name]).
setval(Context, Name, Val) :-
	bt_debug(bt(valuator, setval), '~w: ~w <- ~w', [Context, Name, Val]),
	asserta(val(Context, Name, Val)).

getval(Context, Name, Val) :-
	val(Context, Name, Val),
	bt_debug(bt(valuator, getval), '~w: ~w returns ~w', [Context, Name, Val]).


lastval(Context, Name, Val) :-
	old_val(Context, Name, Val),
	bt_debug(bt(valuator, lastval),
	      '~w: ~w returns lastval ~w', [Context, Name, Val]).
lastval(Context, Name, 0) :-
	bt_debug(error(valuator, no_lastval),
		 '**** Context ~w variable ~w has no last value so cannot be evaluated', [Context, Name]),
	bt_impl:bad_thing_happened.
% lastval questionable design decision that this doesn't also use the
% current val if the previous one isn't avail


ezval(Context, Name, Val) :-
	val(Context, Name, Val),
	bt_debug(bt(valuator, lastval),
	      '~w: ~w returns ezval ~w', [Context, Name, Val]).
ezval(Context, Name, '$not_avail$') :-
	bt_debug(bt(valuator, lastval),
	      '~w: ~w returns ezval ~w', [Context, Name, '$not_avail$']).


:-listen(simulation_starting, reset).

reset :-
	retractall(val(_, _, _)),
	retractall(old_val(_, _, _)).
