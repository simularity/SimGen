:- module(valuator, [valuator/0,
		     cycle_values/0,
		     setval/3,
		    broadcast_values/0,
		    getval/3,
		    lastval/3,
		    ezval/3]).

valuator :-
	valuator(20).

valuator(0) :-
	debug(error(valuator, circular), '***** Valuator cannot resolve after 20 cycles', []),
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
	debug(error(valuator, multiple_sources),
	      'In context ~w value ~w has multiple sources',
	      [Context, Name]).
setval(Context, Name, Val) :-
	debug(bt(valuator, setval), '~w: ~w <- ~w', [Context, Name, Val]),
	asserta(val(Context, Name, Val)).

getval(Context, Name, Val) :-
	val(Context, Name, Val),
	debug(bt(valuator, getval), '~w: ~w returns ~w', [Context, Name, Val]).


lastval(Context, Name, Val) :-
	old_val(Context, Name, Val),
	debug(bt(valuator, lastval),
	      '~w: ~w returns lastval ~w', [Context, Name, Val]).
% lastval questionable design decision that this doesn't also use the
% current val if the previous one isn't avail


ezval(Context, Name, Val) :-
	val(Context, Name, Val),
	debug(bt(valuator, lastval),
	      '~w: ~w returns ezval ~w', [Context, Name, Val]).
ezval(Context, Name, '$not_avail$') :-
	debug(bt(valuator, lastval),
	      '~w: ~w returns ezval ~w', [Context, Name, '$not_avail$']).


:-listen(simulation_starting, reset).

reset :-
	retractall(val(_, _, _)),
	retractall(old_val(_, _, _)).
