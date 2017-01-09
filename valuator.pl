:- module(valuator, [valuator/0,
		     cycle_values/0,
		     setval/3,
		    broadcast_values/0,
		    getval/3,
		    lastval/3,
		    ezval/3]).

valuator :-
	broadcast_request(more),
	!,
	broadcast(propagate),
	!,
	valuator.
valuator.

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
	asserta(val(Context, Name, Val)).

getval(Context, Name, Val) :-
	val(Context, Name, Val).

lastval(Context, Name, Val) :-
	old_val(Context, Name, Val).
% lastval questionable design decision that this doesn't also use the
% current val if the previous one isn't avail


ezval(Context, Name, Val) :-
	val(Context, Name, Val).
ezval(_, _, '$not_avail$').