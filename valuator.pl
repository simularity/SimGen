:- module(valuator, [valuator/0]).

valuator :-
	broadcast_request(more),
	!,
	broadcast(propagate),
	!,
	valuator.
valuator.


