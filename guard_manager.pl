:- module(guard_manager, [
	      guard/2,
	      set_guard/2,
	      clear_guard/2,
	      reset_guards/0]).
/** <module> Support for guards
 *
 * guards are persistent conditions, checked each tick
 *
*/

:- dynamic guard_/2.

guard(C, X) :- guard_(C, X).

set_guard(C, X) :- asserta(guard_(C, X)).

clear_guard(C, X) :- retractall(guard_(C, X)).

reset_guards :- retractall(guard_(_, _)).


:-listen(simulation_starting, reset).

reset :-
	reset_guards.

