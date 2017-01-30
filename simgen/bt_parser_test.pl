:- module(bt_parser_test, [go/0]).
/** <module> test suite for behavior_tree_parser
 *
 */

:- use_module(simgen(behavior_tree_parser)).

go :-
	content(Content),
	phrase(bt_dcg(define_bt(Answer)), Content),
	!,
	show_clauses(Answer).

show_clauses([]).
show_clauses([H|T]) :-
	portray_clause(H),
	nl,
	show_clauses(T).

content(
`wawei ~?
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
 p := levy_flight(p, 0 200),
 t := wander(t, 0, 200, 3),
 speed = 500 + clock() - p - t.`).
