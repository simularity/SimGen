:- module(ast_transform,
	  [
	collect_anons/2
	  ]).
/** <module> Transform the ast
 *
 * code to simplify the ast
 */

%!	collect_anons(+List:list, -Anons:list) is det
%
%	extract the anonymous
collect_anons([], []).
collect_anons([':-'(def_node(H,O,A,C)) | T], [':-'(def_node(H,O,A,NC)) | XT]) :-
	anons_from_children(C, NC, Anons),
	append(Anons, T, NT), % note we reprocess new def_nodes
	collect_anons(NT, XT).
collect_anons([H | T], [H | NT]) :-
	H \= ':-'(def_node(_,_,_,_)),
	collect_anons(T, NT).

anons_from_children([], [], []).
anons_from_children([H | T], [H |NT], Anons) :-
	H \= anon_node(_,_,_,_),
	anons_from_children(T, NT, Anons).
anons_from_children([anon_node(H,O,A,C) | T],
		    [H | NT],
		    [':-'(def_node(H,O,A,C)) | Anons]) :-
	anons_from_children(T, NT, Anons).

