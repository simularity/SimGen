:- module(behavior_tree, [bt/4]).
:- reexport(bt_impl).
/** <module> Behavior Trees
 *
 */
:- use_module(library(quasi_quotations)).
:- use_module(behavior_tree_parser).

:- quasi_quotation_syntax(behavior_tree:bt).

system:term_expansion(define_bt(X), X) :- write('term expands to:'),writeln(X).

bt(Content, _SyntaxArgs, _VariableNames, Result) :-
	debug(bt, 'bt in ~w', [Content]),
    phrase_from_quasi_quotation(bt_dcg(Result), Content),
    debug(bt, 'bt out ~w', [Result]).

