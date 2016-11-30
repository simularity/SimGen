:- module(behavior_tree, [bt/4]).
:- reexport(bt_impl).
/** <module> Behavior Trees
 *
 */

:- use_module(library(quasi_quotations)).
:- use_module(behavior_tree_parser).

:- quasi_quotation_syntax(behavior_tree:bt).

% for debugging
system:term_expansion(define_bt(X), X) :- write('term expands to:'),writeln(X).

%!	bt(Content, SyntaxArgs, VariableNames, Result) is det
%
%	Quasiquoter for the bt language
%
bt(Content, _SyntaxArgs, _VariableNames, Result) :-
	debug(bt, 'bt in ~w', [Content]),
    phrase_from_quasi_quotation(bt_dcg(Result), Content),
    debug(bt, 'bt out ~w', [Result]).

user:message_hook(load_file(Data), _, _) :-
	Data = start(_, file(_Path, FullPath)),
	module_property(Module, file(FullPath)),
	reset_nodes_for_module(Module),
	debug(bt, 'reset ~w nodes', [Module]).

system:term_expansion(end_of_file, [:-(bt_impl:check_nodes), end_of_file]).

