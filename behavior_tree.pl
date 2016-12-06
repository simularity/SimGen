:- module(behavior_tree, [bt/4]).
:- reexport(bt_impl).
/** <module> Behavior Trees
 *
 */

:- use_module(library(quasi_quotations)).
:- use_module(behavior_tree_parser).

:- quasi_quotation_syntax(behavior_tree:bt).

% for debugging this writes as well, but we have to strip the define_bt
system:term_expansion(define_bt(X), X) :- write('term expands to:'),portray_clause(X),nl.

%!	bt(Content, SyntaxArgs, VariableNames, Result) is det
%
%	Quasiquoter for the bt language
%
bt(Content, SyntaxArgs, VariableNames, Result) :-
    bt_(Content, SyntaxArgs, VariableNames, Result),
    writeln(result_gonna_be(Result)),
    !.

bt_(Content, _SyntaxArgs, _VariableNames, Result) :-
	debug(bt, 'bt in ~w', [Content]),
    catch(
       phrase_from_quasi_quotation(bt_dcg(Result), Content),
	Catcher,
	(   print_message(error, error(bt_syntax_error(parser_failed, Catcher))),
	    Result= define_bt([':-'(true)])
	)
    ),
    write('back from parsing'),portray_clause(Result),
    % if we have errors we may have vars in Result
    % this locks up the quasiquoter, and hence swipl
    ground(Result),  % otherwise quasiquoter locks up.
    debug(bt, 'bt out ~w', [Result]).
bt_(_, _, _, define_bt([':-'(true)])).  % make sure we never fail

user:message_hook(load_file(Data), _, _) :-
	Data = start(_, file(_Path, FullPath)),
	module_property(Module, file(FullPath)),
	reset_nodes_for_module(Module),
	debug(bt, 'reset ~w nodes', [Module]).

system:term_expansion(end_of_file, [:-(bt_impl:check_nodes), end_of_file]).


