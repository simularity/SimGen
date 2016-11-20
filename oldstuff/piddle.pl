/* background program, defines a quasiquoter for piddle */

:- use_module(library(quasi_quotations)).
:- use_module(library(dcg/basics)).

:- quasi_quotation_syntax(piddle).

piddle(Content, _SyntaxArgs, _VariableNames, Result) :-
    phrase_from_quasi_quotation(piddle_dcg(Result), Content).

piddle_dcg(true) --> eos.
piddle_dcg(define_piddle(Piddle)) --> piddle_(Piddle).

piddle_([]) --> eos.
piddle_([PiddleStatement | Piddle]) -->
    ws,
    piddle_statement(PiddleStatement),
    ws,
    piddle_(Piddle),
    ws.

% define the exclusive, complete list
% if any of the items are added, the others
% in the set are removed.
% If none occur, the first is added
%
piddle_statement(ec(TL)) -->
    ws,
    (   "exclusive" ; "ex" ),
    ws,
    (   "complete" ; "c" ),
    ws,
    term_list(TL),
    !.

% define the exclusive list
% if any of the items are added, the others
% in the set are removed
%
piddle_statement(en(TL)) -->
    ws,
    (   "exclusive" ; "ex" ),
    ws,
    term_list(TL),
    !.

term_list([Term | T]) -->
    term(Term),
    ws,
    ",",
    ws,
    term_list(T).
term_list([T]) -->
    term(T),
    ws,
    ";".

ws --> blanks.


term(T) -->
    [X],
    {  code_type(X, alpha) },
    rest_term([X], T).

rest_term(RevCodes, T) -->
    [X],
    {  code_type(X, alpha); X == 0'_ ; code_type(X, digit) },
    rest_term([X | RevCodes], T).
rest_term(RevCodes, T) -->
    [],
    { reverse(RevCodes, Codes),
      atom_codes(T, Codes)
    }.

test_piddle :-
	phrase(piddle_dcg(X), `
    ex c foo, bar;
    `),
     writeq(X),nl.
