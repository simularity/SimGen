:- module(behavior_tree_syntax, [bt_dcg//1]).
/** <module> Concrete syntax parser for behavior trees

*/
:- multifile license:license/3.

license:license(simularity, proprietary,
                [ comment('this program is the confidential property of Simularity, Inc. and must not be disclosed to any person without written permission of Simularity, Inc. All rights reserved.'),
                  url('http://thunor.simularity.com/dokuwiki/doku.php?id=components:license')
                ]).

:- license(simularity).

:- use_module(library(dcg/basics)).
:- use_module(simgen(print_system)).

d(Format, Args) --> {bt_debug(bt(parser, info), Format, Args)}, [].

bt_dcg(define_bt([':-'(true)])) --> eos.
bt_dcg(define_bt([':-'(set_current_bt_module) | BT])) -->
	d('in bt_dcg', []),
	bt_(BT).

bt_([BTStatement | BT]) -->
    d('in bt_', []),
    ws,
    bt_statement(BTStatement),
    d('got statement', [BTStatement]),
    ws,
    bt_(BT).
bt_([]) --> ws, eos,!.
bt_([BT]) -->
	d('in bt_ error', []),
    ws,
    bt_error,
    d('got error', []),
    ws,
    bt_(BT).

bt_error -->
	string(OopsCodes),
	(   "." ; eos),
	{ atom_codes(AC, OopsCodes),
	  line_count(current_input, Line),
	  line_position(current_input, Pos),
	  source_file(File),
	  bt_print_message(error, error(syntax_error(AC),
				     context(err(AC:Line:Pos:File))))
	}.

ws --> blanks.
ws --> blanks, comment, ws.
% allow prolog style comments
comment -->
	"%",
	string_without("\n", Body),
	"\n",
	d('{{{%~s}}}~n', [Body]).

% allow C style comments
comment -->
	"/*",
	c_body(Body),
	d('{{{/*~s*/}}}~n', [Body]).

c_body([C|T]) -->
	[C],
	{ C \= 0'* },
	c_body(T).
c_body([]) -->
	"*/".
c_body([ 0'*, C | T]) -->
	"*",
	[C],
	{ C \= 0'/ },
	c_body(T).

... --> [].
... --> [_], ... .

...([]) --> [].
...([X|T]) --> [X], ...(T) .

bt_statement(':-'(def_node(Head, Operator, Args, Children))) -->
	ws,
	d('in bt_statement', []),
	head(Head),
	d('got head ~w', [Head]),
	ws,
	bt_operator(Operator),
	d('got operator ~w', [Operator]),
	ws,
	bt_args(Operator, Args, Children),
	ws,
	".".

head(Head) --> an_atom(Head).

bt_operator( ~? ) --> "~?".     % random selector
bt_operator( '!' ) --> "!".     % pdq
bt_operator( '?' ) --> "?".     % check condition
bt_operator( '-?' ) --> "-?".   % wait condition
bt_operator( set ) --> "set".   % set condition
bt_operator( clear ) --> "clear". % clear condition
bt_operator( '->' ) --> "->". % sequence
bt_operator( '~>' ) --> "~>". % random sequence
bt_operator( '=>' ) --> "=>". % parallel sequence
bt_operator( '=?' ) --> "=?". % parallel selector
bt_operator( try ) --> "try". % try (always succeed)
bt_operator( fail ) --> "fail". % fail (always fail)
bt_operator( not ) --> "not". % not (fail if done, done if fail)
bt_operator( dur ) --> "dur". % duration
bt_operator( pin ) --> "pin". % emit pin event
bt_operator( repeat(fail) ) --> "<>". % loop until failure
bt_operator( repeat(done) ) --> "<-->". % loop until success

bt_args( repeat(_), [], [Child]) --> a_child(Child).
bt_args( pin, [], [Child]) --> a_child(Child).
bt_args( dur, [Dur], []) -->
	ws, number(Dur).
bt_args( try, [], [Child]) --> a_child(Child).
bt_args( fail, [], [Child]) --> a_child(Child).
bt_args( not, [], [Child]) --> a_child(Child).
bt_args( '->', [], Children) --> nonempty_child_list(Children).
bt_args( '~>', [], Children) --> nonempty_child_list(Children).
bt_args( '=>', [], Children) --> nonempty_child_list(Children).
bt_args( '=?', [], Children) --> nonempty_child_list(Children).
bt_args( '?' , [Cond], []) -->  an_atom(Cond).
bt_args( '-?' , [Cond], []) -->  an_atom(Cond).
bt_args( set , [Cond], []) -->  an_atom(Cond).
bt_args( clear , [Cond], []) -->  an_atom(Cond).
bt_args( ~? , Args, Children) --> prob_list(Args, Children).
bt_args( '!', [FirstTicks, RestTicks, Conds], []) -->
	d('in bt_args', []),
	continuous_calc_list(FirstTicks),
	d('Firstticks ~w', [FirstTicks]),
	ws,
	";",
	ws,
	d('pre RestTicks', []),
	continuous_calc_list(RestTicks),
	d('got RestTicks ~w', [RestTicks]),
	ws,
	";",
	ws,
	d('pre conds', []),
	conds(Conds),
	d('conds ~w', [Conds]).

prob_list([Prob | Probs], [Child | Children]) -->
	prob(Prob, Child),
	ws,
	",",
	ws,
	prob_list(Probs, Children).
prob_list([Prob], [Child]) -->
	prob(Prob, Child).

prob(Prob, Child) -->
	ws,
	number(Prob),
	ws,
	":",
	ws,
	an_atom(Child).
prob(1.0, Child) -->
	ws,
	an_atom(Child),
	ws.

continuous_calc_list([Stmt | Rest]) -->
	statement(Stmt),
	ws,
	",",
	ws,
	continuous_calc_list(Rest).
continuous_calc_list([Stmt]) -->
	statement(Stmt).
continuous_calc_list([]) -->  ws.

statement(':='(LVal, RVal)) -->
	an_atom(LVal),
	ws,
	":=",
	ws,
	rval(RVal).

statement('='(LVal, RVal)) -->
	an_atom(LVal),
	ws,
	"=",
	ws,
	rval(RVal).

conds([Cond | Rest]) -->
	cond(Cond),
	ws,
	",",
	ws,
	conds(Rest).
conds([Cond]) -->
	cond(Cond).
conds([]) --> ws.

cond(Cond) -->
	ws,
	rval(Left),
	ws,
	cond_op(Op),
	ws,
	rval(Right),
	{ Cond =.. [Op, Left, Right] }.

cond_op( = ) --> "=".
cond_op( = ) --> "==".
cond_op( < ) --> "<".
cond_op( > ) --> ">".
cond_op( =< ) --> "<=".
cond_op( =< ) --> "=<".
cond_op( >= ) --> ">=".
cond_op( >= ) --> "=>".
cond_op( \= ) --> "!=".
cond_op( \= ) --> "\\=".

bt_op(20, '+' ) --> "+".
bt_op(20, '-' ) --> "-".
bt_op(10, '*' ) --> "*".
bt_op(10, '/' ) --> "/".

rval(Rval) -->
	term(Term),
	rest_of_rval(Term, Rval).

rest_of_rval(In, Out) -->
	ws,
	bt_op(20, Op),
	ws,
	term(R),
	{ New =.. [Op, In, R] },
	rest_of_rval(New, Out).
rest_of_rval(X, X) --> [].

term(Term) -->
	cofactor(CoFactor),
	rest_of_term(CoFactor, Term).

rest_of_term(In, Out) -->
	ws,
	bt_op(10, Op),
	ws,
	cofactor(CoFactor),
	{ Term =.. [Op, In, CoFactor] },
	rest_of_term(Term, Out).
rest_of_term(X, X) --> [].

cofactor('-'(CF)) -->
	ws,
	"-",
	ws,
	cofactor(CF).
cofactor(Lval) -->
	ws,
	"(",
	ws,
	rval(Lval),
	ws,
	")".
cofactor(eval(Term)) -->
	ws,
	an_atom(Functor),
	{ function(Functor, _) },
	ws,
	 "(",
	 ws,
	 rval_args(Args),
	 { function(Functor, Arity),
	   length(Args, Arity),
	   Term =.. [Functor | Args] },
	 ws,
	 ")".
% treat empty arg list as special case to save sanity
cofactor(eval(Functor)) -->
	ws,
	an_atom(Functor),
	{ function(Functor, _) },
	ws,
	 "(",
	 ws,
	 ")",
	 { function(Functor, 0) }.

cofactor(var(X)) -->
	ws,
	an_atom(X).
cofactor(const(X)) -->
	number(X).

rval_args([Arg | Args]) -->
	ws,
	rval(Arg),
	ws,
	",",
	ws,
	rval_args_rest(Args).
rval_args([Arg]) --> ws, rval(Arg).
rval_args([]) --> [].

rval_args_rest([Arg | Args]) -->
	ws,
	rval(Arg),
	ws,
	",",
	ws,
	rval_args_rest(Args).
rval_args_rest([Arg]) --> rval(Arg).

function( levy_flight, 3).
function( wander, 4).
function( clock, 0).

an_atom(Atom) -->
	[X],
	{ code_type(X, lower) },
	atom_codes(Codes),
	{ atom_codes(Atom, [X | Codes]) }.

atom_codes([X | Rest]) -->
	[X],
	{ code_type(X, csym) },
	atom_codes(Rest).
atom_codes([]) --> [].


nonempty_child_list([Child | MoreChildren]) -->
	ws,
	an_atom(Child),
	ws,
	",",
	nonempty_child_list(MoreChildren).
nonempty_child_list([Child]) --> ws, an_atom(Child).


a_child(Child) -->
	an_atom(Child).
a_child(anon_node(Head, Operator, Args, Children)) -->
	{ gensym(node, Head) },
	"{",
	ws,
	bt_operator(Operator),
	ws,
	bt_args(Operator, Args, Children),
	ws,
	"}".

