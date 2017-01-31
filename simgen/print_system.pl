:- module(print_system, [
	      bt_print_message/2,
	      bt_print_message/1,
	      bt_debug/1,
	      bt_nodebug/1,
	      bt_debug/3
	  ]).
/** <module> Utilities for assisting printing
 *
 */

bt_debug(Sig) :-
	debug(Sig).

bt_nodebug(Sig) :-
	nodebug(Sig).

bt_print_message(Message) :-
	member(Message,
	       [bt_nonfatal_error, bt_error]),
	bt_print_message(error, Message).
bt_print_message(Type, Message) :-
	print_message(Type, Message),
	(   Type == error
	->  gtrace
	;   true
	).

bt_debug(Sig, Format, Args) :-
	Sig = error(_, _),
	print_message(error, bt_error(Sig, Format, Args)),
	debug(Sig, Format, Args).
bt_debug(Sig, Format, Args) :-
	Sig \= error(_, _),
	debug(Sig, Format, Args).

:- multifile
	system:goal_expansion/2.

system:goal_expansion(bt_debug(Topic,_,_), true) :-
	(   prolog_debug:optimise_debug
	->  true
	;   prolog_debug:debug_topic(Topic),
	    fail
	).


:- multifile prolog:message//1.

prolog:message(bt_error(Sig, Format, Args)) -->
	['BT Error: ', '~q'-[Sig], ', ', Format-Args, nl].
prolog:message(bt_nonfatal_error(node_error(no_child_to_run), culprit(_C-N))) -->
	['BT Error: ', 'Node ~w has no child to run'-[N], nl].
% bt_nonfatal_error(node_error(no_child_to_run), culprit(C-N))
prolog:message(error(syntax_error(AC), context(err(AC:Line:Pos:File)))) -->
	['BT File Error: ',
	 'Syntax error on line ~w:~w of ~w'-[Line,Pos,File],
	 nl,
	 AC,
	 nl].


% TODO consider using term expansion on this at some point to remove the
% bt_ versions
