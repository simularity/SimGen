:- module(clear_guard, []).
/** <module> Check a guard. Fail when it's reset
 *
 */
:- use_module(bt_impl, [emit/1]).
:- use_module(simgen(guard_manager)).

:- multifile bt_impl:make_cn_impl/3.

bt_impl:make_cn_impl( clear , C-N, _) :-
	bt_impl:node_(_, N, clear, [GuardName], _),
	clear_guard(C, GuardName),
	emit(starting(C-N)),
	emit(stopped(C-N, done)).


