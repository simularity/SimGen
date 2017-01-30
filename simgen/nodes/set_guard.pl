:- module(set_guard, []).
/** <module> Check a guard. Fail when it's reset
 *
 */
:- use_module(simgen(bt_impl), [emit/1]).
:- use_module(simgen(guard_manager)).

:- multifile bt_impl:make_cn_impl/3.

bt_impl:make_cn_impl( set , C-N, _) :-
	bt_impl:node_(_, N, set, [GuardName], _),
	set_guard(C, GuardName),
	emit(starting(C-N)),
	emit(stopped(C-N, done)).


