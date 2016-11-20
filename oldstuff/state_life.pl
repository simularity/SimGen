:- module(state_life, [its_a_life/2]).
/** <module> recursive version of life

*/

:- use_module(library(pita)).
:- use_module(library(random)).

%!	its_a_life(+Born:year, -History:list, -Present:list) is nondet
%
%	Generate a life.
%
%
its_a_life(Present) :-
	life_circumstances(Circumstances),
	live([age(-1) | Circumstances], Present).

live(State, State) :-
	member(dead, State).
live(State, Present) :-
	apply_transforms(State, NewState),
	advance_age(NewState, AAState),
	live(AAState, Present).

advance_age(InState, [age(NA)| OutState]) :-
	member(age(A), InState),
	subtract(InState, age(_), OutState),
	na(A, NA).

na(pre:1, 0:0).
na(pre:N, pre:NN) :-
	N > 1,
	NN is N - 1.
na(Y:M, Y:NM) :-
	M < 11,
	NM is M + 1.
na(Y:11, NY:0) :-
	NY is Y + 1.

apply_transforms(State, NewState) :-
	transforms(T),
	foldl(apply_transform, T, State, NewState).

transforms(T) :-
	bagof(transform(PC, F, A, R), transform(PC, F, A, R), T).

happening(0.01/mo, [age(pre:_)], [], [dead], [], ' miscarries').
happening(0.02, [age(pre:1)], [], [dead], [], ' dies in childbirth').
happening(0.01/mo, [age(pre:_)], [], [disability], [],
	  ' difficulty in pregnancy').




random_pick(_, _, [], _) :- !, fail.
random_pick(Axis, Conditions, Possible, Outcome) :-
	rp(1.0, Axis, Conditions, Possible, Outcome),
	outcome_event(Axis, Outcome, _).

rp(_Prob, _Axis, _Conditions, [Remaining], Remaining).
rp(Prob, Axis, Conditions, [Outcome, _More | _YetMore], Outcome) :-
	LPAD =.. [Axis, Conditions, Outcome],
	prob(LPAD, ProbThis),
	ConditionalProb is ProbThis / Prob,
	random(Roll),
	Roll < ConditionalProb,
	!.
rp(Prob, Axis, Conditions, [NotThis, More | YetMore], Outcome) :-
	LPAD =.. [Axis, Conditions, NotThis],
	prob(LPAD, ProbThis),
	RemProb is Prob - ProbThis,
	rp(RemProb, Axis, Conditions, [More | YetMore], Outcome).

life_circumstances([sex(Sex), parent_econ(Econ)]) :-
	random_pick(sex, [], [m,f], Sex),
	random_pick(parent_econ, [], [rich,middle,poor], Econ).

/*
prob(sex(_Conditions, m), 0.5).
prob(sex(_, f), 0.5).
prob(parent_econ(_, rich), 0.1).
prob(parent_econ(_, middle), 0.6).
prob(parent_econ(_, poor), 0.3).
*/

outcome_event(Axis, Value, object(Axis, Value)).

outcome_desc(sex, m, ' is male').
outcome_desc(sex, f, ' is female').
outcome_desc(parent_econ, rich, ' has wealthy parents').
outcome_desc(parent_econ, middle, ' is born in a middle class family').
outcome_desc(parent_econ, poor, ' is born to a poor family').

% live(YearBorn, , History, Present) :-


:- pita.

:- begin_lpad.

sex(m):1/2 ; sex(f) : 1/2.

n_heads(t, 1):1/2 ; n_heads(f, 1):1/2.
n_heads(t, N):1/2 * P ; n_heads(f, N):(1 - 1/2 * P) :-
	NN is N - 1,
	n_heads(t, NN).

:- end_lpad.

