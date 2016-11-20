:- module(life, [life_transition/6]).

:- include(piddle).

:- dynamic life:lt/6, imply/4.

:- retractall(life:imply(_, _ , _, _)),
   retractall(life:lt(_, _, _, _, _, _)).

define_piddle(PIDDLE) :-
	debug(dp, 'piddle out:~w', [PIDDLE]),
	maplist(dp , PIDDLE).

dp(ec(List)) :-
	maplist(def_exclusion(List), List),
	List = [H | _],
	% enforce having one selected
	asserta(life:lt(1, [], List, [H], [], _)).

dp(en(List)) :-
	maplist(def_exclusion(List), List).

def_exclusion(List, Item) :-
	debug(dp, 'def_exclusion(~w, ~w)', [List, Item]),
	subtract(List, [Item], Remove),
	asserta(life:imply([Item], [], [], Remove)).

life_transition(Prob, Require, Forbid, Add, Remove, Desc) :-
	lt(Prob, Require, Forbid, Add, Remove, Desc).

%!	lt(-Probability:prob, -Requires:list, -Forbids:list,
%!	   -Add:list, -Remove:list, -Desc:atom) is nondet
%
%	life transitions. Things that can happen to a person during
%	their life

:-call({|piddle||
    ex c foo, bar;
    ex baz, mep;
    |}).

		 /*******************************
		 *  Congenital Conditions       *
		 *******************************/

% can you find a way to make the next two lines symmetric?
lt(0.5, [age(0,0)], [female], [male], [], ' is male').
lt(1, [age(0,0)], [male], [female], [], ' is female').
lt(1/15,[age(0,0)], [rich_parents, orphan], [poor_parents], [], ' has poor parents').
lt(1/100, [age(0,0)], [poor_parents, orphan], [rich_parents], [], ' has rich parents').
lt(1/500, [age(0,0)], [rich_parents, poor_parents], [orphan], [], ' is an orphan').
lt(1/100, [age(0,0)], [], [ambitious], [], ' is ambitious').
lt(1/100, [age(0,0)], [], [learning_disabled], [], ' is learning disabled').
lt(0.1, [age(0,0)], [], [gay], [], ' is gay').

		 /*******************************
		 * parenting                    *
		 *******************************/

lt(1/6, [age(0,3), orphan], [], [rich_parents], [orphan], ' is adopted').
lt(1/15, [age(4,9), orphan], [gay, learning_disabled], [rich_parents], [orphan], ' is adopted').
lt(1/45, [age(4,9), orphan], [], [rich_parents], [orphan], ' is adopted').
lt(1/100, [age(0,18)], [orphan], [orphan], [rich_parents, poor_parents], ' loses parents').
lt(1/10, [], [], [problem_parent], [], ' has a parent with a problem').
lt(1/25, [age(14,20), gay], [], [rich_parents], [unsupportive_parents], ' rejected by parents').

		 /*******************************
		 * school                       *
		 *******************************/

% tp is 'time passed'. It's removed when we increment the age
lt(1, [age(6,18)], [dropped_out], [in_school], [], '').  % if you arent dropped out your in school
lt(1, [age(6,18), dropped_out], [], [], [in_school], '').   % if you drop out you're not in school
lt(0.1, [age(12,18), problem_parent], [ambitious], [dropped_out], [], ' dropped out').
lt(1/50, [age(12,18)], [problem_parent], [dropped_out], [], ' dropped out').
lt(1, [age(18,18), in_school], [], [graduated_hs, tp], [in_school], ' graduated').
lt(0.95, [graduated_hs, age(0, 26), rich_parents], [], [college_freshman], [], ' enrolled in college').
lt(0.9, [college_freshman], [tp], [college_sophmore, tp], [college_freshman], ' became a sophmore').
lt(0.9, [college_sophmore], [tp], [college_junior, tp], [college_sophmore], ' became a junior').
lt(0.9, [college_junior], [tp], [college_senior, tp], [college_junior], ' became a senior').
lt(0.9, [college_senior], [tp], [college_grad, tp], [college_senior], ' graduated').

		 /*******************************
		 * illness                      *
		 *******************************/
lt(1/3, [major_illness], [], [dead], [major_illness], ' die').
lt(1/50, [age(0,3)], [], [major_illness], [], ' develop a major illness').
lt(1/100, [age(4,55)], [], [major_illness], [], ' develop a major illness').
lt(1/10, [drug_use], [], [major_illness], [], ' develop a major illness because of drug use').
lt(1/25, [age(56, 70)], [], [major_illness], [], ' develop a major illness').
lt(1/5, [age(70, 999)], [], [major_illness], [], ' develop a major illness').



