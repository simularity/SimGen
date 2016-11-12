:- module(talespin, [talespin/0]).

% File: talespin.pl
% Author: Peter Clark
% Date: Jan 1999
% Purpose: Simple and highly improvised reconstruction of Meehan's TALE-SPIN
%      story generator, here applied to aviation incident "stories". This
%      reconstruction undoubtedly misses out a lot of Meehan's program, and
%      also adds in new parts/approaches that Meehan didn't originally use.
%
%  AO - 11/2016 - added comments and cleaned up code, added more events
%

%!	talespin is nondet
%
%	prints a story on each backtracking
%
talespin :-
	repeat,
	talespin_.

talespin_ :-
	initial_situation(InitialSituation),
	initial_goal(InitialSituation, InitialGoals),
        make_best_plan(InitialGoals,
		       InitialSituation,
		       InitialPlan),
        Prob = 0.3, % Probability of incident occurring at a particular step
        execute_plan(InitialPlan, InitialSituation, InitialGoals,
                                Prob, StoryActions, _StorySituations),
        write('Once upon a time...'), nl,
        anglify(StoryActions, StoryText),
        lwrite(StoryText).

% ======================================================================
%               THE STRIPS-LIKE PLANNER
% ======================================================================
% make_plan/3: Simple backward-chaining planner, without a protected goal list.

make_best_plan(Goals, Situation, BestPlan) :-
        findall(Quality-Plan,
                   ( make_plan(Goals,Situation,Plan),
                     plan_quality(Plan,Quality) ),
                RankedPlans),
        sort(RankedPlans, OrderedPlans),
        last(OrderedPlans, _-BestPlan).

plan_quality(Plan, Quality) :-
        length(Plan, Length),                                 % lose 10 points per step
        ( member(evacuate(_),Plan) -> Cost = 1 ; Cost = 0 ),  % lose 1 for evacuating
        Quality is 100 - Length*10 - Cost.

% ----------

make_plan(Goals, Situation, Plan) :-
        make_plan(Goals,
		  Situation,
		  [], % state stack
		  _FinalSituation,
		  Plan).

% DONE TO HERE
% % trying to change goals to a list of goals, and
% making the stack be that of situations.

%!	make_plan(+Goals:list, +CurrentSituation:list, +StateStack:list,
%!	        -EndSituation:list, -Plan:list) is nondet
%
%	Make a plan to achieve the goals in Goals
%
%	@arg Goals list of goals
%	@arg CurrentSituation current situation, a list of items
%       @arg StateStack list of previous situations, (avoid looping)
%	@arg EndSituation final situation, list of items
%	@arg Plan list of actions
make_plan(Goals, Situation, _StateStack, Situation, []) :-
        satisfieds(Goals, Situation).
make_plan(Goals, Situation, GoalStack, NewSituation, Actions) :-
	member(AGoal, Goals), % cant do this
        \+ satisfieds(Goals, Situation),
        \+ member(Goals, GoalStack),                           % avoid looping
        event_definition(action, Action, PCs, Dels, Adds),
        achieves_some(Adds, Goals),
	% DONE TO HERE
        make_plans(PCs, Situation, [Goal|GoalStack], MidSituation, PreActions),
        apply_effects(Dels, Adds, MidSituation, NewSituation),
        append(PreActions, [Action], Actions).

make_plans([], Situation, _, Situation, []).
make_plans([Goal|Goals], Situation, GoalStack, NewSituation, Actions) :-
        make_plan(Goal, Situation, GoalStack, MidSituation, FirstActions),
        make_plans(Goals, MidSituation, GoalStack, NewSituation, RestActions),
        append(FirstActions, RestActions, Actions).

achieves_some(Adds, Goals) :-
	member(G, Goals),
	achieves(_, _, Adds, G).

achieves(_, _, Adds, Goal) :-                           % Goal = effect
        member(Goal, Adds).
achieves(_, _, Adds, Goal) :-                           % Goal = ramification of effect
        rule(( Goal :- Facts )),
        subset(Facts, Adds).

% ======================================================================
%                       THE SIMULATOR
% This is similar to the planner, *except* it will also throw a spanner
% in the works (ie. a happening), requiring replanning.
% ======================================================================
% Final clause - we're done
execute_plan([], FinalSituation, _, _, [], [FinalSituation]) :-
        !.

% 'happening' (incident) happens
execute_plan(_Actions, Sitn, Goal, P, [Happening|NextActions], [Sitn|NextSitns]) :-
        maybe(P),                  % incident happens!! Abandon old Actions and Goal...
        !,
        findall(Happening,
                   ( event_definition(happening,Happening,PCs,_,_),
                     satisfieds(PCs,Sitn) ),	 % is physically feasible
                Happenings),
        rnd_member(Happening, Happenings),      % choose a random happening
        do_event(Happening, Sitn, NewSitn),
        revise_goal(NewSitn, Goal, NewGoal),
        make_best_plan(NewGoal, NewSitn, NewActions),
        execute_plan(NewActions, NewSitn, NewGoal, 0, NextActions, NextSitns).

% normal event happens
execute_plan([Action|Actions], Sitn, Goal, P, [Action|NextActions], [Sitn|NextSitns]) :-
        do_event(Action, Sitn, NewSitn),
        execute_plan(Actions, NewSitn, Goal, P, NextActions, NextSitns).

do_event(Event, Situation, NewSituation) :-
        event_definition(_, Event, _PCs, Dels, Adds),
        apply_effects(Dels, Adds, Situation, NewSituation).

% Do the deletes and adds as appropriate
apply_effects(Dels, Adds, Situation, NewSituation) :-
        removes(Dels, Situation, MidSituation),
        append(Adds, MidSituation, NewSituation).

% ======================================================================
%               OTHER UTILITIES
% ======================================================================

satisfieds([], _).
satisfieds([F|Fs], S) :-
        satisfied(F, S),
        satisfieds(Fs, S).

satisfied(Fact, Situation) :-
        member(Fact, Situation).
satisfied(Fact, Situation) :-
        rule((Fact :- Facts)),                    % Fact is a ramification of the world
        satisfieds(Facts, Situation).

% ---------- writing...

lwrite([]).
lwrite([X|Xs]) :- write('       '), lwrite2(X), nl, lwrite(Xs).

lwrite2([]).
lwrite2([BitX|BitXs]) :- !, write(BitX), lwrite2(BitXs).
lwrite2(X) :- write(X).

anglify([], []).
anglify([Event|Events], [English|Englishs]) :-
        event_english(Event, English),
        anglify(Events, Englishs).

% ======================================================================
%               GENERAL UTILITIES
% ======================================================================

removes([], L, L).
removes([R|Rs], L, NewL) :-
        remove(R, L, MidL),
        removes(Rs, MidL, NewL).

remove(A, [A|B], B).
remove(A, [C|B], [C|NewB]) :-
        remove(A, B, NewB).

subset([], _).
subset([X|Xs], Ys) :- remove(X, Ys, RestYs), subset(Xs, RestYs).

nmember(Elem, List, N) :-
        nmember(Elem, List, 1, N).

nmember(Elem, [Elem|_], N, N).
nmember(Elem, [_|List], NSoFar, N) :-
        NewN is NSoFar + 1,
        nmember(Elem, List, NewN, N).

% ---------- Randomization utilities

:- dynamic lastrnd/1.
lastrnd(0).

maybe(P) :- random_float < P, !.		   % succeed with probability P

rnd_member(X, Xs) :-
        length(Xs, L),
        R is random_float,
        N is integer(R*L) + 1,
        nmember(X, Xs, N), !.

% ======================================================================
%               THE FLIGHT INCIDENT KNOWLEDGE BASE
% ======================================================================

event_definition(Type, Event, PCs, Adds, Dels) :- ed(Type, Event, PCs, Adds, Dels, _).
event_english(Event, English) :- ed(_, Event, _, _, _, English).

% in Seattle, scheduled for Dallas
initial_situation([ plocation(passengers1, gate(seattle)),
                      alocation(airplane1, gate(seattle)),
                      flight_path(seattle, chicago),
                      flight_path(chicago, dallas),
                      airplane(airplane1),
                      passengers(passengers1),
		      blocation(bags1, gate(seattle))]).

initial_goal(Situation, [plocation(passengers1, gate(dallas)), blocation(bags1, gate(dallas))]) :-
	member(alocation(airplane1, gate(Loc)), Situation),
	Loc \= dallas.

% ---------- Routine actions... ----------

ed(action, load_bags(Bags, Airplane),
   [blocation(Bags, gate(Airport)), alocation(Airplane, gate(Airport))],
   [blocation(Bags, gate(Airport))],
   [bags_on_board(Airplane, Bags)],
   'The bags were loaded').

ed(action, unload_bags(Bags, Airplane),
   [bags_on_board(Airplane, Bags), alocation(Airplane, gate(Airport))],
   [bags_on_board(Airplane, Bags)],
   [blocation(Bags, gate(Airport))],
   'The bags were unloaded').

ed(action, load(Passengers,Airplane),
        /*pcs*/ [plocation(Passengers,gate(Airport)),alocation(Airplane,gate(Airport))],
        /*del*/ [plocation(Passengers,gate(Airport))],
        /*add*/ [contains(Airplane,Passengers)],
        /*txt*/ 'The passengers boarded the plane.' ).

ed(action, taxi_to_runway(Airplane),
        /*pcs*/ [alocation(Airplane,gate(Airport))],
        /*del*/ [alocation(Airplane,gate(Airport))],
        /*add*/ [alocation(Airplane,runway(Airport))],
        /*txt*/ 'The plane taxiied to the runway.' ).

ed(action, take_off(Airplane,Airport),
        /*pcs*/ [alocation(Airplane,runway(Airport))],
        /*del*/ [alocation(Airplane,runway(Airport))],
        /*add*/ [alocation(Airplane,near(Airport))],
        /*txt*/ ['The plane took off from ',Airport,'.']).

ed(action, cruise(Airplane,Airport1,Airport2),
        /*pcs*/ [flight_path(Airport1,Airport2),alocation(Airplane,near(Airport1))],
        /*del*/ [alocation(Airplane,near(Airport1))],
        /*add*/ [alocation(Airplane,near(Airport2))],
        /*txt*/ ['The plane cruised towards ',Airport2,'.']).

ed(action, land(Airplane,Airport2),
        /*pcs*/ [alocation(Airplane,near(Airport2))],
        /*del*/ [alocation(Airplane,near(Airport2))],
        /*add*/ [alocation(Airplane,runway(Airport2))],
        /*txt*/ ['The plane landed at ',Airport2,'.']).

ed(action, taxi_to_gate(Airplane),
        /*pcs*/ [alocation(Airplane,runway(Airport))],
        /*del*/ [alocation(Airplane,runway(Airport))],
        /*add*/ [alocation(Airplane,gate(Airport))],
        /*txt*/ 'The plane taxiied to the gate.' ).

ed(action, unload(Passengers,Airplane),
        /*pcs*/ [contains(Airplane,Passengers),alocation(Airplane,gate(Airport))],
        /*del*/ [contains(Airplane,Passengers)],
        /*add*/ [plocation(Passengers,gate(Airport))],
        /*txt*/ 'The passengers disembarked.' ).

% ---------- Emergency actions... ----------

ed(action, evacuate(Airplane),
        /*pcs*/ [a_on_ground(Airplane),alocation(Airplane,Loc),contains(Airplane,Passengers)],
        /*del*/ [contains(Airplane,Passengers)],
        /*add*/ [plocation(Passengers,Loc)],
        /*txt*/ 'The passengers were evacuated from the plane.' ).

ed(action, emergency_landing(Airplane),
        /*pcs*/ [alocation(Airplane,near(Airport2))],
        /*del*/ [alocation(Airplane,near(Airport2))],
        /*add*/ [alocation(Airplane,on_ground_near(Airport2))],
        /*txt*/ ['The pilot made an emergency landing near ',Airport2,'.']).

ed(action, medical_help(Passengers),
        /*pcs*/ [plocation(Passengers, gate(_))],               % any gate
        /*del*/ [],
        /*add*/ [medical_help(Passengers)],
        /*txt*/ 'Medical help was provided.' ).

% ---------- Possible happenings... ----------

ed(happening, fire(engine),
        /*pcs*/ [],                                     % can happen anywhere
        /*del*/ [],
        /*add*/ [on_fire(engine)],
        /*txt*/ 'The engine caught fire.' ).

% ---------- Possible happenings... ----------

ed(happening, ill_passenger,
        /*pcs*/ [contains(Airplane,Passengers),passengers(Passengers),airplane(Airplane)],
        /*del*/ [],
        /*add*/ [ill_passenger],
        /*txt*/ 'A passenger became very ill.' ).

ed(happening, bag_left,
   /*pcs*/ [alocation(Airplane, gate(_)

% Ramifications of facts about the world...
rule(( a_on_ground(Airplane) :- [alocation(Airplane,gate(_))] )).
rule(( a_on_ground(Airplane) :- [alocation(Airplane,runway(_))] )).
rule(( a_on_ground(Airplane) :- [alocation(Airplane,on_ground_near(_))] )).
rule(( p_on_ground(Passengers) :- [plocation(Passengers,gate(_))] )).
rule(( p_on_ground(Passengers) :- [plocation(Passengers,runway(_))] )).
rule(( p_on_ground(Passengers) :- [plocation(Passengers,on_ground_near(_))] )).

% Rules for revising the goal
% TODO goals are now lists
revise_goal(Situation, plocation(Passengers,_), Goal) :- % If the engine's on fire,
        memberchk(on_fire(engine), Situation), !,   % get to the ground asap!
        Goal = p_on_ground(Passengers).
revise_goal(Situation, plocation(Passengers,_), Goal) :- % If a passenger's ill,
        memberchk(ill_passenger, Situation), !,	   % get to a gate somewhere.
        Goal = medical_help(Passengers).
revise_goal(_Situation, Goal, Goal).
