:- module(fabrizio, [test/1, life_circumstances/4]).

:- use_module(library(mcintyre)).



:- mc.

:- begin_lpad.
life_circumstances([sex(Sex), parent_econ(Econ)]) :-
  sex(Sex),parent_econ(Econ).

sex(m):1/2 ; sex(f) : 1/2.
parent_econ(poor):0.1; parent_econ(middle):0.6; parent_econ(poor):0.3.
:- end_lpad.

/** <examples>

?- mc_sample_arg_first(life_circumstances(L),1,L,O).

*/
test(O) :- mc_sample_arg_first(fabrizio:life_circumstances(L),1,L,O).
