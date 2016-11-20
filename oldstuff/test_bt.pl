:- use_module(behavior_tree).

main :- repeat,
	tick(root),
	fail.
main.

/*
% sequence
root =>
  setup,
  live.

setup =>
  ??gender_female ??= @@ 1/2.


live =>
  prenatal,
  infancy,
  childhood,
  adolescence,
  young_adulthood,
  adulthood,
  middle_age,
  retirement,
  end_of_life.

prenatal >>
*/

