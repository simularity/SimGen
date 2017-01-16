main :-
    reset(p, Ball, Cont),
    write('in main the Ball is '),writeln(Ball),
    Ball = val(3), % something making the reset in handle_ball
%    backtrack?
    % because something can't unify?
    writeln('unified the ball in main'),
    call(Cont).

p :-
%    getval(X),
    shift(val(X)),
    write('in p with X='),writeln(X),
    fail.
p :- writeln(hi).

getval(X) :-
%    X = 5.
    shift(val(X)).

/*
 *  OK, bizarre!
 * main fails without a choice point
 * because
 *
- main.
main
3
hi
false.


?- p.
3
hi
true.

?- call(p).
3
hi
true.

?-

*/

