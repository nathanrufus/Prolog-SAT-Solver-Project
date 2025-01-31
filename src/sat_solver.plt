:- begin_tests(cnf).

test(to_cnf_simple) :-
    to_cnf(and(lit(X), or(lit(Y), lit(Z))), CNF),
    CNF == [[X], [Y, Z]].

test(solve_satisfiable) :-
    solve([[X], [not(X), Y], [not(Y)]]),
    X = true, Y = false.

test(solve_unsatisfiable) :-
    \+ solve([[X], [not(X)]]).

:- end_tests(cnf).
