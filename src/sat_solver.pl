% Base case: literals remain unchanged
normalise(lit(X), lit(X)).

% Logical operators are recursively processed
normalise(and(A, B), and(NA, NB)) :- normalise(A, NA), normalise(B, NB).
normalise(or(A, B), or(NA, NB)) :- normalise(A, NA), normalise(B, NB).
normalise(not(A), not(NA)) :- normalise(A, NA).

% Transform equivalence (A ⇔ B) into implications
normalise(equivalence(A, B), and(or(not(NA), NB), or(not(NB), NA))) :- 
    normalise(A, NA), normalise(B, NB).

% Transform implication (A ⇒ B) into OR (¬A ∨ B)
normalise(implies(A, B), or(not(NA), NB)) :- 
    normalise(A, NA), normalise(B, NB).

% Convert formula to CNF
to_cnf(Formula, CNF) :- 
    normalise(Formula, NFormula), 
    convert_to_cnf(NFormula, CNF).

convert_to_cnf(lit(X), [[X]]).
convert_to_cnf(not(lit(X)), [[not(X)]]).
convert_to_cnf(or(A, and(B, C)), CNF) :-
    convert_to_cnf(or(A, B), CNF1),
    convert_to_cnf(or(A, C), CNF2),
    append(CNF1, CNF2, CNF).
convert_to_cnf(and(A, B), CNF) :-
    convert_to_cnf(A, CNF1),
    convert_to_cnf(B, CNF2),
    append(CNF1, CNF2, CNF).
convert_to_cnf(or(A, B), [Clause]) :-
    convert_to_cnf(A, CNF1),
    convert_to_cnf(B, CNF2),
    append(CNF1, CNF2, Clause).

% DPLL Solver
solve([]). % Satisfiable
solve([[]]) :- fail. % Unsatisfiable

solve(CNF) :- 
    select([Lit], CNF, RestCNF), 
    propagate(Lit, RestCNF, NewCNF),
    solve(NewCNF).

solve(CNF) :-
    select(Clause, CNF, RestCNF),
    member(Lit, Clause),
    propagate(Lit, RestCNF, NewCNF),
    solve(NewCNF).

propagate(Lit, CNF, NewCNF) :-
    exclude(member(Lit), CNF, CNF1),
    negate(Lit, NLit),
    remove_negations(NLit, CNF1, NewCNF).

negate(not(X), X).
negate(X, not(X)).

remove_negations(_, [], []).
remove_negations(Lit, [Clause | CNF], [NewClause | NewCNF]) :-
    exclude(==(Lit), Clause, NewClause),
    remove_negations(Lit, CNF, NewCNF).
