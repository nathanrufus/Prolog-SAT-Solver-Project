:- [sat_solver].

run_benchmarks :-
    writeln('Running SAT benchmarks...'),
    time(solve([[X], [not(X), Y], [not(Y)]])),
    time(solve([[A], [not(A), B], [not(B), C], [not(C)]])).
