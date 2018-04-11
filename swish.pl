pertenece_m(X, [X|_]) :-  X \= [_|_].
pertenece_m(X, [Y|_]) :- pertenece_m(X, Y).
pertenece_m(X, [_|Rs]) :- pertenece_m(X, Rs).

concatena([], L, L).
concatena([X|L1], L2, [X|L3]) :- concatena(L1, L2, L3).

invierte([], []).
invierte([X], [X]).
invierte([X|Rx], Y) :- invierte(Rx,M), concatena(M, [X], Y).
