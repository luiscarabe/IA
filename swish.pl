pertenece_m(X, [X|_]) :-  X \= [_|_].
pertenece_m(X, [Y|_]) :- pertenece_m(X, Y).
pertenece_m(X, [_|Rs]) :- pertenece_m(X, Rs).
