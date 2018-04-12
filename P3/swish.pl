pertenece_m(X, [X|_]) :-  X \= [_|_].
pertenece_m(X, [Y|_]) :- pertenece_m(X, Y).
pertenece_m(X, [_|Rs]) :- pertenece_m(X, Rs).

concatena([], L, L).
concatena([X|L1], L2, [X|L3]) :- concatena(L1, L2, L3).

invierte([], []).
invierte([X|Rx], Y) :- invierte(Rx,M), concatena(M, [X], Y).

insert([X], [], [X]).
insert([A-X], [B-Y|Rs], L3) :- X<Y, concatena([A-X], [B-Y|Rs], L3).
insert([A-X], [B-Y|Rs], L3) :- not(X<Y), insert([A-X], Rs, L4), concatena([B-Y], L4, L3).

elem_count(_,[],0).
elem_count(X,[X|Rs],Xn):- elem_count(X, Rs, Xm), Xn is(1+Xm).
elem_count(X,[Y|Rs],Xn):- X\=Y, elem_count(X, Rs, Xn).
