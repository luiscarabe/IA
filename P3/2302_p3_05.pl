% 1 - pertenece_m(X,Y)
% Predicado que usamos para comprobar si un elemento esta en una lista
% o en alguna de sus sublistas.
% Input : 	X - elemento a buscar en Y
% 			Y - lista (con posibles sublistas)

% Caso base: Si el primer elemento de la lista no es una lista (es un atomo),
%  es true si es igual al elemento que buscamos
pertenece_m(X, [X|_]) :-  X \= [_|_].

% Caso recursivo: 
% Si el primer elemento de la lista es una lista, buscamos el elemento en dicha lista
pertenece_m(X, [Y|_]) :- pertenece_m(X, Y).
% Buscamos en el resto de la lista
pertenece_m(X, [_|Rs]) :- pertenece_m(X, Rs).

% Predicado que concatena el primer argumento con el segundo y guarda el resultaod en el tercero
concatena([], L, L).
concatena([X|L1], L2, [X|L3]) :- concatena(L1, L2, L3).

% 2

invierte([], []).
invierte([X|Rx], Y) :- invierte(Rx,M), concatena(M, [X], Y).

%3

insert([X], [], [X]).
insert([A-X], [B-Y|Rs], L3) :- X=<Y, concatena([A-X], [B-Y|Rs], L3).
insert([A-X], [B-Y|Rs], L3) :- not(X=<Y), insert([A-X], Rs, L4), concatena([B-Y], L4, L3).

%4
%4.1

elem_count(_,[],0).
elem_count(X,[X|Rs],Xn):- elem_count(X, Rs, Xm), Xn is(1+Xm).
elem_count(X,[Y|Rs],Xn):- X\=Y, elem_count(X, Rs, Xn).

%4.2

list_count([],_,L) :- [] = L.
list_count([X|Rx],Y,L) :- list_count(Rx, Y, M), elem_count(X,Y,Rs), concatena([X-Rs],M, L).

%5

sort_list([],[]).
sort_list([A-X|Rs], L) :- sort_list(Rs, M), insert([A-X], M, L).

%6

build_tree([], nil).
build_tree([A], tree(A, nil, nil)).
build_tree([A-_|Rs], tree(1, X, M) ) :- Rs \= [], build_tree(Rs, M), X = tree(A, nil,nil).

%7
%7.1

encode_elem(A, [0], tree(A, nil, nil)). % caso base extraordinario

encode_elem(A, [0], tree(1, tree(A, nil, nil),_)). 
encode_elem(A, [1], tree(1,_, tree(A, nil,nil))). % Caso ultima rama de arbol
encode_elem(A, X, tree(1, tree(_, nil, nil), Rs)) :- Rs \= tree(A,nil,nil), encode_elem(A,M,Rs), concatena([1], M, X).

%7.2

encode_list([], [],_).
encode_list([X | Rs], Y, T) :- encode_list(Rs,A,T), encode_elem(X,R,T), concatena([R],A,Y).


