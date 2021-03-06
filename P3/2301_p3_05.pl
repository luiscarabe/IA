%   Practica 3 IA
%    Grupo: 2301
%    Pareja: 05
%    Autor 1: Nuria Cuaresma Saturio
%    Autor 2: Luis Carabe Fdez-Pedraza


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

% 2 - invierte(X,Y)
% Predicado que usamos para comprobar si una lista es el resultado de invertir
% otra.
% Input : 	X - lista 1
% 			Y - lista 2 (deberia ser la lista 1 invertida)

% Caso base: Una lista vacia es inversa de si misma
invierte([], []).

% Caso recursivo:
% Dividimos la primera lista en su primer elemento y el resto, llamamos a invierte con 
% este resto y concatenamos el resultado con el primer elemento, 
% este será el resultado en la segunda lista
invierte([X|Rx], Y) :- invierte(Rx,M), concatena(M, [X], Y).

% 3 - insert(X-P,L,R)​
% Predicado que usamos para insertar elementos en una lista de manera ordenada.
% Input : 	X-P - elemento a insertar
% 			L - lista ordenada donde insertamos
%			R - lista con los elementos insertados ordenadamente

% Caso base: Insertar un elemento en una lista vacia
insert([X], [], [X]).

% Caso recursivo:
% En caso de que la posicion(P) del elemento a insertar sea menor o igual que el elemento 
% de la lista a analizar, concatenamos la lista detras del elemento.
% En caso contrario llamamos de nuevo a insertar con el resto de la lista concatenando 
% con delante el primero de la lista que ya hemos comparado.
insert([A-X], [B-Y|Rs], L3) :- X=<Y, concatena([A-X], [B-Y|Rs], L3).
insert([A-X], [B-Y|Rs], L3) :- not(X=<Y), insert([A-X], Rs, L4), concatena([B-Y], L4, L3).

%4
% 4.1 - elem_count(X,L,Xn)
% Predicado que cuenta la cantidad de veces que aparece un elemento en una lista.
% Input : 	X - elemento a contar
% 			L - lista de elementos
%			Xn - número de veces que aparece el elemento

% Caso base: Cualquier elemento esta 0 veces en una lista vacia
elem_count(_,[],0).
% Caso recursivo:
% En caso de que la encontremos el mismo elemento en la lista sumamos 1 al numero de veces
% y volvemos a llamar de manera recursiva con el resto de la lista
% En caso de que no sea el mismo mantenemos el mismo numeor de veces, unicamente llamando 
% recursivamente con el resto.
elem_count(X,[X|Rs],Xn):- elem_count(X, Rs, Xm), Xn is(1+Xm).
elem_count(X,[Y|Rs],Xn):- X\=Y, elem_count(X, Rs, Xn).

% 4.2 - list_count(X,Y,Z)
% Predicado que usamos para comprobar si una lista (Z) contiene las ocurrencias de
% los elementos de la lista X en la lista Y en forma de par
% Input : 	X - lista con los elementos que buscamos en Y
% 			Y - lista de elementos
% 			Z - lista resultante de los pares elementoDeX-#ocurrenciasEnY

% Caso base: si X esta vacia, la lista resultante tambien
list_count([],_,[]).

% Caso recursivo:
% Dividimos la primera lista en su primer elemento (X) y el resto, llamamos a list_count con 
% este resto, el resultado sera M. Ahora contamos con ayuda de elem_count, las veces que aparece
% el elemento X en la segunda lista, este resultado sera Rs. 
% Ahora concatenamos el par X-Rs con M, este debera ser el resultado de la tercera lista
list_count([X|Rx],Y,L) :- list_count(Rx, Y, M), elem_count(X,Y,Rs), concatena([X-Rs],M, L).

% 5 - sort_list(X,Y)
% Predicado que usamos para comprobar si una lista contiene los pares de elementos de otra
% en orden.
% Input : 	X - lista con los pares de elementos
% 			Y - lista que deberia contener los elementos de X ordenados

% Caso base: Una lista vacia esta ordenada siempre
sort_list([],[]).

% Caso recursivo:
% Dividimos la primera lista en su primer par y el resto, llamamos a sort-list con 
% este resto e insertamos en el resultado el primer par, 
% esta inserción será el resultado en la segunda lista
sort_list([A-X|Rs], L) :- sort_list(Rs, M), insert([A-X], M, L).


% 6 - ​build_tree(List,Tree)
% Predicado que usamos para transformar una lista de pares de elementos ordenados en
% un árbol de Huffman.
% Input : 	List - lista con los pares de elementos ordenador
% 			Tree - árbol de Huffman obtenido.

% Caso base: Una lista vacia no hay árbol
build_tree([], nil).
% Si la lista tiene un elemento insertamos en el arbol como hoja (con null en sus hijos)
build_tree([A], tree(A, nil, nil)).
% Caso recursivo:
% por cada elemento de la lista hacemos un arbol cuyo nodo sea 1, el hijo izquierdo el elemeno 
% de la lista y el derecho la continuación del arbol con el resto d enodos de la lista, lamando 
% de manera recursiva
build_tree([A-_|Rs], tree(1, X, M) ) :- Rs \= [], build_tree(Rs, M), X = tree(A, nil,nil).

% 7

% 7.1 - encode_elem(X,Y,Tree)
% Predicado que usamos para codificar X en Y basandose en la estructura del arbol
% Tree (arbol de Huffman)
% Input : 	X - elemento a codificar
% 			Y - elemento codificado
% 			Tree - arbol de Huffman en el que se basa para codificar

% Caso base: 
% Caso excepcional: cuando se crea un arbol con un solo elemento, se crea de la forma tree(A,nil,nil)
% Por tanto, el elemento de ese arbol se codifica como 0
encode_elem(A, [0], tree(A, nil, nil)).
% Caso normal en el que tenemos el elemento en el arbol sin estar en la ultima rama.
% A esta altura, el elemento esta en el subarbol izquierdo (el derecho no nos interesa)
% codificamos con un 0
encode_elem(A, [0], tree(1, tree(A, nil, nil),_)). 
% Caso normal en el que tenemos el elemento en el arbol estando en la ultima rama.
% A esta altura, el elemento esta en el subarbol derecho (el izquierdo no nos interesa)
% codificamos con un 1
encode_elem(A, [1], tree(1,_, tree(A, nil,nil))). % Caso ultima rama de arbol

% Caso recursivo:
% Si el subarbol derecho no contiene al elemento que buscamos (caso ya cubierto), entonces,
% llamamos a encode_elem con el subarbol derecho guardando la codificacion en M, despues concatenamos
% un 1 con M, siendo esto la codificacion del elemento
encode_elem(A, X, tree(1, tree(_, nil, nil), Rs)) :- Rs \= tree(A,nil,nil), encode_elem(A,M,Rs), concatena([1], M, X).

% 7.2 - encode_list(X,Y, Tree)
% Predicado que usamos para codificar X en Y basandose en la estructura del arbol
% Tree (arbol de Huffman)
% Input : 	X - lista de elementos a codificar
% 			Y - lista de elementos codificados
% 			Tree - arbol de Huffman en el que se basa para codificar

% Caso base: una lista vacia se codifica como otra lista vacia
encode_list([], [],_).

% Caso recursivo: dividimos la primera lista en el primer elemento y el resto. Llamamos a encode_list
% con dicho resto y guardamos la lista de codificaciones en A. Codificamos el elemento X con encode_elem y 
% concatenamos dicha codificacion con A, siendo esto la lista de codificaciones resultante
encode_list([X | Rs], Y, T) :- encode_list(Rs,A,T), encode_elem(X,R,T), concatena([R],A,Y).

% 8- encode(L1, L2)
% Predicado que usamos para codificar la lista L1 en L2, usando el predicado diccionario
% Input : 	L1 - lista con elementos a codificar
%			L2 - lista resultado

%Función diccionario
dictionary([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]).
% Caso base: Una lista vacia 
encode([],nil).
% Realizamos una llamada a todas las funciones anteriormente codificadas
encode(L,X) :-dictionary(D),list_count(D, L, LC),sort_list(LC, SL),invierte(SL, I),build_tree(I, BT),encode_list(L, X, BT).
