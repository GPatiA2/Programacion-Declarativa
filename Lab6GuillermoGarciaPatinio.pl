% Guillermo Garcia Patinio Lenza
% Tenemos un conjunto de fichas apiladas en tres columnas sobre una mesa.
%  segun rl esquema siguiente:
%
%   D
%   C   G
%   B   F   I
%   A   E   H
%  -----------
% Esta informacion se representa mediante los siguientes predicados.
% sobre(X,Y) <-> la ficha X esta sobre la ficha Y.
% izquierda(X,Y) <-> la ficha X esta inmediatamente a la izquierda 
% de la ficha Y.
% cima(X) <-> la ficha X esta en la cima de una columna.

% hechos
%   D
%   C   G
%   B   F   I
%   A   E   H
%  -----------

cima(d).
cima(g).
cima(i).

sobre(d,c).
sobre(c,b).
sobre(b,a).

sobre(g,f).
sobre(f,e).

sobre(i,h).

izquierda(c,g).

izquierda(b,f).
izquierda(f,i).

izquierda(a,e).
izquierda(e,h).

% Se definen nuevos predicados para manejar esta informacion.


% por_encima_de(X,Y).
% la ficha X esta en la misma pila que la ficha Y y mas arriba.
% uso: por_encima_de(e/s,e/s).
por_encima_de(X,Y) :- sobre(X,Y).
por_encima_de(X,Y) :- sobre(X,Z), por_encima_de(Z,Y).

% por_encima_de_ERROR(X,Y).
%Llamadas recursivas infinitas cuando no hay m‡s soluciones o el objetivo es falso.
% uso: por_encima_de_ERROR(e/s, e/s). 
por_encima_de_ERROR(X,Y) :- sobre(X,Y).
por_encima_de_ERROR(X,Y) :- por_encima_de_ERROR(X,Z), sobre(Z,Y).


% pila_izquierda(X,Y)
% la ficha X est‡ en la pila situada inmediatamente a la izquierda de 
%la pila en la que est‡ la ficha Y
% uso: pila_izquierda(e/s,e/s)
pila_izquierda(X,Y) :- izquierda(X,Y).				% misma altura
pila_izquierda(X,Y) :- izquierda(Z,Y), por_encima_de(X,Z).	% X mas arriba que Y
pila_izquierda(X,Y) :- izquierda(X,Z), por_encima_de(Y,Z).	% X mas abajo que Y

 
% por_arriba(X,L).
% L es la lista que contiene todas las fichas que estan por encima de la ficha X.
% uso:  por_arriba(e/s,e/s).
por_arriba(X,[]) :- cima(X).
por_arriba(X,[Y|L]) :- sobre(Y,X), por_arriba(Y,L).
 


% poner_encima(X,Y)
% la ficha X se puede poner encima de la ficha Y si ambas estan en 
% la cima de su pila, y en pilas contiguas.
% uso: poner_encima(e/s,e/s)
poner_encima(X,Y) :- cima(X), cima(Y), pilas_contiguas(X,Y).


% pilas_contiguas(X,Y)
% la pila de la ficha X y la de la ficha Y estan una al lado de la otra.
% uso: pilas_contiguas(e/s,e/s).
pilas_contiguas(X,Y) :- pila_izquierda(X,Y).
pilas_contiguas(X,Y) :- pila_izquierda(Y,X).

%% Ejercicio 1
mas_por_encima_que(X,Y) :- por_arriba(X,L) , por_arriba(Y,L2) , length(L,T), length(L2,S) , T > S.

% Predicado para obtener la longitud de una lista. No se por que no funciona correctamente
% Por eso he usado length en la solucion del ejercicio
long([],0).
long([_|Xs],L) :- long(Xs, L2), L == L2+1 .
 
%% Ejercicio 2

% Si quisiera concatenar por la derecha lo que me sobre de una de las listas en el caso de que
%   una fuera mas larga que la otra, tendria que escribir mezcla([],L,L)
mezcla([],_,[]).
mezcla(_,[],[]).
% Para mezclar dos listas, pongo en el resultado:
%   - Como primer elemento, el primer elemento de la primera
%   - Como segundo elemento, el primer elemento de la segunda
%   - Concateno eso por la izquierda a la mezcla de las dos listas que me quedan
mezcla([E1|L1], [E2|L2], [E1,E2|L] ) :- mezcla(L1,L2,L).

%% Ejercicio 3

% El vacio esta contenido donde sea
contenido([],_).
% Xs esta contenido en Ys (si el primer elemento es diferente) si Xs esta contenido en 
%    la cola de Ys
contenido([E1|L1],[E2|L2]) :- contenido_aux([E1|L1], L2).
% Xs esta contenido en Ys (si el primer elemento es igual) si la cola de Xs esta contenida en
%    la cola de Ys
contenido([E1|L1],[E1|L2]) :- contenido_aux(L1, L2).

% Este predicado no funciona para elementos repetidos -> caso [1,1] [1,2,3]
% Tengo que guardar la lisa entera para volver a empezar cuando encuentre un elemento
% Este va a consistir en ir buscando los elementos de L1 en L2 uno por uno

% El tercer parametro de contenido_aux es siempre la lista Ys 
% El segundo parametro de contenido_aux es siempre un sufijo de Ys. El primer elemento de 
%    ese sufijo, es el que voy a comparar con el que estoy buscando de Xs.
% El primer paramentro de contenido_aux es tambien un sufijo de Xs. Su primer elemento es el que
%    estoy buscando actualmente

contenido2(Xs,Ys) :- contenido_aux(Xs,Ys,Ys).
contenido_aux([],_,_).
% Si el primer elemento es diferente, avanzo en Ys
contenido_aux([E1|L1], [E2|L2], Orig) :- contenido_aux([E1|L1], L2, Orig).
% Cuando encuentro el elemento de Xs que estoy buscando, avanzo Xs quitando el primer elemento
%    y reinicio la busqueda restaurando el segundo parametro.
contenido_aux([E1|L1], [E1|L2], Orig) :- contenido_aux(L1, Orig, Orig).

% Con la misma idea, el ejercicio de la sublista.
sublista([],_).
sublista(Xs,Ys) :- sublista_aux(Xs,Ys).

sublista_aux([],_).
sublista_aux([E1|L1], [E1|L2]):- sublista_aux(L1, L2).
sublista_aux([E1|L1], [E2|L2]):- sublista_aux([E1|L1], L2).


%% Ejercicio 4

nat(c).
nat( suc(X) ) :- nat(X).

sum(c,X,X).
sum(suc(X), Y, suc(Z)) :- suma(X,Y,Z).

nodos(void,c).
nodos(arbol(_,Ai,Ad), suc(Y)) :- nodos(Ai, X), nodos(Ad, Z), suma(X,Z,Y).