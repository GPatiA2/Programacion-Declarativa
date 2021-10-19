% Ejercicios hoja 6

% 1

sumintersec2(_,[],0).
sumintersec2([],_,0).
sumintersec2([E1|L1],[E2|L2],S) :- E1 > E2 -> sumintersec2([E1|L1],L2,S) ; E1 < E2 -> sumintersec2(L1, [E2|L2],S) ; !,sumintersec2(L1,L2,S2), S is S2 + E1.

% 2

% preguntar por que hace falta el corte en el caso base -> no se donde hace backtrack en caso de exito para tener que cortarlo
nomiembro(_,[]) :- !.
% aqui necesito \= para comprobar que no es el elemento exacto
% si uso =\= significa que a ambos lados tengo expresiones evaluables
% me sobra ademas el corte entre medias de E \= X y nomiembro(X,Xs).
nomiembro(X,[E|Xs]) :- E \= X, nomiembro(X,Xs).

% empleo setof que me devuelve las X que cumplen la propiedad member(X,L) en una lista C que no tiene repetidos y esta ordenada
hazconjunto(L,C) :- setof(X, member(X,L),C).

hazconjunto2([],[]) :- !.
% Si el primer elemento no esta repetido, en Xs, lo puedo poner como primer elemento del
%    conjunto que resulta de hacer conjunto el resto de la lista
% Pongo un corte una vez se que no esta repetido para no hacer mas backtrack
hazconjunto2([X|Xs],[X|C]):- nomiembro(X,Xs), !, hazconjunto(Xs,C).
hazconjunto2([_|L],C):- hazconjunto(L,C).

% para hacer la union de dos conjuntos, los junto usando append y luego quito sus repetidos con setof
union(C1,C2,C) :- append(C1,C2,C3), setof(X, member(X,C3), C).

union2([],C,C2):- !, hazconjunto2(C,C2).
% si la X no esta en C2, la pongo en el resultado de la union de los dos 
union2([X|C1],C2,[X|C]) :- nomiembro(X,C2), ! , union2(C1,C2,C).
% si la X esta en C2 (solo llego hasta aqui si he fallado en lo de antes del corte)
% entonces no pongo la X en el resultado de unir C1 y C2
union2([X|C1],C2,C):- union2(C1,C2,C).

union3(C1,C2,C):- append(C1,C2,Aux), hazconjunto2(Aux,C).

% ahora empleo setof para que me devuelva los elementos que pertenecen a los dos conjuntos en una lista C ordenada sin repetidos
interseccion(C1,C2,C) :- setof(X, (member(X,C1),member(X,C2)), C).

interseccion2([],_,[]) :- !.
interseccion2(_,[],[]) :- !.
% si los dos primeros elementos son iguales, lo puedo poner delante de la interseccion
%   del resto de los dos cjtos
interseccion2([X|Xs],[X|Ys],[X|Zs]):- !, interseccion(Xs,Ys,Zs).
% si no lo son (y no me importa lo que sea el segundo porque si he llegado hasta aqui es porque)
%  he fallado en todo lo que hay antes), Zs sera la interseccion de lo que me queda en ambos
interseccion2([X|Xs], [_|Ys], Zs):- interseccion(Xs,Ys,Zs).

%3

treeSort(Xs,XsO) :- insertar(Xs,void,T), inorden(T,XsO).

% mi definicion original es esta:
% insertar([X|Xs],arbol(R,Ai,Ad),T):- X > R -> insertarE(X,Ad,Ad2), insertar(Xs,arbol(R,Ai,Ad2),T); insertarE(X,Ai,Ai2) , insertar(Xs, arbol(R,Ai2,Ad),T).
% pero esta es mas sencilla

insertar([],arbol(R,Ai,Ad),arbol(R,Ai,Ad)) :-  !.
insertar([X|Xs],void,T) :- !, insertar(Xs,arbol(X,void,void),T).
insertar([X|Xs],T,T2):- insertarE(X,T,T3), insertar(Xs,T3,T2).

insertarE(X,void,arbol(X,void,void)) :- ! .
insertarE(X,arbol(R,Ai,Ad), arbol(R,Ai,Ad2)):- X @> R ,!,insertarE(X,Ad,Ad2).
insertarE(X,arbol(R,Ai,Ad), arbol(R,Ai2,Ad)):- X @< R ,!,insertarE(X,Ai,Ai2).

inorden(void,[]).
inorden(arbol(R,Ai,Ad),L) :- inorden(Ai,Ii), inorden(Ad,Id), append(Ii,[R],Aux), append(Aux,Id,L).

%4

contenido2(void,_):- !.
contenido2(arbol(X,I,D), arbol(X,I2,D2)):- contenido(I,I2), contenido(D,D2).
contenido2(arbol(X,I,D), arbol(Y,I2,D2)):- X @> Y , ! , contenido(arbol(X,void,D),D2), contenido(I,arbol(Y,I2,D2)).
contenido2(arbol(X,I,D), arbol(Y,I2,D2)):- contenido(arbol(X,I,void), I2), contenido(D, arbol(Y,I2,D2)).

contenido(arbol(X,L,R),void):- fail.
contenido(void,void).
contenido(arbol(X,L,R),arbol(X2,L2,R2)):- X < X2 -> contenido(arbol(X,L,R),L2) ; X > X2 -> contenido(arbol(X,L,R),R2) ; contenido(L,L2), contenido(R,R2).

%5

monomio(E,_):- var(E), !, E == X.
monomio(E,_):- !, number(E).
monomio(E,X):- functor(E,*,2), arg(2,E,X), arg(1,E,K), atomic(K).
monomio(E,X):- functor(E,^,2), arg(2,E,X), arg(1,E,K), atomic(K).
monomio(E,X):- functor(E,*,2), arg(2,E,M), arg(1,E,K), atomic(K), functor(M,^,2), arg(2,M,X).

monomio2(Y,X) :- var(Y), !, Y == X.
monomio2(K*Y^N,X) :- !, Y == X, number(K), integer(N), N >= 0 .
monomio2(Y^N,X):- !, Y == X, integer(N), N>= 0.
monomio2(K*Y,X):- !, Y == X, number(K).
monomio2(K,X):- number(K).

polinomio2(P+M,X):- !, polinomio2(P,X), monomio2(M,X).
polinomio2(P-M,X):- !, polinomio2(P,X), monomio2(M,X).
polinomio2(M,X):- monomio2(M,X).

polinomio(E,X) :- functor(E,+,2), arg(1,E,T), monomio(T,X), arg(2,E,Z), polinomio(Z,X).


%7

% emplear predicadod de sublista y los predicados de agregacion.