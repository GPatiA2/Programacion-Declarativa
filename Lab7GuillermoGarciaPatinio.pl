% Guillermo Garcia Patiño Lenza

% ej1

elimina1([ ],X,[ ]).
elimina1([X|R],Y,NR) :- Y == X, elimina1(R,Y, NR).
elimina1([X|R],Y,[X|NR]) :- Y \== X, elimina1(R,Y,NR).

elimina2([ ],X,[ ]).
elimina2([X|R],Y,NR) :- Y = X, elimina2(R,Y, NR).
elimina2([X|R],Y,[X|NR]) :- Y \= X, elimina2(R,Y,NR).

elimina3([ ],X,[ ]).
elimina3([X|R],X,NR) :- elimina3(R,X,NR).
elimina3([X|R],Y,[X|NR]) :- Y \== X, elimina3(R,Y,NR).

% Todos los predicados intentan eliminar un elemento de la lista dada
% Los resultados obtenidos al ejecutar los objetivos son:
%   
%   elimina1([a,b,a,c],a,L). -> L = [b,c] , false
%	elimina1([a,b,a,c],X,L). -> L = [a,b,a,c]
%      Este resultado se debe a que elimina1 requiere que el elemento de la lista
%       y el proporcionado como argumento sean sintacticamente iguales para eliminar el
%       elemento de la lista. 
%       En caso de usar el segundo argumento como argumento de entrada,
%        elimina1 elimina correctamente los elementos de la lista del primer argumento que sean
%        sintacticamente iguales al del segundo.
%       En caso de emplear el segundo argumento como argumento de salida tambien,
%         sucede que al no ser ninguno de los elementos de la lista sintacticamente equivalentes
%         a X (que no toma valores a lo largo de la resolucion del arbol de ese objetivo) , la lista
%         resultante es la original          
%
%	elimina2([a,b,a,c],a,L). -> L = [b,c] , false
%   elimina2([a,b,a,c],X,L). -> X = a y L = [b,c] , false
%     Este predicado trata de hacer lo mismo que el anterior, pero empleando la unificación. Elimina
%        todos los elementos de la lista que no unifiquen con el segundo argumento
%     En el primer caso realiza bien la eliminacion, porque una constante solo unifica con otra en el
%        caso en el que sean las dos la misma constante.
%     En el segundo caso, al hacer el arbol de resolucion del objetivo, se encuentra que X solo puede tomar
%        el valor del primer elemento de la lista, conduciendo a una rama de exito que da las sustituciones
%        anteriores. X no puede tomar el valor de ningun elemento de la lista que no sea el primero, porque
%        al empezar a resolver el arbol, el objetivo no unifica con la primera clausula, pero si con la segunda,
%        donde se sustituye X/a , y al hacer backtracking hasta volver al principio, el objetivo unifica con la 
%        tercera clausula, pero como X si unifica con a, hay un fallo
%
%   elimina3([a,b,a,c],a,L). -> L = [b,c] , false
%   elimina3([a,b,a,c],X,L). -> X = a y L = [b,c] (aparece 2 veces)
%                               X = b y L = [a,a,c]
%                               X = c y L = [a,b,a] , y L = [a,b,a,c]
%
%     Este predicado hace lo mismo que los anteriores, solo que empleando la unificacion como criterio para
%       eliminar un elemento, y la desigualdad sintactica para no hacerlo.
%     En el primer caso se elimina correctamente el elemento 'a' de la lista [a,b,a,c], dando como resultado
%       la lista [b,c], por los mismos motivos que el caso 1 del elimina2
%     El segundo caso es parecido tambien al segundo caso del elimina1, solo que requiere la desigualdad sintactica
%       entre los elementos para dejarlos en la lista. Al hacer el arbol de resolucion del objetivo, cuando se prueba
%       con la tercera clausula, la X no se ha sustituido aun por ningun valor, por lo que ese predicado produce un exito,
%       y se avanza un elemento en la lista. Entonces, se vuelve a probar con la segunda clausula y se produce un exito,
%       ya que siempre una variable puede unificar con una constante, y entonces se procede a hacer una eliminacion como en
%       el primer caso de este predicado.
%       Ademas, al final se devuelve la lista completa por el mismo motivo que en elimina1


% ej2

sumatree1(A,N) :- inorden(A,L), suma(L,N).

inorden(void,[]).
inorden(arbol(R,Ai,Ad),L) :- inorden(Ai,Ii), inorden(Ad,Id), append(Ii,[R|Id],L).

suma([],0) :- !.
suma([X|Xs],N) :- number(X), !, suma(Xs,N2), N is N2 + X.

sumatree2(void,0):- !.
sumatree2(arbol(R,I,D),S):- number(R), !, sumatree2(I,Si), sumatree2(D,Sd), S is Sd + Si + R.

maxtree(void,0) :- !.
maxtree(arbol(R,I,D),M):- maxtree(I,M1), maxtree(D,M2), max(M1,M2,M3), max(M3,R,M).

max(N1,N2,N1):- N1 > N2,!.
max(N1,N2,N2).

% ej3

sublistas(L,S):- findall(X,sublista(X,L),S).

sublista([],_).
sublista([E|L1],[E|L2]):- sublista(L1,L2).
sublista([E|L1],[_|L2]):- sublista([E|L1],L2).

% ej4

hanoi(1,A,B,_,[[A,B]]):- !.
%                            pasar n-1 fichas de la//pasar una ficha de // pasar n-1 fichas de 
%                            primera a la segunda  //primera a tercera //  tercera a primera
hanoi(N,A,B,C,M):- X is (N-1), hanoi(X,A,C,B,L1), hanoi(1,A,B,C,L2), hanoi(X,C,B,A,L3), append(L1,L2,Aux), append(Aux,L3,M).