% Pedro Fuentes Urfeig 1088/22
% Juan Manuel Zimmerman 123/23
% Sebastian Andres 1088/22
% Vicente Tenconi 1171/22

%%%%%%%%%%%%%%%%%%%%%%%%
%% Tablero
%%%%%%%%%%%%%%%%%%%%%%%%

tablero(ej5x5, T) :-
    tablero(5, 5, T),
    ocupar(pos(1, 1), T),
    ocupar(pos(1, 2), T).

tablero(libre20, T) :-
    tablero(20, 20, T).

%% Ejercicio 1 %%

%% tablero(+Filas,+Columnas,-Tablero) instancia una estructura de tablero en blanco
%% de Filas x Columnas, con todas las celdas libres.

tablero(0, _, []).
tablero(_, 0, []).
tablero(F, C, [Fila|Resto]) :-
    F > 0,
    C > 0,
    F1 is F - 1,
    crear_fila(C, Fila),
    tablero(F1, C, Resto).

% crear_fila(+Columnas, -Fila) instancia una fila de celdas libres
crear_fila(0, []).
crear_fila(M, [_|Resto]) :-
    M > 0,
    M1 is M - 1,
    crear_fila(M1, Resto).

%% Ejercicio 2
%% ocupar(+Pos,?Tablero) será verdadero cuando la posición indicada esté ocupada.

ocupar(pos(F,C), Tablero) :- 
    indicesValidos(F,C,Tablero),
    nth0(F, Tablero, Fila),
    nth0(C, Fila, ocupada).  

% indicesValidos(+F, +C, +Tablero) es verdadero si (F,C) es válido en Tablero
indicesValidos(F, C, Tablero) :-
    length(Tablero, N),
    F >= 0,
    C >= 0,
    F < N,
    nth0(F, Tablero, Fila),
    length(Fila, M),
    C < M.

%% Ejercicio 3
%% vecino(+Pos, +Tablero, -PosVecino) será verdadero cuando PosVecino sea
%% un átomo de la forma pos(F', C') y pos(F',C') sea una celda contigua a
%% pos(F,C), donde Pos=pos(F,C). Las celdas contiguas puede ser a lo sumo cuatro
%% dado que el robot se moverá en forma ortogonal.
% N es el length del tablero 

% derecha
vecino(pos(F, C), Tablero, pos(F1, C)) :- 
    F1 is F + 1,
    length(Tablero, N),
    F1 < N.
% abajo
vecino(pos(F, C), Tablero, pos(F, C1)) :- 
    C1 is C + 1,
    length(Tablero, N),
    C1 < N.
% izquierda
vecino(pos(F, C), _, pos(F1, C)) :-
    F1 is F - 1,
    F1 >= 0.
% arriba
vecino(pos(F, C), _, pos(F, C1)) :-
    C1 is C - 1,
    C1 >= 0.

%% Ejercicio 4
%% vecinoLibre(+Pos, +Tablero, -PosVecino) idem vecino/3 pero además PosVecino
%% debe ser una celda transitable (no ocupada) en el Tablero
vecinoLibre(pos(F, C), T, pos(F1,C1)) :-
    vecino(pos(F, C), T, pos(F1, C1)),
    estaLibre(pos(F1, C1), T).

% estaLibre(+Pos, +Tablero) es verdadero si la celda en Pos esta libre
estaLibre(pos(F, C), T) :-
    nth0(F, T, Fila),
    nth0(C, Fila, Celda),
    var(Celda). % Celda es de la forma _

%%%%%%%%%%%%%%%%%%%%%%%%
%% Definicion de caminos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 5
%% camino(+Inicio, +Fin, +Tablero, -Camino) será verdadero cuando Camino sea una lista
%% [pos(F1,C1), pos(F2,C2),..., pos(Fn,Cn)] que denoten un camino desde Inicio
%% hasta Fin pasando solo por celdas transitables.
%% Además se espera que Camino no contenga ciclos.
%% Notar que la cantidad de caminos es finita y por ende se tiene que poder recorrer
%% todas las alternativas eventualmente.
%% Consejo: Utilizar una lista auxiliar con las posiciones visitadas

camino(Inicio, Fin, Tablero, Camino) :-
    estaLibre(Inicio, Tablero),
    % estaLibre(Fin, Tablero), esto permite que Fin sea reversible ya que nos aseguramos que esta libre en caminoAux
    caminoAux(Inicio, Fin, Tablero, [Inicio], Camino).

% caminoAux(+Actual, +Fin, +Tablero, +Visitados, -Camino)
% caminoAux es exitoso si el Camino llega a Fin mediante celdas transitables
caminoAux(Fin, Fin, _, Visitados, Camino) :-
    reverse(Visitados, Camino).

caminoAux(Actual, Fin, Tablero, Visitados, Camino):-
    vecinoLibre(Actual, Tablero, Siguiente), % en alguna iteracion Siguiente va a ser Fin ->> Fin esta libre
    not(member(Siguiente, Visitados)),
    caminoAux(Siguiente, Fin, Tablero, [Siguiente|Visitados], Camino). 

%% 5.1. Analizar la reversibilidad de los parámetros Fin y Camino justificando adecuadamente en cada
%% caso por qué el predicado se comporta como lo hace

% Fin es reversible. Esto se debe a la manera en que caminoAux/5 explora el tablero, verificando celdas libres y continua exploarndo hasta llegar a 'Fin'
% Si agregasemos la restriccion de que Fin debe estar libre (en camino/4), Fin no seria reversible, pues estaLibre/2 no permite que el primer parametro no este instanciado.

% Camino no es reversible por como esta armado caminoAux/5. Se deberia agregar un predicado que verifique que el camino dado es valido (que no use posiciones ocupadas y que no tenga ciclos)
% Camino se construye y luego se devuelve, el predicado no verifica su correctitud.

%% Ejercicio 6
%% camino2(+Inicio, +Fin, +Tablero, -Camino) ídem camino/4 pero que las soluciones
%% se instancien en orden creciente de longitud.

camino2(Inicio, Fin, Tablero, Camino) :-
    bfs([[Inicio]], Fin, Tablero, Camino).

% bfs(+Cola, +Fin, +Tablero, -Camino)
% tablero(3,3, T), bfs([[pos(2,1)]], pos(2,1), T, C)
bfs([[Fin|Visitados]|_], Fin, _, Camino) :- reverse([Fin|Visitados], Camino).
bfs([Visitados|Cola], Fin, Tablero, Camino) :-
    Visitados = [Actual|_],
    findall([Vecino|Visitados],
            (vecinoLibre(Actual, Tablero, Vecino), \+ member(Vecino, Visitados)),
            NuevosCaminos),
    append(Cola, NuevosCaminos, NuevaCola),
    bfs(NuevaCola, Fin, Tablero, Camino).

%% 6.1. Analizar la reversibilidad de los parámetros Inicio y Camino justificando adecuadamente en
%% cada caso por qué el predicado se comporta como lo hace.

% Vemos que Inicio no es reversible en camino2/4. Esto se debe a que el predicado bfs/4 utiliza vecinoLibre, que usa vecino.
% vecino necesita que la posicion actual sea instanciada, por lo que Inicio debe estar instanciado. 
% Entonces, Inicio no es reversible en camino2/4

% Vemos que Camino es reversible en camino2/4. Esto se debe a que actua como un verificador de si el camino es valido.
% Si el camino es valido, la funcion tiene exito al generar un camino valido por bfs/4.
% Si el camino no es valido, la funcion devuelve false, al comparar con todos los caminos validos y no encontrar uno que sea igual.



%% Ejercicio 7
%% caminoOptimo(+Inicio, +Fin, +Tablero, -Camino) será verdadero cuando Camino sea un
%% camino óptimo sobre Tablero entre Inicio y Fin. Notar que puede no ser único.
caminoOptimo(Inicio,Fin,Tablero,C1) :- 
    camino(Inicio, Fin, Tablero, C1), % Generate
    length(C1, L1), % Test
    not((caminoConLong(Inicio, Fin, Tablero, _, L2), L2<L1)). % Test

% caminoConLong(+Inicio, +Fin, +Tablero, -Camino, -L) es verdadero si Camino es un camino con longitud L 
caminoConLong(Inicio, Fin, Tablero, C2, L2) :- 
    camino(Inicio, Fin, Tablero, C2), % Generate
    length(C2, L2). % Test

%%%%%%%%%%%%%%%%%%%%%%%%
%% Tableros simultáneos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 8
%% caminoDual(+Inicio, +Fin, +Tablero1, +Tablero2, -Camino) será verdadero
%% cuando Camino sea un camino desde Inicio hasta Fin pasando al mismo tiempo
%% sólo por celdas transitables de ambos tableros.
%% caminoDual(+Inicio, +Fin, +Tablero1, +Tablero2, -Camino) 
%% Camino es un camino desde Inicio hasta Fin pasando sólo por celdas transitables en ambos tableros.

caminoDual(Inicio, Fin, Tablero1, Tablero2, Camino) :-
    % chequeo de dimensiones
    caminoDual_aux(Inicio, Fin, Tablero1, Tablero2, [Inicio], Camino).

% caminoDual_aux(+Actual, +Fin, +Tablero1, +Tablero2, +Visitados, -Camino)
% llegue al final
caminoDual_aux(Fin, Fin, _, _, Visitados, Camino) :- reverse(Visitados, Camino).
% sigo buscando
caminoDual_aux(Actual, Fin, T1, T2, Visitados, Camino):-
    vecinoLibre(Actual, T1, Siguiente), % en alguna iteracion Siguiente va a ser Fin ->> Fin esta libre
    estaLibre(Siguiente, T2),
    not(member(Siguiente, Visitados)),
    caminoDual_aux(Siguiente, Fin, T1, T2, [Siguiente|Visitados], Camino).


%%%%%%%%
%% TESTS
%%%%%%%%

cantidadTestsTablero(8). 
testTablero(1) :- tablero(0,0,[]).

% crear_fila
testTablero(2) :- crear_fila(3, [_,_,_]).
testTablero(3) :- not(crear_fila(0, [_,_,_])).

% ocupar
testTablero(4) :- ocupar(pos(0,0), [[ocupada]]).
testTablero(5) :- not(ocupar(pos(2,1),[[_,_],[_,_]])).
testTablero(6) :- tablero(2,2,T), ocupar(pos(0,0), T), T = [[ocupada,_],[_,_]].

% indicesValidos
testTablero(7) :- indicesValidos(0, 0, [[_,_]]).
testTablero(8) :- not(indicesValidos(1, 1, [[_,_]])).

cantidadTestsVecino(8).
% vecino
testVecino(1) :- tablero(2,3, T), vecino(pos(0,0), T, pos(0,1)), T = [[_,_,_],[_,_,_]].
testVecino(2) :- tablero(2,3, T), vecino(pos(0,0), T, pos(1,0)), T = [[_,_,_],[_,_,_]].
testVecino(3) :- tablero(2,3, T), vecino(pos(0,0), T, pos(0,1)), T = [[_,_,_],[_,_,_]].
testVecino(4) :- tablero(2,3, _), not(vecino(pos(1,1), _, pos(0,0))). 
% vecinoLibre
testVecino(5) :- tablero(2,3, T), vecinoLibre(pos(0,0), T, _).
testVecino(6) :- tablero(2,3, T), ocupar(pos(1,0), T), vecinoLibre(pos(0,0), T, _).
testVecino(7) :- tablero(2,3, T), ocupar(pos(0,1), T), ocupar(pos(1,0), T), not(vecinoLibre(pos(0,0), T, _)).
testVecino(8) :- not((tablero(2,3, T), ocupar(pos(1,0), T), vecinoLibre(pos(0,0), T, pos(1,0)))). 

cantidadTestsCamino(3).
% camino
testCamino(1) :- tablero(2,2,T), camino(pos(0,0), pos(1,1), T, [pos(0,0), pos(0,1), pos(1,1)]).
testCamino(2) :- tablero(2,2,T), camino(pos(0,0), pos(1,1), T, [pos(0,0), pos(1,0), pos(1,1)]).
testCamino(3) :- tablero(2,2,T), ocupar(pos(1,0), T), ocupar(pos(0,1), T), not(camino(pos(0,0), pos(1,1), T, _)).

cantidadTestsCaminoAux(5).

% caminoAux
testCaminoAux(1) :- tablero(2, 2, T), caminoAux(pos(0, 0), pos(1, 1), T, [pos(0, 0)], [pos(0, 0), pos(0, 1), pos(1, 1)]).
testCaminoAux(2) :- tablero(2, 2, T), caminoAux(pos(0, 0), pos(1, 1), T, [pos(0, 0)], [pos(0, 0), pos(1, 0), pos(1, 1)]).
testCaminoAux(3) :- tablero(2, 2, T), ocupar(pos(1, 0), T), ocupar(pos(0, 1), T), not(caminoAux(pos(0, 0), pos(1, 1), T, [pos(0, 0)], _)).
testCaminoAux(4) :- tablero(3, 3, T), ocupar(pos(1, 1), T), caminoAux(pos(0, 0), pos(2, 2), T, [pos(0, 0)], [pos(0, 0), pos(0, 1), pos(0, 2), pos(1, 2), pos(2, 2)]).
testCaminoAux(5) :- tablero(3, 3, T), ocupar(pos(1, 1), T), not(caminoAux(pos(0, 0), pos(2, 2), T, [pos(0, 0)], [pos(0, 0), pos(1, 0), pos(1, 1), pos(2, 2)])).

cantidadTestsBFS(4).
% bfs
testBFS(1) :- tablero(3,3,T), bfs([[pos(0,0)]], pos(2,2), T, [pos(0,0), pos(0,1), pos(0,2), pos(1,2), pos(2,2)]).
testBFS(2) :- tablero(3,3,T), ocupar(pos(1,1), T), bfs([[pos(0,0)]], pos(2,2), T, [pos(0,0), pos(0,1), pos(0,2), pos(1,2), pos(2,2)]).
testBFS(3) :- tablero(3,3,T), bfs([[pos(0,0)]], pos(0,0), T, [pos(0,0)]).
testBFS(4) :- tablero(3,3,T), ocupar(pos(1,1), T), ocupar(pos(0,1), T), ocupar(pos(1,0), T), not(bfs([[pos(0,0)]], pos(2,2), T, _)).

cantidadTestsCamino2(2).
% camino2
testCamino2(1) :- tablero(3,3,T), camino2(pos(0,0), pos(2,2), T, [pos(0,0), pos(0,1), pos(0,2), pos(1,2), pos(2,2)]).
testCamino2(2) :- tablero(3,3,T), ocupar(pos(1,1), T), camino2(pos(0,0), pos(2,2), T, [pos(0,0), pos(0,1), pos(0,2), pos(1,2), pos(2,2)]).

cantidadTestsCaminoOptimo(6).
% caminoOptimo
testCaminoOptimo(1) :- tablero(3,2,T), caminoOptimo(pos(0,0), pos(1,1), T, [pos(0,0), pos(0,1), pos(1,1)]).
testCaminoOptimo(2) :- tablero(3,2,T), caminoOptimo(pos(0,0), pos(1,1), T, [pos(0,0), pos(1,0), pos(1,1)]).
testCaminoOptimo(3) :- tablero(3,2,T), ocupar(pos(1,0), T), not(caminoOptimo(pos(0,0), pos(1,1), T, [pos(0,0), pos(1,0), pos(1,1)])).
testCaminoOptimo(4) :- tablero(3,2,T), ocupar(pos(1,0), T), caminoOptimo(pos(0,0), pos(1,1), T, [pos(0,0), pos(0,1), pos(1,1)]).

% caminoConLong
testCaminoOptimo(5) :- tablero(3,2,T), caminoConLong(pos(0,0), pos(1,1), T, [pos(0,0), pos(0,1), pos(1,1)], 3).
testCaminoOptimo(6) :- tablero(3,2,T), not(caminoConLong(pos(0,0), pos(1,1), T, [pos(0,0), pos(1,0), pos(1,1)], 4)).

cantidadTestsCaminoDual(3). 
testCaminoDual(1) :- tablero(3,2,T1), tablero(3,2,T2), caminoDual(pos(0,0), pos(1,1), T1, T2, [pos(0,0), pos(0,1), pos(1,1)]). % Camino correcto
testCaminoDual(2) :- tablero(3,2,T1), tablero(3,2,T2), ocupar(pos(1,0), T1), ocupar(pos(0,1), T2), not(caminoDual(pos(0,0), pos(1,1), T1, T2, _)). % Distintos caminos
testCaminoDual(3) :- tablero(3,2,T1), tablero(3,2,T2), ocupar(pos(1,0), T2), ocupar(pos(0,1), T2), not(caminoDual(pos(0,0), pos(1,1), T1, T2, _)). % T2 no tiene camino


cantidadTestsCaminoDualAux(4).
testCaminoDualAux(1) :- tablero(3,3,T1), tablero(3,3,T2), caminoDual_aux(pos(0,0), pos(2,2), T1, T2, [pos(0,0)], [pos(0,0), pos(0,1), pos(0,2), pos(1,2), pos(2,2)]).
testCaminoDualAux(2) :- tablero(3,3,T1), tablero(3,3,T2), ocupar(pos(1,1), T1), ocupar(pos(1,1), T2), caminoDual_aux(pos(0,0), pos(2,2), T1, T2, [pos(0,0)], [pos(0,0), pos(0,1), pos(0,2), pos(1,2), pos(2,2)]).
testCaminoDualAux(3) :- tablero(3,3,T1), tablero(3,3,T2), caminoDual_aux(pos(0,0), pos(0,0), T1, T2, [pos(0,0)], [pos(0,0)]).
testCaminoDualAux(4) :- tablero(3,3,T1), tablero(3,3,T2), ocupar(pos(1,1), T1), ocupar(pos(0,1), T2), ocupar(pos(1,0), T2), not(caminoDual_aux(pos(0,0), pos(2,2), T1, T2, [pos(0,0)], _)).

tests(bfs) :- cantidadTestsBFS(M), forall(between(1,M,N), testBFS(N)).
tests(camino2) :- cantidadTestsCamino2(M), forall(between(1,M,N), testCamino2(N)).
tests(caminoAux) :- cantidadTestsCaminoAux(M), forall(between(1, M, N), testCaminoAux(N)).
tests(caminoDualAux) :- cantidadTestsCaminoDualAux(M), forall(between(1,M,N), testCaminoDualAux(N)).
tests(tablero) :- cantidadTestsTablero(M), forall(between(1,M,N), testTablero(N)).
tests(vecino) :- cantidadTestsVecino(M), forall(between(1,M,N), testVecino(N)).
tests(camino) :- cantidadTestsCamino(M), forall(between(1,M,N), testCamino(N)).
tests(caminoOptimo) :- cantidadTestsCaminoOptimo(M), forall(between(1,M,N), testCaminoOptimo(N)).
tests(caminoDual) :- cantidadTestsCaminoDual(M), forall(between(1,M,N), testCaminoDual(N)).

tests(todos) :-
  tests(tablero),
  tests(vecino),
  tests(camino),
  tests(caminoOptimo),
  tests(caminoDual),
  tests(caminoDualAux),
  tests(bfs),
  tests(camino2),
  tests(caminoAux).


tests :- tests(todos).
