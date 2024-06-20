%%%%%%%%%%%%%%%%%%%%%%%%
%% Tablero
%%%%%%%%%%%%%%%%%%%%%%%%

tablero(ej5x5, T) :-
tablero(5, 5, T),
ocupar(pos(1, 1), T),
ocupar(pos(1, 2), T).
tablero(libre20, T) :-
tablero(20, 20, T).

%% Ejercicio 1
%% tablero(+Filas,+Columnas,-Tablero) instancia una estructura de tablero en blanco
%% de Filas x Columnas, con todas las celdas libres.

tablero(0, _, []).
tablero(_, 0, []).
tablero(N, M, [Fila|Resto]) :-
    N > 0,
    M > 0,
    N1 is N - 1,
    crear_fila(M, Fila),
    tablero(N1, M, Resto).

crear_fila(0, []).
crear_fila(M, [_|Resto]) :-
    M > 0,
    M1 is M - 1,
    crear_fila(M1, Resto).
%% Ejercicio 2
%% ocupar(+Pos,?Tablero) será verdadero cuando la posición indicada esté ocupada.
pos(F, C).
ocupar(pos(F, C), Tablero) :-
    nth0(F, Tablero, Fila),
    nth0(C, Fila, ocupada).
nth0(0, [X|_], X).
nth0(N, [_|Xs], Y) :-
    N > 0,
    N1 is N - 1,
    nth0(N1, Xs, Y).

%% Ejercicio 3
%% vecino(+Pos, +Tablero, -PosVecino) será verdadero cuando PosVecino sea
%% un átomo de la forma pos(F', C') y pos(F',C') sea una celda contigua a
%% pos(F,C), donde Pos=pos(F,C). Las celdas contiguas puede ser a lo sumo cuatro
%% dado que el robot se moverá en forma ortogonal.
% N es el length del tablero 
vecino(pos(F, C), Tablero, pos(F1, C)) :-
    F1 is F + 1,
    length(Tablero, N),
    F1 < N.
vecino(pos(F, C), Tablero, pos(F, C1)) :-
    C1 is C + 1,
    length(Tablero, N),
    C1 < N.
vecino(pos(F, C), Tablero, pos(F1, C)) :-
    F1 is F - 1,
    F1 >= 0.
vecino(pos(F, C), Tablero, pos(F, C1)) :-
    C1 is C - 1,
    C1 >= 0
.

%% Ejercicio 4
%% vecinoLibre(+Pos, +Tablero, -PosVecino) idem vecino/3 pero además PosVecino
%% debe ser una celda transitable (no ocupada) en el Tablero
vecinoLibre(pos(F, C), T, pos(F1,C1)) :-
    vecino(pos(F, C), T, pos(F1, C1)),
    estaLibre(pos(F1, C1), T).


estaLibre(pos(F, C), T) :-
    nth0(F, T, Fila),
    nth0(C, Fila, Celda),
    var(Celda).

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
    % [Inicio] es la lista de posiciones visitadas
    % agregar check de si Inicio/Fin estan ocupados; si lo estan, no hay camino
    estaLibre(Inicio, Tablero),
    caminoAux(Inicio, Fin, Tablero, [Inicio], Camino).

caminoAux(Fin, Fin, Tablero, Visitados, Camino) :- reverse(Visitados, Camino).

caminoAux(Actual, Fin, Tablero, Visitados, Camino):-
    vecinoLibre(Actual, Tablero, Siguiente),
    not(member(Siguiente, Visitados)),
    caminoAux(Siguiente, Fin, Tablero, [Siguiente|Visitados], Camino).


%% 5.1. Analizar la reversibilidad de los parámetros Fin y Camino justificando adecuadamente en cada
%% caso por qué el predicado se comporta como lo hace

% Fin es reversible, podemos dejarla sin instanciar y encuentra los caminos desde cada inicio.
% Camino parece no ser reversible (por lo que probamos en la terminal), esta raro hay que chequear :p xd

%% Ejercicio 6
%% camino2(+Inicio, +Fin, +Tablero, -Camino) ídem camino/4 pero que las soluciones
%% se instancien en orden creciente de longitud.

% listaDeCaminos(Inicio,Fin,Tablero,[Caminos]

caminoMasCorto(Inicio,Fin,Tablero,C1) :- camino(Inicio, Fin, Tablero, C1), length(C1, L1),
                                            not((caminoConLong(Inicio, Fin, Tablero, C2, L2), L2<L1)).
caminoConLong(Inicio, Fin, Tablero, C2, L2) :- camino(Inicio, Fin, Tablero, C2), length(C2, L2).


camino2(Inicio, Fin, Tablero, Camino) :-
    setof(C, caminoMasCorto(Inicio, Fin, Tablero, C), Camino)

% CAMINO 3 FUNCIONA PERO USA FINDALL, MEDIO CHEAT POR AHI
camino3(Inicio, Fin, Tablero, Camino) :-
    bfs([[Inicio]], Fin, Tablero, Camino).

bfs([[Fin|Visitados]|_], Fin, _, Camino) :-
    reverse([Fin|Visitados], Camino).

bfs([Visitados|Cola], Fin, Tablero, Camino) :-
    Visitados = [Actual|_],
    findall([Vecino|Visitados],
            (vecinoLibre(Actual, Tablero, Vecino), \+ member(Vecino, Visitados)),
            NuevosCaminos),
    append(Cola, NuevosCaminos, NuevaCola),
    bfs(NuevaCola, Fin, Tablero, Camino).
    
%%%%
camino4(Inicio, Fin, Tablero, Camino) :-
    bfs([[Inicio]], Fin, Tablero, [], Camino).

bfs([[Fin|Visitados]|_], _, _, _, Camino) :-
    reverse([Fin|Visitados], Camino).

bfs([Visitados|Cola], Fin, Tablero, VisitadosPrevios, Camino) :-
    Visitados = [Actual|_],
    setof([Vecino|Visitados],
          (vecinoLibre(Actual, Tablero, Vecino),
           \+ member(Vecino, Visitados),
           \+ member([Vecino|Visitados], VisitadosPrevios)),
          NuevosCaminos),
    append(VisitadosPrevios, NuevosCaminos, NuevosVisitadosPrevios),
    append(Cola, NuevosCaminos, NuevaCola),
    bfs(NuevaCola, Fin, Tablero, NuevosVisitadosPrevios, Camino).

bfs([Visitados|Cola], Fin, Tablero, VisitadosPrevios, Camino) :-
    Visitados = [Actual|_],
    Actual \= Fin,
    bfs(Cola, Fin, Tablero, VisitadosPrevios, Camino).


%% 6.1. Analizar la reversibilidad de los parámetros Inicio y Camino justificando adecuadamente en
%% cada caso por qué el predicado se comporta como lo hace.


%% Ejercicio 7
%% caminoOptimo(+Inicio, +Fin, +Tablero, -Camino) será verdadero cuando Camino sea un
%% camino óptimo sobre Tablero entre Inicio y Fin. Notar que puede no ser único.
caminoOptimo(Inicio,Fin,Tablero,C1) :- camino(Inicio, Fin, Tablero, C1), length(C1, L1),
                                            not((caminoConLong(Inicio, Fin, Tablero, C2, L2), L2<L1)).
caminoConLong(Inicio, Fin, Tablero, C2, L2) :- camino(Inicio, Fin, Tablero, C2), length(C2, L2).

%%%%%%%%%%%%%%%%%%%%%%%%
%% Tableros simultáneos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 8
%% caminoDual(+Inicio, +Fin, +Tablero1, +Tablero2, -Camino) será verdadero
%% cuando Camino sea un camino desde Inicio hasta Fin pasando al mismo tiempo
%% sólo por celdas transitables de ambos tableros.
caminoDual(_,_,_,_,_).

%%%%%%%%
%% TESTS
%%%%%%%%

cantidadTestsTablero(2). % Actualizar con la cantidad de tests que entreguen
testTablero(1) :- tablero(0,0,[]).
testTablero(2) :- ocupar(pos(0,0), [[ocupada]]).
% Agregar más tests

cantidadTestsVecino(1). % Actualizar con la cantidad de tests que entreguen
testVecino(1) :- vecino(pos(0,0), [[_,_]], pos(0,1)).
% Agregar más tests

cantidadTestsCamino(0). % Actualizar con la cantidad de tests que entreguen
% Agregar más tests

cantidadTestsCaminoOptimo(0). % Actualizar con la cantidad de tests que entreguen
% Agregar más tests

cantidadTestsCaminoDual(0). % Actualizar con la cantidad de tests que entreguen
% Agregar más tests

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
  tests(caminoDual).

tests :- tests(todos).