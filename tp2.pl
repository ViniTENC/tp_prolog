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
tablero(F, C, [Fila|Resto]) :-
    F > 0,
    C > 0,
    F1 is F - 1,
    crear_fila(C, Fila),
    tablero(F1, C, Resto).

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
vecino(pos(F, C), Tablero, pos(F1, C)) :- F1 is F + 1,
    length(Tablero, N),
    F1 < N.
% abajo
vecino(pos(F, C), Tablero, pos(F, C1)) :- C1 is C + 1,
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

% caminoAux(Actual, Fin, Tablero, Visitados, Camino)
% caminoAux es exitoso si el Camino llega a Fin mediante celdas transitables
caminoAux(Fin, Fin, Tablero, Visitados, Camino) :- reverse(Visitados, Camino).
caminoAux(Actual, Fin, Tablero, Visitados, Camino):-
    vecinoLibre(Actual, Tablero, Siguiente), % en alguna iteracion Siguiente va a ser Fin ->> Fin esta libre
    not(member(Siguiente, Visitados)),
    caminoAux(Siguiente, Fin, Tablero, [Siguiente|Visitados], Camino). 

%% 5.1. Analizar la reversibilidad de los parámetros Fin y Camino justificando adecuadamente en cada
%% caso por qué el predicado se comporta como lo hace

% Fin es reversible, podemos dejarla sin instanciar y encuentra los caminos desde cada inicio. 
% Camino parece no ser reversible (por lo que probamos en la terminal), esta raro hay que chequear :p xd

%% Ejercicio 6
%% camino2(+Inicio, +Fin, +Tablero, -Camino) ídem camino/4 pero que las soluciones
%% se instancien en orden creciente de longitud.

% generarCaminosOrdenados(Inicio, Fin, Tablero, CaminosOrdenados) :-
%     camino(Inicio, Fin, Tablero, Camino), % Camino es un camino valido
%     member(Camino, CaminosOrdenados),     % Esta en CaminosOrdenados
%     not(not(camino(Inicio, Fin, Tablero, CN), member(CN, CaminosOrdenados))) % No hay elementos que no sean caminos en CaminosOrdenados
%     ordenadosPorLongitud(caminosOrdenados). % CaminosOrdenados esta ordenado por longitud

% ordenadosPorLongitud([]).
% ordenadosPorLongitud([C|Css]) :- 
%     length(C, L),
%     not((member(C2, Css), length(C2, L2), L2 < L)),
%     ordenadosPorLongitud(Css).

% % Generate & Test
% camino2(Inicio, Fin, Tablero, Camino) :-
%     generarCaminosOrdenados(Inicio, Fin, Tablero, [], CaminosOrdenados),
%     member(Camino, CaminosOrdenados).

camino5(Inicio, Fin, Tablero, Camino):- 
    length(Tablero, N),
    M is N*N,
    between(0, M, L), 
    setof(C, caminoDeLong(Inicio, Fin, Tablero, L, C), Camino).

caminoDeLong(Inicio, Fin, Tablero, L, Camino) :- camino(Inicio, Fin, Tablero, Camino), length(C, L).

% CAMINO 3 FUNCIONA PERO USA FINDALL, MEDIO CHEAT POR AHI
camino3(Inicio, Fin, Tablero, Camino) :-
    bfs([[Inicio]], Fin, Tablero, Camino).

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
%% caminoDual(+Inicio, +Fin, +Tablero1, +Tablero2, -Camino) 
%% Camino es un camino desde Inicio hasta Fin pasando sólo por celdas transitables en ambos tableros.

caminoDual(Inicio, Fin, Tablero1, Tablero2, Camino) :-
    % chequeo de dimensiones
    caminoDual_aux(Inicio, Fin, Tablero1, Tablero2, [Inicio], Camino).

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