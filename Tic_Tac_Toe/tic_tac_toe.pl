% Cambia los jugadores, X pasa a ser O y viceversa
voltear(N1, N2) :- N2 is N1*(-1).

% Tablero inicial, útil para pruebas
tablero_inicial([0,0,0,0,0,0,0,0,0]).

% Todas las posiciones ganadoras para el tic-tac-toe
posicion_ganadora(P, [P, P, P, _, _, _, _, _, _]).
posicion_ganadora(P, [_, _, _, P, P, P, _, _, _]).
posicion_ganadora(P, [_, _, _, _, _, _, P, P, P]).
posicion_ganadora(P, [P, _, _, P, _, _, P, _, _]).
posicion_ganadora(P, [_, P, _, _, P, _, _, P, _]).
posicion_ganadora(P, [_, _, P, _, _, P, _, _, P]).
posicion_ganadora(P, [P, _, _, _, P, _, _, _, P]).
posicion_ganadora(P, [_, _, P, _, P, _, P, _, _]).

% Es verdadero si no hay 0s (valores vacíos) en el tablero
esta_lleno(Tablero) :- \+ member(0, Tablero).

% Es verdadero si el juego está en un estado final para el jugador P
fin_juego(Tablero, P, P) :- posicion_ganadora(P, Tablero), !.
fin_juego(Tablero, _, 0) :- esta_lleno(Tablero).

% Coloca una ficha (P) en el espacio correspondiente
% Es falso si el movimiento es inválido
mover([0,B,C,D,E,F,G,H,I], P, 0, [P,B,C,D,E,F,G,H,I]).
mover([A,0,C,D,E,F,G,H,I], P, 1, [A,P,C,D,E,F,G,H,I]).
mover([A,B,0,D,E,F,G,H,I], P, 2, [A,B,P,D,E,F,G,H,I]).
mover([A,B,C,0,E,F,G,H,I], P, 3, [A,B,C,P,E,F,G,H,I]).
mover([A,B,C,D,0,F,G,H,I], P, 4, [A,B,C,D,P,F,G,H,I]).
mover([A,B,C,D,E,0,G,H,I], P, 5, [A,B,C,D,E,P,G,H,I]).
mover([A,B,C,D,E,F,0,H,I], P, 6, [A,B,C,D,E,F,P,H,I]).
mover([A,B,C,D,E,F,G,0,I], P, 7, [A,B,C,D,E,F,G,P,I]).
mover([A,B,C,D,E,F,G,H,0], P, 8, [A,B,C,D,E,F,G,H,P]).

% Crea una lista de todos los movimientos posibles
movimientos_posibles(Tablero, Movimientos) :- movimientos_posibles_aux(Tablero, Movimientos, 0).

movimientos_posibles_aux([], [], _).
movimientos_posibles_aux([0|TP], [X|TM], X) :- 
    X2 is X + 1,
    movimientos_posibles_aux(TP, TM, X2), !.
movimientos_posibles_aux([_|TP], TM, X) :- 
    X2 is X + 1,
    movimientos_posibles_aux(TP, TM, X2), !.

% Función del jugador para extraer el mejor movimiento del minimax
jugador_minimax(Tablero, P, Movimiento) :-
    minimax(Tablero, P, Movimiento-_).

% Utiliza el algoritmo básico de minimax para devolver el mejor movimiento-valor
% de los movimientos posibles para el jugador P
minimax(Tablero, P, M-V) :-
    movimientos_posibles(Tablero, Movimientos),
    argmax(Tablero, P, Movimientos, M-V).

% Devuelve el par de movimiento-valor máximo dado una lista de movimientos
argmax(Tablero, P, [M], M-V) :- 
    valor_movimiento(Tablero, P, M, V).
argmax(Tablero, P, [M|TM], NuevoM-NuevoV) :-
    argmax(Tablero, P, TM, AnteriorM-AnteriorV),
    valor_movimiento(Tablero, P, M, V),
    mejor_movimiento(M,V,AnteriorM,AnteriorV,NuevoM,NuevoV).
    


% Evalúa el valor de un movimiento dado la posición actual del tablero
valor_movimiento(Tablero, P, Movimiento, Valor) :- 
    mover(Tablero, P, Movimiento, NuevoTablero),
    valor_tablero(NuevoTablero, P, Valor).

% Función auxiliar para valor_movimiento, devuelve el negamax de la siguiente búsqueda 
% si la posición actual del tablero no es un estado terminal
valor_tablero(Tablero, P, Valor) :-
    fin_juego(Tablero, P, Valor2),
    Valor is Valor2*P.
valor_tablero(Tablero, P, Valor) :-
    voltear(P, ProximoP),
    minimax(Tablero, ProximoP, _-Valor2),
    voltear(Valor2, Valor), !.

% Función auxiliar para arg_max, toma dos pares y devuelve el que tiene el valor mayor. 
mejor_movimiento(M1,V1,_,V2,M1,V1) :- V1 >= V2.
mejor_movimiento(_,V1,M2,V2,M2,V2) :- V1 < V2.


% Muestra el tablero de manera regular en formato tic-tac-toe
% Prueba con: mostrar_tablero([0,1,1,-1,-1,0,1,0,-1]).
mostrar_tablero([A,B,C,D,E,F,G,H,I]) :- 
    nl,
    write(' '), mostrar(A), write(' | '), 
    mostrar(B), write(' | '), mostrar(C), nl,
    write('-----------'), nl,
    write(' '), mostrar(D), write(' | '), 
    mostrar(E), write(' | '), mostrar(F), nl,
    write('-----------'), nl,
    write(' '), mostrar(G), write(' | '), 
    mostrar(H), write(' | '), mostrar(I), nl.


% Muestra la casilla individual del tic-tac-toe
mostrar(0) :- write(' '), !.
mostrar(1) :- write('X'), !.
mostrar(-1) :- write('O').

% Pide al usuario que ingrese un movimiento
pedir(N) :- 
    mostrar_movimientos, 
    read(N).

% Muestra todos los movimientos
mostrar_movimientos :-     
    write('Selecciona un número de 0 a 8 inclusive. '),
    write('Termina tu elección con un punto ').

% Pide al jugador que seleccione si quiere mover primero o segundo
iniciar :-
    write('Selecciona [0] para elegir X, o [1] para elegir O. '),
    write('Termina tu elección con un punto: '), 
    read(N), 
    (
        N == end_of_file    -> halt(0) ;
        N = 0               -> jugar([0,0,0,0,0,0,0,0,0], humano, 1) ; 
        N = 1               -> jugar([0,0,0,0,0,0,0,0,0], computadora, 1) ;
        % sino
        write('Elección inválida'), nl, iniciar
    ).

% Pide al jugador (ya sea humano o minimax) que seleccione un movimiento 
% y actualiza el estado del juego de acuerdo con ello, luego llama a jugar para el siguiente jugador
jugar(Tablero, humano, P) :- 
    mostrar_tablero(Tablero), 
    pedir(N),
    (N == end_of_file -> halt(0) ;
    mover(Tablero, P, N, Tablero_actualizado) ->
        ((fin_juego(Tablero_actualizado, P, V) ->  % el movimiento es legal
            mostrar_tablero(Tablero_actualizado), !, fin_juego(V) ;
            voltear(P, Q)), jugar(Tablero_actualizado, computadora, Q)) ;
        write('Movimiento inválido'), nl,          % el movimiento no es legal
            jugar(Tablero, humano, P)).

jugar(Tablero, computadora, P) :- 
    jugador_minimax(Tablero, P, Movimiento_elegido), 
    mover(Tablero, P, Movimiento_elegido, Tablero_actualizado), 
    write('La computadora hizo un movimiento'), nl, 
    (fin_juego(Tablero_actualizado, P, V) -> 
        mostrar_tablero(Tablero_actualizado), fin_juego(V) ;
        voltear(P, Q), jugar(Tablero_actualizado, humano, Q)).

% Muestra el mensaje de fin de juego y pregunta si el usuario quiere jugar de nuevo
fin_juego(V) :-
    mensaje_ganador(V), nl, nl,
    fin_juego_2.

mensaje_ganador(1) :- write('¡X ganó!'), !.
mensaje_ganador(-1) :- write('¡O ganó!'), !.
mensaje_ganador(0) :- write('¡Empate!').

fin_juego_2 :-
    write('¿Quieres jugar de nuevo?'), nl,
    write('Selecciona [1] para Sí o [0] para No '),
    read(M),
    (
        M == end_of_file -> halt(0) ; 
        M = 1 -> iniciar ;
        M = 0 -> write('¡Adiós!'), nl ;
        % sino
        write('Elección inválida'), nl, nl, fin_juego_2
    ). 