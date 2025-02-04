


obtenerProblema(H, W, Filas,Salida) :-
    trasponer(Filas,Columnas), obtenerCajas(Filas,W,H,Cajas), append(Filas,Columnas,Temp), append(Temp,Cajas,Salida).


resolverPieza([], _, _, _) :- !.
resolverPieza([Item|Resto], Dominio, Lista, Problema) :- 
    nonvar(Item), resolverPieza(Resto, Dominio, Lista, Problema).
resolverPieza([VariableActual|Variables], Dominio, Lista, Problema) :- 
        select(PunteroActual, Dominio, NuevoDominio),   
        VariableActual = PunteroActual,
        comprobarConsistencia(Problema),
        resolverPieza(Variables, NuevoDominio, Lista, Problema).
resolverPieza(Variables,Dominio, Problema) :-
    reducirDominio(Variables,Dominio,DominioReducido), 
    resolverPieza(Variables, DominioReducido, Variables, Problema).
obtenerSolucion([], _).
obtenerSolucion([H|T],Dominio) :- resolverPieza(H,Dominio,[H|T]), obtenerSolucion(T,Dominio).




imprimirLinea(_,[]) :- writeln('|').

imprimirLinea(Ancho, [H|T]) :-
    (
        (length([H|T], ItemsRestantes),
        Mod is ItemsRestantes mod Ancho,
        Mod = 0,
        write('| ')); 
        true
    ),
    (
        (var(H), write('_ '));
        (nonvar(H), write(H), write(' '))
    ),
    imprimirLinea(Ancho, T),!.

imprimirSudoku([],_,_).

imprimirSudoku([PrimeraFila|Resto],Ancho,Alto) :-
    (
        (length([PrimeraFila|Resto], LineasRestantes),
        Mod is LineasRestantes mod Alto,
        Mod = 0,
        writeln('')); 
        true
    ),
    imprimirLinea(Ancho, PrimeraFila),
    imprimirSudoku(Resto,Ancho,Alto), !.


resolverSudoku(H,W,Sudoku) :-
    obtenerProblema(H,W,Sudoku,Problema), 
    Nums is H*W, 
    numlist(1,Nums,Dominio),

    random_permutation(Dominio,DominioMezclado),

    obtenerSolucion(Problema,DominioMezclado),
    writeln('Solución: '),
    imprimirSudoku(Sudoku, W, H).

generarSudoku(AnchoTablero, AltoTablero, AnchoCaja, AltoCaja, Dificultad, Salida) :-
    obtenerTablero(AltoTablero, AnchoTablero, Sudoku),
    resolverSudoku(AnchoCaja, AltoCaja, Sudoku),
    maplist(enmascararLinea(Dificultad), Sudoku, Salida), !,
    writeln('Sudoku: '),
    imprimirSudoku(Salida, AltoCaja, AnchoCaja).
