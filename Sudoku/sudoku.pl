removeItem(Item,List,Out) :-
    select(Item,List,Out), !.
removeItem(_,List,List).

reduceDomain(_,[],[]) :- !.
reduceDomain([],Domain,Domain) :- !.
reduceDomain([H|T],Domain,Out) :- 
    nonvar(H), removeItem(H,Domain,DomainWithoutValue), reduceDomain(T,DomainWithoutValue,Out), !.
reduceDomain([H|T],Domain,Out) :- 
    var(H), reduceDomain(T,Domain,Out).

    obfuscateLine(_, [], []) :- !.
obfuscateLine(Diff, [H|T], [H|ObfuscatedRest]) :-
    random(X), X > Diff, !, obfuscateLine(Diff,T,ObfuscatedRest).
obfuscateLine(Diff, [_|T], [_|ObfuscatedRest]) :-
    obfuscateLine(Diff,T,ObfuscatedRest).

    peelLeft([], [], []).
peelLeft([[H|R]|T], [H|RestOfColumn], [R|RestOfRests]) :- peelLeft(T,RestOfColumn,RestOfRests).


transpose([[]|_],[]) :- !.
transpose(Matrix, [H|T]) :- peelLeft(Matrix,H,RestOfMatrix), transpose(RestOfMatrix,T).


getBoxStack([],_,_,Buffer,[Buffer],[]) :- !.
getBoxStack(Sudoku,Width,Height,Buffer,[Buffer|RestOfBoxes],RemainingRows):-
    SizeOfBox is Width * Height,    
    length(Buffer,SizeOfBox),
    getBoxStack(Sudoku,Width,Height,[],RestOfBoxes,RemainingRows), !.
getBoxStack([CurrentRow|Frontier],Width,Height,OldBuffer,Boxes,[RestOfCurrentRow|RemainingRows]):-
    length(CurrentStackSlice, Width),
    append(CurrentStackSlice, RestOfCurrentRow, CurrentRow),
    append(CurrentStackSlice,OldBuffer,NewBuffer),
    getBoxStack(Frontier,Width,Height,NewBuffer,Boxes,RemainingRows).


    getBoxes_([],_,_,[]) :- !.
getBoxes_([[]|_],_,_,[]) :- !.
getBoxes_(Sudoku, Width, Height, Out) :-
    getBoxStack(Sudoku,Width,Height,[],Stack,Rest),
    append(Stack,OtherBoxes,Out),
    getBoxes_(Rest,Width,Height,OtherBoxes).
getBoxes([H|T], BoxWidth, BoxHeight, Out) :- 
        length([H|T], MatrixSize), 
        length(H, MatrixSize), 
        Y is (MatrixSize mod BoxHeight),
        X is (MatrixSize mod BoxWidth),
        X = Y, Y = 0, BoxWidth \= 0, BoxHeight \= 0,
        MatrixSize is BoxWidth * BoxHeight,
        getBoxes_([H|T], BoxWidth, BoxHeight, Out)




obtenerProblema(H, W, Filas,Salida) :-
    trasponer(Filas,Columnas), obtenerCajas(Filas,W,H,Cajas), append(Filas,Columnas,Temp), append(Temp,Cajas,Salida).

    inequal(X,Y) :- X \== Y.
allDifferent([]).
allDifferent([X|Xss]) :- maplist(inequal(X), Xss), allDifferent(Xss).


checkConsistency(Problem) :-
    maplist(allDifferent, Problem).


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
