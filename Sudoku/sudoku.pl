
% Elimina un elemento de una lista y devuelve la lista resultante.
% Si el elemento no está en la lista, la devuelve sin cambios.
removeItem(Item,List,Out) :-
    select(Item,List,Out), !.
removeItem(_,List,List).


% Reduce el dominio de valores eliminando aquellos que ya han sido usados.
% - Si la lista de variables está vacía, devuelve una lista vacía.
% - Si la lista de dominio está vacía, devuelve la misma lista de dominio.
% - Si la cabeza de la lista tiene un valor asignado, lo elimina del dominio y sigue reduciendo.
% - Si la cabeza es una variable sin asignar, sigue con la reducción sin cambios.
reduceDomain(_,[],[]) :- !.
reduceDomain([],Domain,Domain) :- !.
reduceDomain([H|T],Domain,Out) :- 
    nonvar(H), removeItem(H,Domain,DomainWithoutValue), reduceDomain(T,DomainWithoutValue,Out), !.
reduceDomain([H|T],Domain,Out) :- 
    var(H), reduceDomain(T,Domain,Out).


% Oculta algunos valores en una línea del Sudoku según un umbral de dificultad.
% - Si la lista está vacía, devuelve una lista vacía.
% - Si un número aleatorio es mayor que el umbral, conserva el valor y continúa.
% - Si es menor o igual al umbral, lo reemplaza por una variable anónima (_).
obfuscateLine(_, [], []) :- !.
obfuscateLine(Diff, [H|T], [H|ObfuscatedRest]) :-
    random(X), X > Diff, !, obfuscateLine(Diff,T,ObfuscatedRest).
obfuscateLine(Diff, [_|T], [_|ObfuscatedRest]) :-
    obfuscateLine(Diff,T,ObfuscatedRest).


% Crea una matriz de tamaño Rows x Cols representando el Sudoku.
% - Si Rows es 0, devuelve una lista vacía.
% - En cada paso, genera una fila de tamaño Cols y sigue reduciendo Rows.
getBoard(0,_,[]) :- !.
getBoard(Rows, Cols, [H|T]) :- length(H, Cols), NewRows is Rows-1, getBoard(NewRows,Cols,T).


% Separa la primera columna de una matriz y devuelve la columna y el resto
peelLeft([], [], []).
peelLeft([[H|R]|T], [H|RestOfColumn], [R|RestOfRests]) :- peelLeft(T,RestOfColumn,RestOfRests).


% Transpone una matriz (convierte filas en columnas y viceversa).
transpose([[]|_],[]) :- !.
transpose(Matrix, [H|T]) :- peelLeft(Matrix,H,RestOfMatrix), transpose(RestOfMatrix,T).


% Crea las cajas de un lado.
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


% Extrae las cajas del Sudoku organizadas en una lista de listas.
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
        getBoxes_([H|T], BoxWidth, BoxHeight, Out).



% Obtiene la representación del Sudoku en términos de filas, columnas y cajas.
% - Transpone la matriz para obtener las columnas.
% - Obtiene las cajas de la matriz.
% - Une filas, columnas y cajas en una lista que representa el problema.
getProblem(H, W, Rows,Out) :-
    transpose(Rows,Cols), getBoxes(Rows,W,H,Boxes), append(Rows,Cols,Temp), append(Temp,Boxes,Out).


% Verifica que todos los elementos de una lista sean diferentes.
inequal(X,Y) :- X \== Y.
allDifferent([]).
allDifferent([X|Xss]) :- maplist(inequal(X), Xss), allDifferent(Xss).


% Verifica si el Sudoku es consistente, es decir, si no hay valores repetidos en filas, columnas o cajas.
checkConsistency(Problem) :-
    maplist(allDifferent, Problem).


% Resuelve una parte del Sudoku asignando valores a las variables de manera recursiva.
solvePiece([], _, _, _) :- !.
solvePiece([Item|Rest], Dom, List, Problem) :- 
    nonvar(Item), solvePiece(Rest, Dom, List, Problem).
solvePiece([CurrentVar|Vars], Domain, List, Problem) :- 
        select(CurrentTip, Domain, NewDomain),  
        CurrentVar = CurrentTip,
        checkConsistency(Problem),
        solvePiece(Vars, NewDomain, List, Problem).
solvePiece(Vars,Domain, Problem) :-
    reduceDomain(Vars,Domain,ReducedDomain), 
    solvePiece(Vars, ReducedDomain, Vars, Problem).


% Resuelve todo el Sudoku aplicando solvePiece a todas las filas.
getSolution([],_).
getSolution([H|T],Domain) :- solvePiece(H,Domain,[H|T]), getSolution(T,Domain).



% Imprime el Sudoku.
printLine(_,[]) :- writeln('|').
printLine(BoxWidth, [H|T]) :-
    (
        (length([H|T], RemainingItems),
        Mod is RemainingItems mod BoxWidth,
        Mod = 0,
        write('| ')); 
        true
    ),
    (
        (var(H), write('_ '));
        (nonvar(H), write(H), write(' '))
    ),
    printLine(BoxWidth, T),!.
printSudoku([],_,_).
printSudoku([FirstRow|Rest],BoxWidth,BoxHeight) :-
    (
        (length([FirstRow|Rest], RemainingLines),
        Mod is RemainingLines mod BoxHeight,
        Mod = 0,
        writeln('')); 
        true
    ),
    printLine(BoxWidth, FirstRow),
    printSudoku(Rest,BoxWidth,BoxHeight), !.


% Resuelve el Sudoku asignando valores a las variables en función de las restricciones.
solveSudoku(H,W,Sudoku) :-
    getProblem(H,W,Sudoku,Problem), 
    Nums is H*W, 
    numlist(1,Nums,Domain), 
    random_permutation(Domain,MixedDomain),
    getSolution(Problem,MixedDomain),
    writeln('Solution: '),
    printSudoku(Sudoku, W, H).


% Genera un Sudoku resuelto y lo oculta según un nivel de dificultad.
generateSudoku(BoardWidth, BoardHeight, BoxWidth, BoxHeight, Diff, Out) :-
    getBoard(BoardHeight, BoardWidth, Sudoku),
    solveSudoku(BoxWidth, BoxHeight, Sudoku),
    maplist(obfuscateLine(Diff), Sudoku, Out), !,
    writeln('Sudoku: '),
    printSudoku(Out, BoxHeight, BoxWidth).
