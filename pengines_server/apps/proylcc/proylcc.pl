:- module(proylcc, [obtenerCapturadasInicial/5,flick/7,ayudaEstrategia/7]).



/*
  ayudaEstrategia(+PE,+ListaColores,+Grid,+Capturadas,+Metodo,-JugadasColores,-CantFCapturadas)
  Calcula la secuencia de a lo sumo PE jugadas que consiguen capturar la mayor cantidad de celdas de la forma
  mas correcta y eficiente posible
  PE es la profundidad de estrategia
  ListaColores es el conjunto de colores seleccionables en el juego
  Grid es la grilla a utilizar
  Capturadas es el conjunto de celdas capturadas en Grid
  Metodo es el metodo a utilizar
  JugadasColores es la secuencia de jugadas resultante
  CantFCapturadas es la cantidad de celdas capturadas al realizar JugadasColores
*/
ayudaEstrategia(PE,ListaColores,Grid,Capturadas,Metodo,JugadasColores,CantFCapturadas) :-
  (is(Metodo,0), !,
  ayudaEstrategiaOptimal(PE,ListaColores,Grid,Capturadas,JugadasColores,CantFCapturadas));
  ayudaEstrategiaGreedy(PE,ListaColores,Grid,Capturadas,JugadasColores,CantFCapturadas).


/*
  ayudaEstrategiaOptimal(+PE,+ListaColores,+Grid,+Capturadas,-JugadasColores,-CantFCapturadas)
  Calcula la secuencia de a lo sumo PE jugadas que consiguen capturar la mayor cantidad de
  celdas 
  PE es la profundidad de estrategia
  ListaColores es el conjunto de colores seleccionables en el juego
  Grid es la grilla a utilizar
  Capturadas es el conjunto de celdas capturadas en Grid
  JugadasColores es la secuencia de jugadas resultante
  CantFCapturadas es la cantidad de celdas capturadas al realizar JugadasColores
*/
ayudaEstrategiaOptimal(PE,ListaColores,Grid,Capturadas,JugadasColores,CantFCapturadas) :-
  ayudaEstrategiaOptimalAux(PE,ListaColores,Grid,Capturadas,Grids),
  obtenerMayorGrilla(Grids,[_FGrid,FCapturadas,JugadasColoresRev]),
  reverse(JugadasColoresRev,JugadasColores),
  length(FCapturadas,CantFCapturadas).

ayudaEstrategiaOptimalAux(0,_,Grid,Capturadas,[[Grid,Capturadas,[]]]).
ayudaEstrategiaOptimalAux(PE,ListaColores,Grid,Capturadas,FGrids):-
  is(PEMenos1,PE-1),
  ayudaEstrategiaOptimalAux(PEMenos1,ListaColores,Grid,Capturadas,PreGrids),
  longitudGrilla(Grid,CantFilas,CantColumnas),
  is(LongitudGrilla,CantFilas*CantColumnas),
  ((completadas(LongitudGrilla,PreGrids,FGrids),!);
  (avanzarNivelGrillas(ListaColores,PreGrids,FGrids))).


/*
  ayudaEstrategiaGreedy(+PE,+ListaColores,+Grid,+Capturadas,-JugadasColores,-CantFCapturadas)
  Calcula la secuencia de a lo sumo PE jugadas que consiguen capturar la mayor cantidad de
  celdas (En la mayoría de los casos)
  PE es la profundidad de estrategia
  ListaColores es el conjunto de colores seleccionables en el juego
  Grid es la grilla a utilizar
  Capturadas es el conjunto de celdas capturadas en Grid
  JugadasColores es la secuencia de jugadas resultante
  CantFCapturadas es la cantidad de celdas capturadas al realizar JugadasColores
*/
ayudaEstrategiaGreedy(PE,ListaColores,Grid,Capturadas,JugadasColores,CantFCapturadas) :-
  ayudaEstrategiaGreedyAux(PE,ListaColores,Grid,Capturadas,[_FGrid,FCapturadas,JugadasColores]),
  length(FCapturadas,CantFCapturadas).

ayudaEstrategiaGreedyAux(0,_,Grid,Capturadas,[Grid,Capturadas,[]]).
ayudaEstrategiaGreedyAux(PE,ListaColores,Grid,Capturadas,[FGrid,FCapturadas,JugadasColores]):-
    is(PEMenos1,PE-1),
    ayudaEstrategiaGreedyAux(PEMenos1,ListaColores,Grid,Capturadas,[PreFGrid,PreFCapturadas,PreJugadasColores]),
    longitudGrilla(Grid,CantFilas,CantColumnas),
    is(LongitudGrilla,CantFilas*CantColumnas),
    ((length(PreFCapturadas,LongitudGrilla),!,
     append(PreJugadasColores,[],JugadasColores),
     append(PreFCapturadas,[],FCapturadas),
     append(PreFGrid,[],FGrid));   
    (avanzarNivel(ListaColores,[PreFGrid,PreFCapturadas,PreJugadasColores],ListaGrillas),
     obtenerMayorGrilla(ListaGrillas,[FGrid,FCapturadas,[Color | _SeqColores]]),
     append(PreJugadasColores,[Color],JugadasColores))). 


/*
  obtenerMayorGrilla(+GridsInfo,-Res)
  Obtiene la grilla del tipo GridInfo con mayor cantidad de capturadas
  +GridsInfo lista de GridInfo a encontrar la mayor grilla
  -Res GridInfo resultado con mayor cantidad de celdas capturadas
*/
obtenerMayorGrilla([GridInfo | GridsInfo],Res):- obtenerMayorGrilla(GridsInfo,GridInfo,Res).
obtenerMayorGrilla([[Grid,Capturadas,Colores] | GridsInfo],[_GridMayor,CapturadasMayor,_ColoresMayor],Res):-
  length(Capturadas,LongCapt),
  length(CapturadasMayor,LongCaptMayor),
  LongCapt>LongCaptMayor, !,
  obtenerMayorGrilla(GridsInfo,[Grid,Capturadas,Colores],Res).
obtenerMayorGrilla([_ | GridsInfo],Max,Res):-
  obtenerMayorGrilla(GridsInfo,Max,Res).
obtenerMayorGrilla([],Max,Max).
  

/*
  avanzarNivelGrillas(+ListaColores,+GridsInformacion,-FGridsInfo)
  Obtiene todas las grillas resultantes de avanzar un nivel a cada grilla
  ListaColores es la lista de colores para realizar los flicks de cada grilla
  GridsInformacion es una lista de GridInformacion a la cual realizar un avance de nivel a cada una
  FGridsInfo es una lista GridInformacion resultante de haberle avanzado nivel a cada grilla ingresada
*/
avanzarNivelGrillas(_,[],[]).
avanzarNivelGrillas(ListaColores,[GridInfo | GridsInfo],FGridsInfo) :-
  avanzarNivel(ListaColores,GridInfo,FGridsInfoAux),
  avanzarNivelGrillas(ListaColores,GridsInfo,FGrillasInfoRes),
  append(FGridsInfoAux,FGrillasInfoRes,FGridsInfo).


/*
  avanzarNivel(+ListaColores,+GridInformacion,-ListaGrillas)
  Obtiene todas las grillas resultantes de realizar un flick por color a la grilla ingresada (es decir avanza un nivel)
  ListaColores es la lista de colores para realizar los flicks a la grilla
  GridInformacion es de la forma [Grilla,Capturadas,Colores], almacena una grilla junto con otra informacion
  relevante, un conjunto de celdas Capturadas en la grilla y una secuencia de colores relacionadas a la grilla
  ListaGrillas es un conjunto de GridInformacion resultado de haber hecho flick con cada color a la grilla ingresada
*/
avanzarNivel(ListaColores,[Grid,Capturadas,Colores],ListaGrillas):-
    member(CeldaCapturada,Capturadas),
    CeldaCapturada=[X,Y],
    nth0(X,Grid,Fila),
    nth0(Y,Fila,ColorInicial),
    delete(ListaColores,ColorInicial,ListaColoresNivel),
    length(Capturadas,CantCapturadas),
    findall(
            [FGrid,NewCapturadas,[Color | Colores]],
            (
             member(Color,ListaColoresNivel),
             flick(Grid,Color,Capturadas,FGrid,NewCapturadas,CantNewCapturadas,_Complete),
             CantNewCapturadas>CantCapturadas
            ),
            ListaGrillas
           ).
  

/*
  completadas(+LongitudGrilla,+GridsInfo,-GridInfo)
  Determina si alguna grilla ingresada fue completada, es decir que todas sus celdas sean del mismo color y la retorna
  LongitudGrilla es la longitud de la grilla modelo que aplica para todas
  GridsInfo es una lista de GridInfo a verificar si alguna se completo
  GridInfo es una grilla completada del tipo GridInfo en caso de haberla
*/
completadas(LongitudGrilla,[[Grid,Capturadas,SeqColores]| _GridsInfo],[Grid,Capturadas,SeqColores]):-
  length(Capturadas,LongitudGrilla), !.
completadas(LongitudGrilla,[_ | GridsInfo],Grid):-
  completadas(LongitudGrilla,GridsInfo,Grid).


/*obtenerCapturadasInicial(+Grid,+CeldaInicial,-NewCapturadas,-CantNewCapturadas,-Complete) 
  Obtiene el conjunto inicial de celdas capturadas
  Grid es la grilla a utilizar
  CeldaInicial es la celda con la que inicia el juego
  NewCapturadas son las celdas adyacentes y del mismo color a la celda inicial 
  CantNewCapturadas es el tamaño del conjunto de celdas NewCapturadas
  Complete es el valor de verdad que indica si se capturaron todas las celdas de la grilla
*/
obtenerCapturadasInicial(Grid,CeldaInicial,NewCapturadas,CantNewCapturadas,Complete) :-
    adyCStar(CeldaInicial,Grid,NewCapturadas),
    length(NewCapturadas,CantNewCapturadas),
    longitudGrilla(Grid,CantFilas,CantColumnas),
    is(LongitudGrilla,CantFilas*CantColumnas),
    ((LongitudGrilla=:=CantNewCapturadas,
      Complete=true);
    (LongitudGrilla=\=CantNewCapturadas,
      Complete=false)). 
   

/*flick(+Grid,+Color,+Capturadas,-FGrid,-NewCapturadas,-CantNewCapturadas,-Complete)
  Cambia el color de las celdas capturadas y obtiene las nuevas celdas capturadas
  Grid es la grilla a utilizar
  Color es el color al cual cambiar las celdas capturadas
  Capturadas son las celdas capturadas, es decir todas las que son adyacentes del mismo color a partir de la celda inicial
  FGrid es la grilla resultante de cambiar el color a todas las celdas capturadas
  NewCapturadas son las celdas adyacentes y del mismo color a la celda inicial después de cambiar el color de las celdas Capturadas
  CantNewCapturadas es el tamaño del conjunto de celdas NewCapturadas
  Complete es el valor de verdad que indica si se capturaron todas las celdas de la grilla
*/
flick(Grid,Color,Capturadas,FGrid,NewCapturadas,CantNewCapturadas,Complete) :-
	cambiarColor(Color,Capturadas,Grid,FGrid),
  member(CeldaCapturada,Capturadas),
	adyCStar(CeldaCapturada,FGrid,NewCapturadas),
  length(NewCapturadas,CantNewCapturadas),
  longitudGrilla(FGrid,CantFilas,CantColumnas),
  is(LongitudGrilla,CantFilas*CantColumnas),
  ((LongitudGrilla=:=CantNewCapturadas,
      Complete=true);
  (LongitudGrilla=\=CantNewCapturadas,
      Complete=false)). 


/*cambiarColor(+Color,+Celdas,+Grid,-FGrid)
  Cambia de color un conjunto de celdas
  Color es el color al cual cambiar las Celdas
  Celdas son las celdas a cambiar de Color
  Grid es la grilla a utilizar
  FGrid es la grilla resultante de cambiar las Celdas de Color
*/
cambiarColor(_Color,[],Grid,Grid).
cambiarColor(Color,[Celda | ConjuntoCeldas],Grid,FGrid):-
   cambiarColor(Color,ConjuntoCeldas,Grid,FGridParcial),
   cambiarColorCelda(Celda,FGridParcial,Color,FGrid).


/*cambiarColorCelda(+Celda,+Grid,+Color,-FGrid)
  Cambia el color de una celda 
  Celda es la celda a cambiar de color
  Grid es la grilla a utilizar
  Color es el color al cual cambiar la Celda
  FGrid es la grilla resultante de cambiar la Celda de Color
*/
cambiarColorCelda([X,Y], Grid, Color, FGrid) :-
	nth0(X,Grid,Fila),
	nth0(Y,Fila,ColorInicialSquare),
	Color \= ColorInicialSquare,
	replace(Y,Fila,Color,FilaNueva),
	replace(X,Grid,FilaNueva,FGrid).


/*replace(+Indice,+Lista,+Elemento,-ListaResultado)
  Reemplaza un elemento ubicado en cierto índice de una lista por otro
  Indice es el índice en el que está ubicado el elemento a reemplazar en Lista
  Lista es la lista que contiene el elemento a ser reemplazado
  Elemento es el elemento para reemplazar
  ListaResultado es la lista resultante de realizar el reemplazo
*/
replace(Indice,Lista,Elemento,ListaResultado) :-
  nth0(Indice,Lista,_,Resto),
  nth0(Indice,ListaResultado,Elemento,Resto).


/*
 * adyCStar(Origin, +Grid, -Res)
 * Calcula el conjunto de celdas adyacentesC* de la celda Origin en la grilla Grid
 * siguiendo una estrategia de propagación o expansión.
 */
adyCStar(Origin, Grid, Res) :-
    adyCStarSpread([Origin], [], Grid, Res).


/*
 * adyCStarSpread(+Pend, +Vis, +Grid, -Res)
 * Pend: por "pendientes", inicialmente es la lista [Origin], y en general es 
 * el conjunto de celdas adyacentesC* a Origin que aún no fueron consideradas.
 * Vis: por "visitados", inicialmente [], son las celdas adyacentesC* a la Origen 
 * que ya fueron consideradas.
 * Grid: idem adyCStar
 * Res: idem adyCStar
 * En cada paso se selecciona una celda de las pendientes, se pasa a visitados, y
 * se agregan a pendientes todas aquellas adyacentes a la celda, del mismo color, que no estén
 * ya ni en pendientes ni visitados.
 */
adyCStarSpread([], Vis, _Grid, Vis).
adyCStarSpread(Pend, Vis, Grid, Res):-
    Pend = [P|Ps],
    findall(A, 
	        (
    	        adyC(P, Grid, A),
        	    not(member(A, Pend)),
            	not(member(A, Vis))
	        ), 
            AdyCP),
    append(AdyCP, Ps, NPend),
    adyCStarSpread(NPend, [P|Vis], Grid, Res).


/* 
 * adyC(+P, +Grid, -A)
   Dada una celda calcula una celda adyacente y del mismo color 
   P es una celda
   Grid es la grilla a utilizar
   A es una celda adyacente a P y del mismo color 
 */
adyC(P, Grid, A):-
    ady(P, Grid, A),
    color(P, Grid, C),
    color(A, Grid, C).


/* 
 * ady(+P, +Grid, -A)
  Dada una celda calcula una celda adyacente  
   P es una celda
   Grid es la grilla a utilizar
   A es una celda adyacente a P
 */
ady([X, Y], Grid, [X1, Y]):-
    length(Grid, L),
    X < L - 1,
    X1 is X + 1.
ady([X, Y], _Grid, [X1, Y]):-
    X > 0,
    X1 is X - 1.
ady([X, Y], Grid, [X, Y1]):-
    Grid = [F|_],
    length(F, L),
    Y < L - 1,
    Y1 is Y + 1.
ady([X, Y], _Grid, [X, Y1]):-
    Y > 0,
    Y1 is Y - 1.


/* 
 * color(+P,+Grid,-C)
   Dada una celda calcula una celda del mismo color 
   P es una celda
   Grid es la grilla a utilizar
   C es una celda del mismo color que P 
 */
color([X,Y], Grid, C):-
    nth0(X, Grid, F),
    nth0(Y, F, C).    
 

/* longitudGrilla(+Grid,-CantFilas,-CantColumnas)
   Calcula cuantas filas y columnas tiene la grilla
   Grid es la grilla a utilizar
   CantFilas es la cantidad de filas de la grilla Grid
   CantColumnas es la cantidad de columnas de la grilla Grid
 */ 
longitudGrilla(Grid,CantFilas,CantColumnas):-
    nth0(0,Grid,Fila),
    length(Grid,CantFilas),
    length(Fila,CantColumnas).


    
