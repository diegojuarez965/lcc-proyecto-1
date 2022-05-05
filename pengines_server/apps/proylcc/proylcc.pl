:- module(proylcc, 
	[  
        flickInicial/7,
		flickGeneral/7
	]).
	
:-dynamic visitados/1.


/*flickInicial(+Grid,+Color,+CeldaInicial,-FGrid,-NewCapturadas,-CantNewCapturadas,-Complete) 
  Obtiene el conjunto inicial de celdas capturadas y realiza el cambio de color de las mismas
  Grid es la grilla a utilizar
  Color es el color al cual cambiar las celdas inicialmente capturadas
  CeldaInicial es la celda con la que inicia el juego
  FGrid es la grilla resultante de cambiar el color a todas las celdas inicialmente capturadas
  NewCapturadas son las celdas adyacentes y del mismo color a la celda inicial después de cambiar el color de las celdas inicialmente Capturadas
  CantNewCapturadas es el tamaño del conjunto de celdas NewCapturadas
  Complete es el valor de verdad que indica si se capturaron todas las celdas de la grilla
*/
flickInicial(Grid,Color,[CeldaInicial],FGrid,NewCapturadas,CantNewCapturadas,Complete) :-
    findall(Celda,(adyacenteCE(Grid,CeldaInicial,Celda)),AdyacentesCEInicialAux),
    retractall(visitados(_)),
    list_to_set(AdyacentesCEInicialAux,AdyacentesCEInicial),
    flickGeneral(Grid,Color,AdyacentesCEInicial,FGrid,NewCapturadas,CantNewCapturadas,Complete).


/*flickGeneral(+Grid,+Color,+Capturadas,-FGrid,-NewCapturadas,-CantNewCapturadas,-Complete)
  Cambia el color de las celdas capturadas y obtiene las nuevas celdas capturadas
  Grid es la grilla a utilizar
  Color es el color al cual cambiar las celdas capturadas
  Capturadas son las celdas capturadas, es decir todas las que son adyacentes del mismo color a partir de la celda inicial
  FGrid es la grilla resultante de cambiar el color a todas las celdas capturadas
  NewCapturadas son las celdas adyacentes y del mismo color a la celda inicial después de cambiar el color de las celdas Capturadas
  CantNewCapturadas es el tamaño del conjunto de celdas NewCapturadas
  Complete es el valor de verdad que indica si se capturaron todas las celdas de la grilla
*/
flickGeneral(Grid,Color,Capturadas,FGrid,NewCapturadas,CantNewCapturadas,Complete) :-
	cambiarColor(Color,Capturadas,Grid,FGrid),
    member(CeldaCapturada,Capturadas),
	findall(Celda,(adyacenteCE(FGrid,CeldaCapturada,Celda)),NewCapturadasAux),
    retractall(visitados(_)),
    list_to_set(NewCapturadasAux,NewCapturadas),
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


/*adyacenteCE(+Grid,+Celda1,+Celda2)
  Verifica si 2 celdas son adyacentes y tienen el mismo color transitivamente
  Grid es la grilla a utilizar
  Celda1 y Celda2 son las celdas a verificar si son adyacenteCE
*/
adyacenteCE(Grid,Celda1,Celda2) :-
       assert(visitados(Celda1)),
       adyacenteC(Grid,Celda1,Celda2).
adyacenteCE(Grid,Celda1,Celda2) :-
       adyacenteC(Grid,Celda1,Celda3),           
       Celda3 \== Celda2,
       not(visitados(Celda3)),
       assert(visitados(Celda3)),
       adyacenteCE(Grid,Celda3,Celda2).


/*adyacenteC(+Grid,+Celda1,+Celda2)
  Verifica si 2 celdas son adyacentes y tienen el mismo color
  Grid es la grilla a utilizar
  Celda1 y Celda2 son las celdas a verificar si son adyacenteC  
 */
adyacenteC(Grid,[X1,Y1],[X2,Y2]):-
    adyacentes(Grid,[X1,Y1],ListaAdy),
	member([X2,Y2],ListaAdy),
    nth0(X1,Grid,Fila),
	nth0(Y1,Fila,Color),
	nth0(X2,Grid,FilaAd),
	nth0(Y2,FilaAd,ColorAd),
	Color = ColorAd.


/*adyacentes(+Grid,+Celda,-ListaAdyacentes)
  Obtiene las celdas adyacentes de determinada celda
  Grid es la grilla a utilizar
  Celda es la celda a calcularle las celdas adyacentes
  ListaAdyacentes es la lista que contiene las celdas adyacentes a Celda 
*/
adyacentes(Grid,[X,Y],[[X,Y]|ListaAdy]):-
    longitudGrilla(Grid,CantFilas,CantColumnas),
    is(XMax,CantFilas-1),
    is(YMax,CantColumnas-1),
    
    is(XMenos1,X-1),
    is(XMas1,X+1),
    is(YMenos1,Y-1),
    is(YMas1,Y+1),
    
   /*Adyacentes con respecto a Arriba y Abajo*/
   ((is(X,0),
   append([],[[1,Y]],ListaAdyParcial)); 
   
   (is(X,XMax),
   append([],[[XMenos1,Y]],ListaAdyParcial));
   
   (X\=0,X\=XMax,
	append([],[[XMenos1,Y]],ListaAdyParcial1), 
    append(ListaAdyParcial1,[[XMas1,Y]],ListaAdyParcial))), 
    
   /*Adyacentes con respecto a Izquierda y Derecha*/
   ((is(Y,0),
   append(ListaAdyParcial,[[X,1]],ListaAdy)); 
   
   (is(Y,YMax),
   append(ListaAdyParcial,[[X,YMenos1]],ListaAdy));
   
   (Y\=0,Y\=YMax,
	append(ListaAdyParcial,[[X,YMenos1]],ListaAdyParcial2), 
    append(ListaAdyParcial2,[[X,YMas1]],ListaAdy))). 


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
    