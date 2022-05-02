:- module(proylcc, 
	[  
        flickInicial/7,
		flickGeneral/7
	]).
	
:-dynamic visitados/1.


flickInicial(Grid,Color,[CeldaInicial],FGrid,NewCapturadas,CantNewCapturadas,Complete) :-
    findall(Celda,(adyacenteCE(Grid,CeldaInicial,Celda)),AdyacentesCEInicialAux),
    retractall(visitados(_)),
    list_to_set(AdyacentesCEInicialAux,AdyacentesCEInicial),
    flickGeneral(Grid,Color,AdyacentesCEInicial,FGrid,NewCapturadas,CantNewCapturadas,Complete).
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

cambiarColor(_Color,[],Grid,Grid).
cambiarColor(Color,[Celda | ConjuntoCeldas],Grid,FGrid):-
   cambiarColor(Color,ConjuntoCeldas,Grid,FGridParcial),
   cambiarColorCelda(Celda,FGridParcial,Color,FGrid).

cambiarColorCelda([X,Y], Grid, Color, FGrid) :-
	nth0(X,Grid,Fila),
	nth0(Y,Fila,ColorInicialSquare),
	Color \= ColorInicialSquare,
	replace(Y,Fila,Color,FilaNueva),
	replace(X,Grid,FilaNueva,FGrid).

replace(Indice,Lista,Elemento,ListaResultado) :-
  nth0(Indice,Lista,_,Resto),
  nth0(Indice,ListaResultado,Elemento,Resto).

adyacenteCE(Grid,Celda1,Celda2) :-
       assert(visitados(Celda1)),
       adyacenteC(Grid,Celda1,Celda2).
adyacenteCE(Grid,Celda1,Celda2) :-
       adyacenteC(Grid,Celda1,Celda3),           
       Celda3 \== Celda2,
       not(visitados(Celda3)),
       assert(visitados(Celda3)),
       adyacenteCE(Grid,Celda3,Celda2).

adyacenteC(Grid,[X1,Y1],[X2,Y2]):-
    adyacentes(Grid,[X1,Y1],ListaAdy),
	member([X2,Y2],ListaAdy),
    nth0(X1,Grid,Fila),
	nth0(Y1,Fila,Color),
	nth0(X2,Grid,FilaAd),
	nth0(Y2,FilaAd,ColorAd),
	Color = ColorAd.

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

longitudGrilla(Grid,CantFilas,CantColumnas):-
    nth0(0,Grid,Fila),
    length(Grid,CantFilas),
    length(Fila,CantColumnas).
    