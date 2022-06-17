write_log(S) :- open('error_logs.txt', append, Out), write(Out, S), write(Out, '\n'), close(Out).

/* slice([1, 2, 3 ,4],2,3,L2). */
/***************
* EJERCICIO 2. sum_pot_prod/4
*
*	ENTRADA:
*		X: Vector de entrada de numeros de valor real.
*		Y: Vector de entrada de numeros de valor real.
*		Potencia: Numero de valor entero, potencia.
*	SALIDA:
*		Resultado: Numero de valor real resultado de la operacion sum_pot_prod. 
*
****************/
potencia(A, 0, 1) :- not(A=0), !.
potencia(X, Y, P) :- Y1 is Y-1, potencia(X, Y1, P1), P is P1*X, Y>0.

sum_pot_prod([], [], _, 0).
sum_pot_prod([X|X1], [Y|Y1], P, R) :- 
    P < 0, print('Error 1.1 Potencia'), !, fail;
    length([X|X1], N1), length([Y|Y1], N2), N1 =\= N2, print('Error 1.2 Longitud'), !, fail;
    sum_pot_prod(X1, Y1, P, R1),
    M is X*Y,
    potencia(M, P, E),
    R is R1 + E.


/***************
* EJERCICIO 3. segundo_penultimo/3
*
*       ENTRADA:
*               L: Lista de entrada de numeros de valor real.
*       SALIDA:
*               X : Numero de valor real. Segundo elemento.
*		Y : Numero de valor real. Penultimo elemento.
*
****************/
segundo_penultimo([Y,X], X, Y) :-!.
segundo_penultimo([Y,_], _, Y) :-!. 
segundo_penultimo(L, _, _) :- longitud(L,Ll),  Ll < 2, write_log('ERROR 2.1 Longitud'), !, fail.
segundo_penultimo([_,X|R], X, Y) :- segundo_penultimo([X|R], X, Y).
segundo_penultimo([_|R], X, Y) :- segundo_penultimo(R, X, Y).

penultimo(X,[X,_]).
penultimo(X,[_,Y|Ys]) :- penultimo(X,[Y|Ys]).

segundo(X,[_,X|Y]).

segundo_penultimo(L, X, Y) :-
    length(L, N), N < 2, print('Error 2.1 Longitud'), !, fail;
    penultimo(Y, L),
    segundo(X, L).


/***************
* EJERCICIO 4. sublista/5
*
*       ENTRADA:
*		L: Lista de entrada de cadenas de texto.
*		Menor: Numero de valor entero, indice inferior.
l*		E: Elemento, cadena de texto.
*       SALIDA:
*		Sublista: Sublista de salida de cadenas de texto.
*
****************/

sublista(L, Menor, Mayor, E, SL):- 
   sublista_rec(L, Menor, Mayor, SL),
   comprobar_elemento(SL, E).

sublista_rec(_, 1, 0, []) :- !.

sublista_rec(_, Menor, Mayor, _) :- 
   Menor > Mayor, 
   write_log('ERROR 3.2 Indices'), !, fail.

sublista_rec(L, _, Mayor, _) :- 
   longitud(L,N), N < Mayor, 
   write_log('ERROR 3.2 Indices'), !, fail.

sublista_rec([X|L], 1, Mayor, Subl):-  
   May is Mayor-1, 
   sublista_rec(L, 1, May, SL),
   Subl = [X|SL].

sublista_rec([_|L], Menor, Mayor, SL):- 
   Men is Menor-1,
   May is Mayor-1, 
   sublista_rec(L, Men, May, SL).

comprobar_elemento(L, E) :- member(E, L).
comprobar_elemento(_, _) :- write_log('ERROR 3.1 Elemento'), !, fail.


/***************
* EJERCICIO 5. espacio_lineal/4
*
*       ENTRADA:
*               Menor: Numero de valor entero, valor inferior del intervalo.
*               Mayor: Numero de valor entero, valor superior del intervalo.
*               Numero_elementos: Numero de valor entero, numero de valores de la rejilla.
*       SALIDA:
*               Rejilla: Vector de numeros de valor real resultante con la rejilla.
*
****************/
espacio_lineal(Menor, Mayor, _, _) :- 
   Menor >= Mayor, 
   write_log('ERROR 4.1 Indices.'), !, fail.

espacio_lineal(Menor, Mayor, Numero_elementos, Rejilla) :- 
   N is Mayor/(Numero_elementos-1), 
   espacio_lineal_rec(N, Menor, Mayor, Rejilla).

espacio_lineal_rec(_, Menor, Mayor, []) :- Menor > Mayor, !.
espacio_lineal_rec(Npaso, Menor, Mayor, [Menor|R]) :- S is Menor + Npaso, espacio_lineal_rec(Npaso, S, Mayor, R).

/***************
* EJERCICIO 6. normalizar/2
*
*       ENTRADA:
*		Distribucion_sin_normalizar: Vector de numeros reales de entrada. Distribucion sin normalizar.
*       SALIDA:
*		Distribucion: Vector de numeros reales de salida. Distribucion normalizada.
*
****************/
normalizar(Distribucion_sin_normalizar, Distribucion) :- 
   constante(Distribucion_sin_normalizar, Sum), 
   normalizar_rec(Distribucion_sin_normalizar,Distribucion, Sum).

constante([], 0) :-!.
constante([N|_], _) :- N < 0, write_log('ERROR 5.1 Negativos.'), !, fail.
constante([N|R], Sum) :- constante(R, C), Sum is C + N.

normalizar_rec([],[], _) :-!.
normalizar_rec([X|Rx], [Y|Ry], N) :- Y is X/N, normalizar_rec(Rx, Ry, N).

/***************
* EJERCICIO 7. divergencia_kl/3
*
*       ENTRADA:
*		D1: Vector de numeros de valor real. Distribucion.
*		D2: Vector de numeros de valor real. Distribucion.
*       SALIDA:
*		KL: Numero de valor real. Divergencia KL.
*
****************/

testDivergencias(D1, _) :- constante(D1, R), R \== 1.0, write_log('ERROR 6.2. Divergencia KL definida solo para distribuciones.'), !, fail.
testDivergencias(_, D2) :- constante(D2, R), R \== 1.0, write_log('ERROR 6.2. Divergencia KL definida solo para distribuciones.'), !, fail.
testDivergencias(_, _) :- true.

divergencia_kl_rec([], [], 0) :-!.
divergencia_kl_rec([N|_], _, _) :- N =< 0, write_log('ERROR 6.1. Divergencia KL no definida.'), !, fail.
divergencia_kl_rec(_, [N|_], _) :- N =< 0, write_log('ERROR 6.1. Divergencia KL no definida.'), !, fail.
divergencia_kl_rec([N1|D1], [N2|D2], KL) :- divergencia_kl_rec(D1, D2, N), KL is N1*log(N1/N2)+N.

divergencia_kl(D1, D2, KL) :- testDivergencias(D1,D2), divergencia_kl_rec(D1, D2, KL).

/***************
* EJERCICIO 8. producto_kronecker/3
*
*       ENTRADA:
*		Matriz_A: Matriz de numeros de valor real.
*		Matriz_B: Matriz de numeros de valor real.
*       SALIDA:
*		Matriz_bloques: Matriz de bloques (matriz de matrices) de numeros reales.
*
****************/
producto_kronecker(Matriz_A, Matriz_B, Matriz_bloques) :- producto_kronecker_rec1(Matriz_A, Matriz_B, Matriz_bloques).
   
producto_kronecker_rec1([], _, []):-!.
producto_kronecker_rec1([FA|MA], MB, [R1|R]) :- producto_kronecker_rec1(MA, MB, R), producto_kronecker_rec2(FA, MB, R1).

producto_kronecker_rec2([], _, []):-!.
producto_kronecker_rec2([XA|_], _, _):- XA < 0, write_log('ERROR 7.1. Elemento menor que cero.'), !, fail.
producto_kronecker_rec2([XA|FA], MB, [R1|R]):-producto_kronecker_rec2(FA, MB, R), producto_kronecker_rec3(XA, MB, R1).

producto_kronecker_rec3(_, [], []):-!.
producto_kronecker_rec3(XA, [FB|MB], [R1|R]):-producto_kronecker_rec3(XA, MB, R), producto_kronecker_rec4(XA, FB, R1).

producto_kronecker_rec4(_, [], []):-!.
producto_kronecker_rec4(_, [XB|_], _):- XB < 0, write_log('ERROR 7.1. Elemento menor que cero.'), !, fail.
producto_kronecker_rec4(XA, [XB|FB], [N|R]):-producto_kronecker_rec4(XA, FB, R), N is XA * XB.
/***************
* EJERCICIO 9a. distancia_euclidea/3
*
*       ENTRADA:
*               X1: Vector de numeros de valor real. 
*               X2: Vector de numeros de valor real.
*       SALIDA:
*               D: Numero de valor real. Distancia euclidea.
*
****************/

distancia_euclidea_rec([],[], 0) :-!.
distancia_euclidea_rec([X1|R1], [X2|R2], D) :- distancia_euclidea_rec(R1, R2, Sum), D is ((X1*X1) + (X2*X2) - (2*X1*X2) + Sum).

distancia_euclidea(X1, X2, D) :- distancia_euclidea_rec(X1, X2, R), D is sqrt(R).

/***************
* EJERCICIO 9b. calcular_distancias/3
*
*       ENTRADA:
*               X_entrenamiento: Matriz de numeros de valor real donde cada fila es una instancia representada por un vector.
*               X_test: Matriz de numeros de valor real donde cada fila es una instancia representada por un vector. Instancias sin etiquetar.
*       SALIDA:
*               Matriz_resultados: Matriz de numeros de valor real donde cada fila es un vector con la distancia de un punto de test al conjunto de entrenamiento X_entrenamiento.
*
****************/
calcular_distancias(X_entrenamiento, X_test, Matriz_resultados) :-
   calcular_distancias_rec1(X_entrenamiento, X_test, Matriz_resultados).

calcular_distancias_rec1(_, [], []) :-!.
calcular_distancias_rec1(M1, [X|M2], [R1|R]) :- 
   calcular_distancias_rec1(M1, M2, R),
   calcular_distancias_rec2(M1, X, R1).
   

calcular_distancias_rec2([], _, []) :-!.
calcular_distancias_rec2([X|M1], Y, [D|R]) :- 
   calcular_distancias_rec2(M1, Y, R),
   distancia_euclidea(X,Y,D).

/***************
* EJERCICIO 9c. predecir_etiquetas/4
*
*       ENTRADA:
*               Y_entrenamiento: Vector de valores alfanumericos de una distribucion categorica. Cada etiqueta corresponde a una instancia de X_entrenamiento.
*               K: Numero de valor entero.
*               Matriz_resultados: Matriz de numeros de valor real donde cada fila es un vector con la distancia de un punto de test al conjunto de entrenamiento X_entrenamiento.
*       SALIDA:
*               Y_test: Vector de valores alfanumericos de una distribucion categorica. Cada etiqueta corresponde a una instancia de X_test.
*
****************/

predecir_etiquetas(_, _, [], []) :-!.
predecir_etiquetas(Y_entrenamiento, K, [X|Mr], [R|Y_test]) :-
   predecir_etiquetas(Y_entrenamiento, K, Mr, Y_test),
   predecir_etiqueta(Y_entrenamiento, K, X, R).

/***************                             
* EJERCICIO 9d. predecir_etiqueta/4                                                          
*
*       ENTRADA:
*               Y_entrenamiento: Vector de valores alfanumericos de una distribucion categorica. Cada etiqueta corresponde a una instancia de X_entrenamiento.
*               K: Numero de valor entero.
*               Vec_distancias: Vector de valores reales correspondiente a una fila de Matriz_resultados.
*       SALIDA:
*               Etiqueta: Elemento de valor alfanumerico.
*
****************/
predecir_etiqueta(Y_entrenamiento, K, Vec_distancias, Etiqueta) :- 
   calcular_K_etiquetas_mas_relevantes(Y_entrenamiento, K, Vec_distancias, R),
   calcular_etiqueta_mas_relevante(R, Etiqueta).

/***************
* EJERCICIO 9e. calcular_K_etiquetas_mas_relevantes/4
*
*       ENTRADA:
*               Y_entrenamiento: Vector de valores alfanumericos de una distribucion categorica. Cada etiqueta corresponde a una instancia de X_entrenamiento.
*               K: Numero de valor entero.
*               Vec_distancias: Vector de valores reales correspondiente a una fila de Matriz_resultados.
*       SALIDA:
*		K_etiquetas: Vector de valores alfanumericos de una distribucion categorica.
*
****************/

unir_clave_valor([],[],[]):-!.
unir_clave_valor([X|L1], [Y|L2], [[Y,X]|R]) :- unir_clave_valor(L1, L2, R).

calcular_K_etiquetas_mas_relevantes_rec(_, 0, []) :-!.
calcular_K_etiquetas_mas_relevantes_rec([[_,X]|L], K, [X|R]) :- K1 is K-1, calcular_K_etiquetas_mas_relevantes_rec(L, K1, R).

calcular_K_etiquetas_mas_relevantes(Y_entrenamiento, K, Vec_distancias, K_etiquetas) :- 
   unir_clave_valor(Y_entrenamiento, Vec_distancias, L),
   sort(L, Sorted),
   calcular_K_etiquetas_mas_relevantes_rec(Sorted, K, K_etiquetas).

/***************
* EJERCICIO 9f. calcular_etiqueta_mas_relevante/2
*
*       ENTRADA:
*               K_etiquetas: Vector de valores alfanumericos de una distribucion categorica.
*       SALIDA:
*               Etiqueta: Elemento de valor alfanumerico.
*
****************/

calcular_contadores_rec1(_, [], []):-!.
calcular_contadores_rec1(L, [E1|E], [[Sum,E1]|R]) :- 
   calcular_contadores_rec2(L,E1,Sum),
   calcular_contadores_rec1(L,E,R).


calcular_contadores_rec2([], _, 0):-!.
calcular_contadores_rec2([L1|L], E, R) :-
   L1 \== E,
   calcular_contadores_rec2(L,E,R).

calcular_contadores_rec2([L1|L], E, R) :-
   L1 == E,
   calcular_contadores_rec2(L,E,R1),
   R is R1+1.

calcular_etiqueta_mas_relevante(K_etiquetas, Etiqueta) :- 
   sort(K_etiquetas, Etiquetas),
   calcular_contadores_rec1(K_etiquetas, Etiquetas, Count),
   sort(Count, Sortedcount),
   last(Sortedcount, [_,Etiqueta]).
    
/***************
* EJERCICIO 9g. k_vecinos_proximos/5
*
*       ENTRADA:
*		X_entrenamiento: Matriz de numeros de valor real donde cada fila es una instancia representada por un vector.
*		Y_entrenamiento: Vector de valores alfanumericos de una distribucion categorica. Cada etiqueta corresponde a una instancia de X_entrenamiento.
*		K: Numero de valor entero.
*		X_test: Matriz de numeros de valor real donde cada fila es una instancia representada por un vector. Instancias sin etiquetar.
*       SALIDA:
*		Y_test: Vector de valores alfanumericos de una distribucion categorica. Cada etiqueta corresponde a una instancia de X_test.
*
****************/
k_vecinos_proximos(X_entrenamiento, Y_entrenamiento, K, X_test, Y_test) :- 
   calcular_distancias(X_entrenamiento, X_test, R),
	predecir_etiquetas(Y_entrenamiento, K, R, Y_test).

/***************
* EJERCICIO 9h. clasifica_patrones/4
*
*       ENTRADA:
*		iris_patrones.csv: Fichero con los patrones a clasificar, disponible en Moodle.
*		iris_etiquetas.csv: Fichero con las etiquetas de los patrones a clasificar, disponible en Moodle.
*		K: Numero de valor entero.
*       SALIDA:
*		tasa_aciertos: Tasa de acierto promediada sobre las iteraciones leave-one-out
*
****************/


/***************
* EJERCICIO 10. fractal/0
*
****************/
fractal :-
   new(D, window('Fractal')),
   send(D, size, size(800, 600)),
   draw(D, 300, 300, 0, 2),

   send(D, open).

draw(_D, _X, _Y, _Angle, 0).
draw(D, X1, Y1, Angle, Depth) :-
   X2 is X1 + Depth * 10.0,
   Y2 is Y1 + Depth * 10.0,
   X3 is X2 + Depth * 10.0,
   Y3 is Y2 + Depth * 10.0,
   X4 is X3 + Depth * 10.0,
   Y4 is Y3 + Depth * 10.0,
   new(Line, line(X1, Y1, X2, Y2, none)),
   send(D, display, Line),
   A1 is Angle - 0,
   A2 is Angle - 60,
   A3 is Angle + 120,
   A4 is Angle - 60,
   De is Depth - 1,
   draw(D, X1, Y1, A1, De),
   draw(D, X2, Y2, A2, De),
   draw(D, X3, Y3, A3, De),
   draw(D, X4, Y4, A4, De).

