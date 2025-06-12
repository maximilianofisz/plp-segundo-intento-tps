:- use_module(piezas).

% Completar ...

%Ej 1
%sublista(+Descartar, +Tomar, +L, -R)
sublista(D, T, L, R) :- append(L1, L2, L), length(L1, D), append(R, _, L2), length(R, T).

% Ej 2
%tablero(+K, -T)
%tablero(K, T) :- K > 0, crearFilas(5, K, T).
%PREGUNTAR: Tenemos esta opcion de aca con maplist y otra haciendo recursion                                                    

tablero(K, T) :- K > 0, length(T, 5), maplist(columnas(K), T).
columnas(K, L) :- length(L, K).


tablero2(K, T) :- K > 0, tableroAux(K, T, 5).
tableroAux(_, [], 0).
tableroAux(K, [X|Ts], C) :- C > 0, length(X, K), C1 is C - 1, tableroAux(K, Ts, C1).


%Ej 3
%tamaño(+M, -F, -C)
%tamano(M, F, C) :- length(M, F), member(X, M), length(X, C).
tamano(M, F, C) :- length(M, F), maplist(columnas(C), M).
%PREGUNTAR: Estamos usando bastante mapList, esta ok o deberiamos usar otro predicado (forEach, forall, etc)? Cuales son las diferencias?
 

% Ej 4
%coordenadas(+T, -IJ)
coordenadas(T, (I, J)) :- tamano(T, _, C), between(1, 5, I), between(1, C, J).

%PREGUNTAR: Medio random calculo que esta ok pero en consultas como tablero(3, T), coordenadas(T, IJ). prolog nos escupe tmb la unif de T, todo pelota?
%42 ?- tablero(3, T), coordenadas(T, IJ).
%T = [[_, _, _], [_, _, _], [_, _, _], [_, _, _], [_, _, _]],
%IJ = (1, 1) ;


%Ej 5
%kPiezas(+K, -PS)
kPiezas(K, PS) :- nombrePiezas(Piezas), kAux(K, Piezas, PS).

kAux(0, _, []).
kAux(K, [X | Piezas ] , [X |PS]) :- K > 0, length(Piezas, Y), K1 is K-1, Y >= K1, kAux(K1, Piezas, PS).
kAux(K, [_ | Piezas ] , PS) :- K > 0, length(Piezas, Y), Y >= K, kAux(K, Piezas, PS).

% Ej 6
%seccionTablero(+T, +ALTO, +ANCHO, +IJ, ?ST)
seccionTablero(T, ALTO, ANCHO, (I, J), ST) :-
  I1 is I - 1,
  sublista(I1, ALTO, T, FilasSeleccionadas),
  maplist(subcolumna(J, ANCHO), FilasSeleccionadas, ST).

%subcolumna(+Inicio, +Largo, +Fila, -SubFila)
subcolumna(Inicio, Largo, Fila, SubFila) :-
    I1 is Inicio - 1,
    sublista(I1, Largo, Fila, SubFila).
