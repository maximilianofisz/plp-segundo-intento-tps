:- use_module(piezas).

% Ej 1
%sublista(+Descartar, +Tomar, +L, -R)
sublista(D, T, L, R) :- length(L1, D), append(L1, L2, L), length(R, T), append(R, _, L2).


% Ej 2
%tablero(K, -T)
columnas(K, L) :- length(L, K).
tablero(K, T) :- length(T, 5), maplist(columnas(K), T).


%Ej 3
%dimensiones(+M, -F, -C)
dimensiones(M, F, C) :- length(M, F), maplist(columnas(C), M).


%Ej 4
%coordenadas(+T, -IJ)
coordenadas(T, (I,J)) :- between(1, 5, I), dimensiones(T, _, C), between(1, C, J).


%Ej 5
%kPiezas(+K, -PS)
kPiezas(K, PS) :- nombrePiezas(PiezasRestantes), kPiezasAux(K, PiezasRestantes, PS).
kPiezasAux(0, _, []).
kPiezasAux(K, [P | Restantes], [P | PS]) :- K > 0, length(Restantes, LenRestantes), KRec is K - 1, LenRestantes >= KRec, kPiezasAux(KRec, Restantes, PS).
kPiezasAux(K, [_ | Restantes], PS) :- K > 0, length(Restantes, LenRestantes), LenRestantes >= K, kPiezasAux(K, Restantes, PS).



% Ej 6
%seccionTablero(+T, +ALTO, +ANCHO, +IJ, ?ST)
descartoColumnas(D, T, L, R) :- sublista(D, T, L, R).
seccionTablero(T, ALTO, ANCHO, (I,J), ST) :- Ioffset is I - 1, sublista(Ioffset, ALTO, T, STParcial), %Descarto las filas de arriba y de abajo
                                              Joffset is J - 1, maplist(descartoColumnas(Joffset, ANCHO), STParcial, ST).  % Aplico a cada fila
                                              % Para las que quedaron en el medio, descarto

