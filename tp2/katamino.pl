:- use_module(piezas).

% Ej 1
%sublista(+Descartar, +Tomar, +L, -R)
sublista(D, T, L, R) :-
  append(L1, L2, L),
  length(L1, D),
  append(R, _, L2),
  length(R, T).

% Queremos analizar el caso de sublista(-Descartar, +Tomar, +L, +R).
% El predicado sublista/4 como fue implementado es reversible en su primer y cuarto argumento, es decir puede ser utilizado para decidir si una lista es sublista de otra.
% Como sublista/4 utiliza predicados reversibles en todos sus parámetros, la consulta se realizará sin problemas.
% Al no instanciar el primer parámetro, el predicado intentará descartar hasta encontrar el posible principio de la sublista consultada.
% Si hay múltiples inicios posibles, habrá posiblemente múltiples soluciones (si efectivamente la sublista aparece repetida (2)).
% Si la consulta tiene éxito, esos intentos serán los valores que podrá tomar Descartar. 
% Luego, la consulta será exitosa si el restante luego del descarte es la sublista consultada. Esta sublista ahora viene instanciada en R, (1) (o la sublista y algo más) y habrá una unica solución.
% Si al descartar nunca nos encontramos con el inicio de la sublista (4), o al descartar el restante es mas pequeño que Tomar (3), no habrá solución.

% (*1)
% ?- sublista(X, 2, [a,b,c,d], [c,d]).
% X = 2

% (*2)
% ?- sublista(X, 1, [a,b,a,b], [b]).
% X = 1
% X = 3

% (*3)
% ?- sublista(X, 4, [a,b], [a,b,c,d]).
% false

% (*4)
% ?- sublista(X, 2, [a,b,c], [d]).
% false

% ................................

% Ej 2
%tablero(+K, -T)
tablero(K, T) :-
  K > 0,
  length(T, 5),
  maplist(columnas(K), T).

columnas(K, L) :-
  length(L, K).

% Ej 3
%dimensiones(+M, -F, -C)
dimensiones(M, F, C) :-
  length(M, F),
  maplist(columnas(C), M).

% Ej 4
%coordenadas(+T, -IJ)
coordenadas(T, (I, J)) :-
  dimensiones(T, _, C),
  between(1, 5, I),
  between(1, C, J).

% Ej 5
%kPiezas(+K, -PS)
kPiezas(K, PS) :-
  nombrePiezas(Piezas),
  kAux(K, Piezas, PS).

kAux(0, _, []).
kAux(K, [X | Piezas ], [X |PS]) :-
  K > 0,
  length(Piezas, Y),
  K1 is K-1,
  Y >= K1,
  kAux(K1, Piezas, PS).
kAux(K, [_ | Piezas ], PS) :-
  K > 0,
  length(Piezas, Y),
  Y >= K,
  kAux(K, Piezas, PS).

% Ej 6
%seccionTablero(+T, +Alto, +Ancho, +IJ, ?ST)
seccionTablero(T, Alto, Ancho, (I, J), ST) :-
  I1 is I - 1,
  sublista(I1, Alto, T, FilasSeleccionadas),
  maplist(subcolumna(J, Ancho), FilasSeleccionadas, ST).

%subcolumna(+Inicio, +Largo, +Fila, -SubFila)
subcolumna(Inicio, Largo, Fila, SubFila) :-
    I1 is Inicio - 1,
    sublista(I1, Largo, Fila, SubFila).

% Ej 7
%ubicarPieza(+Tablero, +Identificador)
ubicarPieza(T, ID) :-
  pieza(ID, E),
  dimensiones(E, F, C),
  coordenadas(T, (I, J)),
  seccionTablero(T, F, C, (I, J), E).

% Ej 8
%ubicarPiezas(+Tablero, +Poda, +Identificadores)
ubicarPiezas(_, _, []).
ubicarPiezas(T, Poda, [ID | IDS]) :-
  ubicarPieza(T, ID),
  poda(Poda, T),
  ubicarPiezas(T, Poda, IDS).

%poda(+Poda, +Tablero)
poda(sinPoda, _).
poda(podaMod5, T) :- todosGruposLibresModulo5(T).

% Ej 9
%llenarTablero(+Poda, +Columnas, -Tablero)
llenarTablero(Poda, Columnas, T) :-
  tablero(Columnas, T),
  kPiezas(Columnas, IDS),
  ubicarPiezas(T, Poda, IDS).

% Ej 10
cantSoluciones(Poda, Columnas, N) :-
  findall(T, llenarTablero(Poda, Columnas, T), TS),
  length(TS, N).

% ?- time(cantSoluciones(sinPoda, 3, N)).
% 38,415,870 inferences, 2.297 CPU in 2.735 seconds (84% CPU, 16725277 Lips)
% N = 28.

% ?- time(cantSoluciones(sinPoda, 4, N)).
% 1,505,808,293 inferences, 90.531 CPU in 98.922 seconds (92% CPU, 16633022 Lips)
% N = 200.

% Ej 11
%todosGruposLibresModulo5(+Tablero)
todosGruposLibresModulo5(T) :-
  findall((I, J), casillaLibre(T, (I, J)), Libres),
  agrupar(Libres, Grupos),
  forall(member(G, Grupos), (length(G, N), mod(N, 5) =:= 0)).

casillaLibre(T, (I, J)) :-
  nth1(I, T, Fila),
  nth1(J, Fila, Casilla),
  var(Casilla).

% ?- time(cantSoluciones(podaMod5, 3, N)).
% 17,435,859 inferences, 1.078 CPU in 1.178 seconds (91% CPU, 16172391 Lips)
% N = 28.

% ?- time(cantSoluciones(podaMod5, 4, N)).
% 359,313,587 inferences, 20.875 CPU in 21.888 seconds (95% CPU, 17212627 Lips)
% N = 200.
