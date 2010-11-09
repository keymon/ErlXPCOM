%% Comproba se un termino é ok ou unha tupla contendo ok.
%% Returns: O propio termino se conten ok
%% Throws: {not_ok, E} con E o propio termino
ok(T) when atom(T), T == ok -> T;
ok(T) when tuple(T), size(T) >= 1, element(1, T) == ok -> T;
ok(E) -> throw({not_ok, E}).

%% Comproba que un termino sexa unha tupla de 2 ou máis elementos
%% co 1º elemento =  ok
%% Returns: devolve o 2º elemento da tupla
%% Throws: {not_ok, E} con E o propio termino
ok2(T) when tuple(T), size(T) >= 2, element(1, T) == ok -> element(2, T);
ok2(E) -> throw({not_ok, E}).

%% Comproba que un termino sexa unha tupla de 3 ou máis elementos
%% co 1º elemento =  ok
%% Returns: devolve o 3º elemento da tupla
%% Throws: {not_ok, E} con E o propio termino
ok3(T) when tuple(T), size(T) >= 3, element(1, T) == ok -> element(3, T);
ok3(E) -> throw({not_ok, E}).

%% Comproba que un termino sexa unha tupla de 4 ou máis elementos
%% co 1º elemento =  ok
%% Returns: devolve o 4º elemento da tupla
%% Throws: {not_ok, E} con E o propio termino
ok4(T) when tuple(T), size(T) >= 4, element(1, T) == ok -> element(4, T);
ok4(E) -> throw({not_ok, E}).

-define(IF(P), case P of true ->).
-define(FI, ; false -> nop end).

