% MANUEL GUERRERO MOÑÚS
% ALEJANDRO CILLEROS GARRUDO

%%%%%% Poner aquí los include necesario
include "cumulative.mzn";
include "circuit.mzn";
%%%%%%% Esto no se toca %%%%%%%

int:N=6;  % total de camiones
set of int: CAMIONES=1..N;


int:D=13;
set of int:DESTINOS=1..D;
array [DESTINOS, 1..4] of int:d  = 
        % ind, Nºcamiones Tiempo Destino
        [| 1,      1,     4,  7 
         | 2,      2,     3,  8
         | 3,      3,     4,  4
         | 4,      2,     5,  10
         | 5,      4,     3,  9
         | 6,      5,     2,  2
         | 7,      2,     2,  11
         | 8,      6,     2,  12
         | 9,      1,     6,  1
         | 10,     4,     2,  5
         | 11,     3,     2,  6
         | 12,     2,     2,  13
         | 13,     3,     2,  3
         |];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1
array [DESTINOS] of var int: t; % Tiempo para cada viaje
constraint forall(i in DESTINOS)(t[i] = d[i,3]);
% 2
set of int: instante = 0..sum(i in DESTINOS)(d[i,3]);
array [DESTINOS] of var instante: s; % Dia en el que parte cada viaje
constraint forall(i in DESTINOS)(s[i] >= 0);
% 3
array [DESTINOS] of var int: r; % Camiones que necesita cada viaje
constraint forall(i in DESTINOS)(r[i] = d[i,2]);
% 4
constraint cumulative (s, t, r, N);

%solve satisfy;

% 5
array [DESTINOS] of var 1..sum(i in DESTINOS)(d[i,3]): a;
constraint forall(i in DESTINOS)(a[i] = s[i] + t[i]); 
% 6
var int: fin;
constraint fin = max(a);

solve minimize fin;
% 7
array [DESTINOS] of var int: nodos;
constraint forall(i in DESTINOS)(nodos[i] = d[i,4]);
constraint circuit(nodos);
% 8
output["Tiempo total: "++show(fin)++"\n" ]++
      [ (if j>=fix(s[i]) /\ j < fix(s[i])+fix(t[i])
         then "|*"
         else "| " endif) ++
        (if j==fix(fin)-1
         then "|\n"
         else "" endif)       | i in DESTINOS, j in 0 .. fix(fin)-1];