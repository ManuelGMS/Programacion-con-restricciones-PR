% alldifferent en un array del que no conocemos todos los elementos.

include "alldifferent.mzn";

int : n = 10;

array [1..3] of var 8..10: s = [_,9,_];

constraint alldifferent(s);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Para un tablero
array [1..8,1..8] of var 1..6: t;

% Array de constantes
array [1..3] of 1..n: s = [1,2,3]; 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

include "alldifferent.mzn";
int:n = 10;
  % Array de 3 valores en el que cada cual puede tomar valores de 4 a n
array [1..3] of var 4..n: s; 

constraint alldifferent(s);
solve satisfy;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

include "alldifferent.mzn";
int:n = 10;
  % Array de constantes
array [1..3] of var 8..n: s = [_,9,_]; 

constraint alldifferent(s);
solve satisfy;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

include "alldifferent.mzn";

  % Array bidimensional de 3 filas por 2 columnas
array [1..3,1..2] of var 0..500: u = [| 1,_, | 4,_, | 6,7 |];
constraint alldifferent(u);

solve satisfy;

