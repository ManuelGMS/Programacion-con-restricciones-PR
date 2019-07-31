% Sacar los multiplos de 2 existentes entre el 1 y el 20.

output [ show(x)++" " | x in 1..20 where x mod 2 == 0 ];
output ["\n"];
output [ show(2*x)++" " | x in 1..10 ];

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Listas intensionales

output [show(x)++" "| x in 1..5];
output [show(x*y) ++ " " | x in 1..5, y in 1..6];

solve satisfy;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Para todo 1..10 mostrar los sus divisores
output [ show(x) ++ " divisible por " ++ show(y) ++ "\n" | x in 1..10 , y in 1..x where x mod y == 0 ];

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
Queremos que el array solo tenga un true
int : k = 5;
array [1..k] of var bool : b;
constraint 1 == sumb(b);
*/

int : k = 10;
array [1..k] of var 1..10 : c;

% 1 -> 2 iguales
% 0 -> ALLDIFFERENT
constraint sum ([ c[i] == c[j] | i,j in 1..k where i<j ]) == 0;

solve satisfy;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  % Ejemplo muestrame una lista de todos los numeros pares del 1 al 20

  % Con where
output [ show(x)++" " | x in 1..20 where x mod 2 == 0];
  % Sin where
output [show(2*x)++" " | x in 1..10];

  % Para cada numero del 1 al 10, mostrar sus divisores del 1 al 10
output [" Numero "++show(x)++" Divisor: "++show(y)++"\n" | x in 1..10, y in 1..x where x mod y == 0]; 

solve satisfy;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  % Listas intensionales
int: n = 5;

array [1..n] of var 1..10:s;

  % forall sera un y generalizado, es decir, todos los elementos de la lista tienen que ser true
  % constraint s[1]<s[2] /\ s[2] < s[3] ...
constraint forall([s[i] < s[i+1] | i in 1..n-1]);
constraint forall([s[i] != s[j] | i in 1..n, j in i+1..n]);

solve satisfy;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

int: k = 5;
array [1..k] of var bool:b;

  % El array tenga exactamente un true
constraint sum(b) == 1;

  % El array tiene exactamente dos valores iguales (1 valor repetido)
constraint sum([c[i] == c[j] | i,j in 1..k where i < j]) == 1;

solve satisfy;
