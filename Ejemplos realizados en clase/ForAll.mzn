int : n = 5;

array[1..n] of var 1..10:s;

/*
forall es un Y generalizado, este es true si
todos los elementos de la lista son true (obliga a true)
*/
constraint forall( [ s[i] < s[i+1] | i in 1..n-1 ] );

/*
Por debajo esto se traduciria como:
constraint forall( s[1] < s[2] , s[2] < s[3] , s[3] < s[4] , s[4] < s[5] )
Que pasaria a:
constraint s[1] < s[2] /\ s[2] < s[3] /\ s[3] < s[4] /\ s[4] < s[5]
*/

output [ show(x*x) ++ " " | x in 1..5 ]; % show(var) --> string

solve satisfy;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Simular el "alldistinct"

int : n = 5;

array[1..n] of var 1..10:s;

constraint forall( [ s[i] < s[j] | i in 1..n , j in i+1..n ] );

solve satisfy;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

int : n = 5;

array [1..n,1..n] of var 1..10: s;


% Hacer que todas las filas del vector tengan numeros diferentes.
/*
constraint forall( [s[f,i] != s[f,j] | f in 1 .. n ,
                                          i in 1..n , 
                                            j in i+1..n] );


output [ show(s) ++ "\n" ];
*/

constraint forall( [s[i,c] != s[j,c] | c in 1 .. n ,
                                          i in 1..n , 
                                            j in i+1..n] );

/*
Equivalente anterior simpllificado:
constraint forall( [s[i,c] != s[j,c] | c,i,j in 1..n where i < j ] );
*/

solve satisfy;

output [ show(s) ++ "\n" ];

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

int : n = 5;

array [1..n] of var 1..10 : s;

constraint forall( [ s[i] != 5 | i in 1..n ] );

solve satisfy;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

int : n = 5;

array [1..n] of var 1..10 : s;

% Esta version de forall se traduce a una lista intensional.
constraint forall(i in 1..n /*WHERE*/)(s[i]!=5);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

int : n = 5;
array [1..n,1..n] of var 1..10 : s;

constraint forall( [s[f,i] != s[f,j] | f in 1 .. n ,
                                          i in 1..n , 
                                            j in i+1..n] );

/*
Comando equivalente al anterior:
constraint forall(c,i,j in 1..n where i < j)([s[f,i] != s[f,j]);
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

int: n = 5;

array [1..n,1..n] of var 1..10:s;

  % forall sera un y generalizado, es decir, todos los elementos de la lista tienen que ser true
  % constraint s[1]<s[2] /\ s[2] < s[3] ...

  % Diferentes por filas
constraint forall([s[f,i] != s[f,j] | f in 1..n, 
                                         i in 1..n, 
                                            j in i+1..n]);

  % Diferentes por columnas                            
constraint forall([s[i,c] != s[j,c] | c in 1..n, 
                                         i in 1..n, 
                                            j in i+1..n]);

  % Mismo codigo que diferentes por columnas
constraint forall([s[i,c] != s[j,c] | c, i in 1..n, j in i+1..n where i < j]);


solve satisfy;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Array de constantes
array [1..3] of 1..n: s = [1,2,3]; 

  % Lo mismo
constraint forall([s[i] != 5 | i in 1..n]); 
constraint forall(i in 1..n)(s[i] != 5);
  
  % Ejc de filas 
constraint forall(f in 1..n, i in 1..n, j in i+1..n)(s[f,i] != s[f,j]);
