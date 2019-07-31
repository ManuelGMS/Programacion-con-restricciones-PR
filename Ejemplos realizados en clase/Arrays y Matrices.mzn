int:k=5;
array [1..k] of var bool:b;
% el array tenga exactamente un true
constraint sum(b)==1;

array [1..k] of var 1..10:c;
% el array tiene exactamente 2 valores iguales (1 valor repe)
constraint sum([c[i]==c[j] | i,j in 1..k where i<j])==1;
constraint sum(i,j in 1..k where i<j)(c[i]==c[j])==1;

int:m=5;
array [1..m] of var 1..10:a;
% s[i] != 5 para todo i=1..n
%constraint forall([ a[i]!=5| i in 1..m ] );
constraint forall(i in 1..m)(a[i]!=5);

% Para cada número del 1 al 10, mostrar sus divisores
%output [ show(i)++":"++show(j)++"\n" | i,j in 1..10 where i mod j==0 ] ;

% números pares del 1 al 20 inclusive
%output [show(2*i)++" " | i in 1..10];
%output [show(i)++" " | i in 1..20 where i mod 2 ==0];

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% diferente por filas y columnas
int:n=5;

array [1..n,1..n] of var 1..10:s;

% diferentes por columnas
constraint forall(c,i,j in 1..n where i<j)(s[i,c]!=s[j,c]);
constraint forall(c in 1..n)
              (forall(i in 1..n)
                 (
                   forall (j in 1..n where i<j)
                          (s[i,c]!=s[j,c])
                  )
               );

constraint forall([ s[i,c]!=s[j,c] | c,i,j in 1..n where i<j]);

% diferentes por filas
constraint forall([ s[f,i]!=s[f,j] | f in 1..n, 
                                        i in 1..n, 
                                           j in i+1..n]);


%constraint forall([ s[i]!=s[j] | i in 1..n, j in i+1..n]);
% constraint forall(s[1]<s[2],s[2]<s[3],s[3]<s[4],s[4]<s[5]);

% constraint s[1]<s[2] /\ s[2]<s[3] /\ s[3]<s[4] /\ s[4]<s[5]

solve satisfy;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

int: n = 5;
set of int: filas = 1..n;
set of int: columnas = 1..3;

array [filas,columnas] of int: f = [|3,0,0 | 4,0,0 |1,1,0 |2,0,0 |2,0,0 |];
array [filas,columnas] of int: c = [|3,0,0 | 2,0,0 |2,0,0 |1,2,0 |3,0,0 |];

set of int: tamano = 1..n+1; % el +1 se hara como un truco para no salirme de los limites

array [tamano,tamano] of var bool: t; % Tablero

constraint forall(i = n + 1, j in tamano)(t[i,j] = false); 
constraint forall(j = n + 1, i in tamano)(t[i,j] = false);

array [filas] of var int: suma_f;
array [filas] of var int: suma_c;

%%% suma_f[i] = suma[f[i,1],f[i,2],f[i,3]]
constraint forall(i in filas)(suma_f[i] = sum(j in columnas)(f[i,j]));
constraint forall(i in filas)(suma_c[i] = sum(j in columnas)(c[i,j]));

constraint forall(i in filas)(suma_f[i] == sum(j in tamano)(t[i,j]));
constraint forall(i in filas)(suma_c[i] == sum(j in tamano)(t[j,i]));

array [filas] of var int: fv;
array [filas] of var int: cv;

constraint forall(i in filas)(fv[i] = sum(j in columnas)(f[i,j] != 0));
constraint forall(i in filas)(cv[i] = sum(j in columnas)(c[i,j] != 0));

constraint forall(i in filas)(sum(j in filas)(t[i,j] = true /\ t[i,j+1] == false) == fv[i]);
constraint forall(i in filas)(sum(j in filas)(t[j,i] = true /\ t[j+1,i] == false) == cv[i]);

output([show(if t[i,j] then 1 else 0 endif) ++ " " ++
           (if (j == n + 1) then "\n" else " " endif) | i in tamano, j in tamano]);
