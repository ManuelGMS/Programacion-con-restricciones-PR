% MANUEL GUERRERO MOÑUS
% ALEJANDRO CILLEROS GARRUDO

  % EJC 1
include "alldifferent.mzn";

int: N = 5;
array [1..N] of var 1..N: t;
array [1..N] of var 1..N: s;

constraint alldifferent(t);

constraint exists(c in 1..N) (s[c] != t[c]); 

constraint exists(k in 1..N) (forall (i in 1..N) (s[i] = t[((i+k) mod N) + 1]));

  % EJC 2
  
include "alldifferent.mzn";

int: N = 5;
array [1..N] of var 1..N: t;

constraint alldifferent(t);

constraint exists(j in 1..N, i in 1..j-1) (t[i] > t[j]);

constraint forall (i in 1..N-1) (exists (j in i+1..N) ((t[i] mod t[j]) == 0));

  % EJC 3
include "alldifferent.mzn";

int: N = 6;
array [1..N] of var 1..N: t;

constraint alldifferent(t);

constraint t[5] - t[4] == 1 /\ t[6] - t[5] == 1;

constraint t[2] = 1;

constraint forall(i in 1..N) (t[i] != i);

constraint t[4] > t[3] /\ t[4] > t[2];

  % EJC 4
  
par int:N=6;
array [1..N,1..N] of var 0..1: t;

var int: num_reinas;

constraint num_reinas = sum(i,j in 1..N)(if t[i,j] == 1 then 
                             
              (sum(h in 1..N where j != h)(t[i,h] == 1) +
              sum(h in 1..N where i != h)(t[h,j] == 1) +
              sum(h in 1..N where abs (i-h) >= 1 /\ j+h <= N)(t[i-h,j+h] == 1) +
              sum(h in 1..N where i+h <= N /\ j+h <= N)(t[i+h,j+h] == 1) +
              sum(h in 1..N where i+h <= N /\ abs (j-h) >= 1)(t[i+h,j-h] == 1) +
              sum(h in 1..N where abs (i-h) >= 1 /\ abs (j-h) >= 1)(t[i-h,j-h] == 1)) == 2 else 0 endif);
              
output([show(t[i,j])++" "++ (if (j==N) 
                      then "\n" 
                      else " " 
                      endif) | i in 1..N,j in 1..N]);

output(["Num reinas: " ++ show(num_reinas)]);

solve maximize num_reinas;