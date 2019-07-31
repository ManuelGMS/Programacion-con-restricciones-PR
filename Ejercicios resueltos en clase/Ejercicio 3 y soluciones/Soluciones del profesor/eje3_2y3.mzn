include "alldifferent.mzn";

int:n=8;
array [1..n,1..n] of var 1..n*n:t;
 int:suma = n*(n*n+1) div 2;
constraint alldifferent(t);

constraint forall(f in 1..n)(sum([t[f,c] | c in 1..n])==suma);
constraint forall(c in 1..n)(sum([t[f,c] | f in 1..n])==suma);


output([show(t[i,j]) ++         
           (if (j==n) then "\n" else "  " endif) | i in 1..n,
                                                   j in 1..n]); 