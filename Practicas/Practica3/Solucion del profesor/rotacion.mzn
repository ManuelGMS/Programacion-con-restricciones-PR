include "alldifferent.mzn";
int: n=3;
array [1..n] of var 1..n:t;

array [1..n] of var 1..n: s; 

constraint alldifferent(t);
constraint exists(i in 1..n)(t[i]!=s[i]);
constraint exists(k in 1..n)(forall(i in 1..n)(s[i]=t[((i+k) mod n)+1]));

solve satisfy;

