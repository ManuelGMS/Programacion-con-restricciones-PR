   % EJC 1
  
int: n = 5;
array [1..n,1..n] of var bool: t;

var int: d1;
var int: d2;

constraint forall(c in 1..n)(sum([t[f,c] | f in 1..n]) == 1);
constraint d1 = sum([t[i,i] | i in 1..n]);
constraint d2 = sum([t[i,n-i+1] | i in 1..n]);

solve minimize abs(d1 - d2);

output(
 [show(if (t[i,j]) then 1 else 0 endif)++
 (if (j==n) then "\n" else " " endif) | i in 1..n,j in 1..n]);
 
   % EJC 2
   
include "alldifferent.mzn";

int: n = 3;
array [1..n,1..n] of var 1..9: t;

constraint alldifferent(t);
var int: suma = (n*((n*n) +1)) div 2;
constraint forall(f in 1..n)(sum([t[f,c] | c in 1..n]) == suma) /\ forall(c in 1..n)(sum([t[f,c] | f in 1..n]) == suma) ;


output(
 [show(t[i,j])++
     (if (j==n) then "\n" else " " endif) | i in 1..n,j in 1..n]);
     

  % EJC 4
  
% Use this editor as a MiniZinc scratch book
include "alldifferent.mzn";

int: n = 10;
array [1..n,1..n] of var bool: t;
array [1..n] of var int: suma;

constraint forall([t[i,i] | i in 1..n]);
constraint forall(a,b,c,d in 1..n where a<b /\ b < c /\ c < d)(exists(i in [a,b,c,d])
                                                                     (t[i,a] /\ t[i,b] /\ t[i,c] /\ t[i,d]));

var int: max;
var int: min;

constraint forall(i in 1..n)(suma[i] = sum([ t[i,j] | j in 1..n ] ));
constraint max = max(suma);
constraint min = min(suma);

% solve minimize min;

output(
 [show(if (t[i,j]) then 1 else 0 endif)++
 (if (j==n) then "\n" else " " endif) | i in 1..n,j in 1..n]);
 
 output( [show(max)++" <--> "++show(min) ] );