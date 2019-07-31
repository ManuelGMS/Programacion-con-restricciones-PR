int: n=3;
array [1..n] of var 2..7: t; % dos sols. [4,6,2] y [6,4,2]

constraint forall(i,j in 1..n where i<j)(t[i]!=t[j]);
constraint exists(i,j in 1..n where i<j)(t[i]>t[j]);
constraint forall(i in 1..n-1)(exists (j in i+1..n)((t[i] mod t[j]) ==0));

solve satisfy;

