int:N=4;
array [1..N] of var 2..N: t;

constraint exists(i in 1..N)(sum([t[j] | j in 1..N where j!=i])==3*N-1);

solve satisfy;