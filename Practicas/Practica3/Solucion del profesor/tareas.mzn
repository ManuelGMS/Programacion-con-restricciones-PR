int:N=6;
array [1..N] of var 1..N: t;

% tareas distintas
constraint forall(i,j in 1..N where i<j)(t[i]!=t[j]);

%
constraint t[6]==t[5]+1 /\ t[5]==t[4]+1;
constraint t[2]=1 ;
constraint forall(i in 1..N)(t[i]!=i);
constraint t[4] > t[3] /\ t[4]>t[2] ;
solve satisfy;