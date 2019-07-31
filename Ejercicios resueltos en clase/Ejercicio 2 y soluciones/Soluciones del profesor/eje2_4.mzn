int:N=3;

array [1..N, 1..N] of var 1..N: t;

% toda fila tiene una columna con el valor N
constraint forall(i in 1..N)(exists(j,k in 1..N where j<k)(t[i,j]>t[i,k])); 
constraint forall(i in 1..N)(exists(j,k in 1..N where j<k)(t[j,i]>t[k,i])); 
constraint forall(j in 1..N)(exists(i in 1..N)(t[i,j]==1)); 
constraint forall(i,j,k in 1..N where j<k)(t[i,j]!=t[i,k]);
constraint forall(i,j,k in 1..N where j<k)(t[j,i]!=t[k,i]);

solve satisfy;

output(
       [show(t[i,j])++" "++
        (if (j==N) then "\n" else "  " endif) | i in 1..N,j in 1..N]);	

