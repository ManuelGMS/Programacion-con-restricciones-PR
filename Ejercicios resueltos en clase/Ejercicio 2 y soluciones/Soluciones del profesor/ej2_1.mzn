par int:M=100;
par int:N=5;
array [1..N,1..N] of var int: a;

constraint sum(a)=M;
constraint forall(i,j in 1..N)(a[i,j]>=1 /\ a[i,j]<=N);
constraint forall(i in 1..N)(exists(j,k in 1..N)(a[j,k]=i));
solve minimize max(a);

output(
     [show(a[i,j])++" "++
      (if (j==N) then "\n" else "  " endif) | i in 1..N,j in 1..N]);
