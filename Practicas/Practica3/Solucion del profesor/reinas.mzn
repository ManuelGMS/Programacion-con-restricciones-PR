
int:n=6;

array [1..n, 1..n] of var bool: t;

var int:nreinas; 

constraint nreinas = sum(t);
constraint forall(i,j in 1..n where t[i,j])(% para toda casilla
            sum(a,b in 1..n where t[a,b] /\ (a!=i \/ b!=j)) % existe una reina
                ((a==i) \/ % en la misma fila, o
                (b==j) \/ % en la misma columna, o
                abs(a-i)==abs(b-j) % en la misma diagonal
                )==2
           );                    

solve maximize nreinas;

output(["num. reinas "++show(nreinas)++"\n"]++
       [show(if (t[i,j]) then 1 else 0 endif)++
        (if (j==n) then "\n" else "  " endif) | i in 1..n,j in 1..n]);	
%-----------------