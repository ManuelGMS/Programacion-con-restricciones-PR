% Queremos encontrar un array t de tamaño NxN en los que cada casilla puede tomar los valores 0,2,3 o 5. En particular, la casilla t[i,j] contendrá k con k en {2,3,5} si i+j es divisible entre k, o 0 en caso contrario. 

int:N=4;

array [1..N, 1..N] of var 0..5: t;

% ni 1 ni 4
constraint forall (i,j in 1..N)(t[i,j]!=1 /\ t[i,j]!=4);

% el valor que toca según el enunciado
constraint forall (i,j in 1..N)
                  (t[i,j] = if (i+j) mod 2 ==0 then 2
                            elseif (i+j) mod 3 == 0 then 3
                            elseif (i+j) mod 5 == 0 then 5
                            else 0                                     
                            endif);
solve satisfy;
output(
       [show(t[i,j])++" "++
        (if (j==N) then "\n" else "  " endif) | i in 1..N,j in 1..N]);	

