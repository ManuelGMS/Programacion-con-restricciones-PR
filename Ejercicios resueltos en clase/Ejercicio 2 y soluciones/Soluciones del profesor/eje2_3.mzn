%Queremos encontrar un array de tamaño NxN con valores de 1..N en el que:
%- Si una casilla de una fila>1 contiene el valor N, la casilla de arriba (misma columna, fila una menos) tiene que ser un 1.
%- Si una casilla de una columna c, 1<c<N contiene un valor par p, entonces las casillas de la izquierda y de la derecha contienen los valores p-1 y p+1, respectivamente
%- Todos los números 1..N aparecen al menos 2 veces en el array
int:N=3;

array [1..N, 1..N] of var 1..N: t;

% toda fila tiene una columna con el valor N
constraint forall(i in 2..N, j in 1 ..N)(t[i,j]==N->t[i-1,j]==1); 
constraint forall(i in 1..N, j in 2..N-1, k in 1..N  where k mod 2==0 )(t[i,j]==k->t[i,j-1]==k-1 /\ t[i,j+1]==k+1); 
constraint forall(v in 1..N)(sum([ t[i,j]==v| i,j in 1..N])>=2); 
solve satisfy;

output(
       [show(t[i,j])++" "++
        (if (j==N) then "\n" else "  " endif) | i in 1..N,j in 1..N]);	

