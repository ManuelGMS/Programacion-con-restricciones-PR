  % EJC 1

par int:M=100;
par int:N=5;
array [1..N,1..N] of var int: a;

%%% a) La suma de valores en a sea m
constraint sum(a) == M;

%%% b) Todos los valores en a estén entre 1 y N
constraint forall([a[i,j] <= N /\ a[i,j] >= 1| i, j in 1..N]); % Lo mismo a lo de abajo
constraint forall(i,j in 1..N)(a[i,j] >= 1 /\ a[i,j] <= N);

output([show(a[i,j])++" "++ (if (j==N) 
                             then "\n" 
                             else " " 
                             endif) | i in 1..N,j in 1..N]);

%%% Suma de los elemns de la primera fila es 20                             
%constraint sum([a[1,j] | j in 1..N]) == 20;

%%% Segunda fila todos menores que 5 
%constraint forall([a[2,j] < 5 | j in 1..N]);

%%% a contiene al menos un 3
%constraint exists([a[i,j] == 3 | i,j in 1..N]);

%%% c) Para todo valor de i de 1 a N, el valor i aparece al menos una vez en a.
constraint forall(h in 1..N)(exists([a[i,j] == h | i,j in 1..N]));
constraint forall([(exists([a[i,j] == h | i,j in 1..N])) | h in 1..N]);

%%% d) Los valores en a sean lo más pequeños posibles; si llamamos c al máximo de los valores en a, se trata de minimizar c.

solve minimize max(a);

  % EJC 2
par int:N=5;
array [1..N,1..N] of var 0..5: t;

constraint forall([
                    t[i,j] = if ((i+j) mod 2 == 0) then 2  
                    elseif ((i+j) mod 3 == 0) then 3  
                    elseif ((i+j) mod 5 == 0) then 5 
                    else 0 endif | i,j in 1..N]);
                    
output([show(t[i,j])++" "++ (if (j==N) 
                      then "\n" 
                      else " " 
                      endif) | i in 1..N,j in 1..N]);
                      
  % EJC 3
par int:N=5;
array [1..N,1..N] of var 1..N: t;

  % a)
%constraint forall([a[i-1,j] = if(a[i,j] == N) then 1 else a[i-1,j] endif | i in 2..N, j in 1..N]);
constraint forall(i in 2..N, j in 1..N)(
                          if t[i,j] == N then t[i-1,j] == 1 endif); % Es un contraint que se activa en algunos casos, por 
                                                                    % lo que no se necesita else

  % b)                                             
constraint forall(i in 1..N, j in 2..N-1)(
                          if t[i,j] mod 2 == 0 then t[i,j-1] == t[i,j] - 1 /\ t[i,j+1] == t[i,j] + 1 endif);
                          
  % c) 
%constraint forall(h in 1..N)([sum(t[i,j] == h) >= 2 | i,j in 1..N]);
constraint forall(h in 1..N)([sum(t[i,j] == h | i,j in 1..N) >= 2]);

                          
output([show(t[i,j])++" "++ (if (j==N) 
                      then "\n" 
                      else " " 
                      endif) | i in 1..N,j in 1..N]);
                      
                      
  % EJC 4  
par int:N=5;
array [1..N,1..N] of var 1..N: t;

  % todas las filas son diferentes
  % para todo f1 f2, existe c tal que t[f1,c] != t[f2,c]
  
constraint forall(f1, f2 in 1..N where f1<f2)(exists(c in 1..N) (t[f1,c] != t[f2,c]));    

