%  ALEJANDRO CILLEROS GARRUDO
%  MANUEL GUERRERO MOÑÚS


int:N=5;

set of int: rango = 1..N;
set of int: orden = 1..N-1;

array [1..N,1..N] of var 1..N: t=
           [|_,_,_,_,_ |
             4,_,_,_,2 |
             _,_,4,_,_ |
             _,_,_,_,4 |
             _,_,_,_,_ |];

array [1..N,1..N-1] of 0..2:r = [| 1,0,1,1 |
                                   0,0,0,0 |
                                   0,0,0,0 |
                                   0,0,0,2 |
                                   2,2,0,0 |];                                                                                   

                           
constraint forall(i,j,z in rango where j < z)(t[i,j] != t[i,z]);
constraint forall(j,i,z in rango where i < z)(t[i,j] != t[z,j]);
constraint forall(i in rango, j in orden)(if (r[i,j] == 1) then (t[i,j] > t[i,j+1]) 
                                else (if (r[i,j] == 2) then (t[i,j] < t[i,j+1]) else t[i,j] != t[i,j+1] endif) endif);

output(["MATRIZ EJC 1: " ++ "\n"]);
output([show(t[i,j]) ++ " " ++(if (j != N /\ r[i,j] == 1) then ">" else (if (j != N /\ r[i,j] == 2) then "<" else " " endif) endif) ++
           (if (j ==N) then "\n" else " " endif) | i in rango, j in rango]); 
                                
   
      % EJC 2 ---------------------------------
                                                            
% 1 indica que la fila de arriba es menor que la de abajo
% 2 indica que la fila de arriba es mayor que la de abajo
                              
array [orden,rango] of 0..2:r_vertical = [| 0,0,1,0,0 |
                                            0,0,0,0,0 |
                                            0,0,0,1,0 |
                                            0,0,0,0,0 |];
                                    
array [1..N,1..N-1] of 0..2:r_horizontal = [| 0,0,0,0 |
                                              0,1,2,0 |
                                              0,0,0,1 |
                                              0,0,0,0 |
                                              1,0,2,0 |]; 
                                              
array [1..N,1..N] of var 1..N: t_2=
           [|_,_,_,_,_ |
             _,_,_,_,_ |
             _,_,4,_,_ |
             _,_,_,_,_ |
             _,_,_,_,4 |];                                                                          
                                                                                                                      
constraint forall(i,j,z in rango where j < z)(t_2[i,j] != t_2[i,z]);
constraint forall(j,i,z in rango where i < z)(t_2[i,j] != t_2[z,j]);
constraint forall(i in rango, j in orden)(if (r_horizontal[i,j] == 1) then (t_2[i,j] > t_2[i,j+1]) 
                                else (if (r_horizontal[i,j] == 2) then (t_2[i,j] < t_2[i,j+1]) else t_2[i,j] != t_2[i,j+1] endif) endif);

constraint forall(j in rango, i in orden)(if (r_vertical[i,j] == 1) then (t_2[i,j] < t_2[i+1,j]) 
                                else (if (r_vertical[i,j] == 2) then (t_2[i,j] > t_2[i+1,j]) else t_2[i,j] != t_2[i+1,j] endif) endif);


output(["\nMATRIZ EJC 2: " ++ "\n"]);
output([show(t_2[i,j]) ++ " " ++ (if (j != N /\ r_horizontal[i,j] == 1) then ">" else (if (j != N /\ r_horizontal[i,j] == 2) then "<" else " " endif) endif) ++ (if (j == N) then "\n" else " " endif) | i in rango, j in rango]); 