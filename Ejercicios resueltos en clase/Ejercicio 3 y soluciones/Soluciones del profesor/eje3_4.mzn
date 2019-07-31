int:n=10;
array[1..n,1..n] of var bool:t;
array[1..n] of var int:suma;  % suma[1] es la suma de la fila 1
var int:masconocidos;
var int:menosconocidos;

constraint forall(i in 1..n)(suma[i] = sum([t[i,j] | j in 1..n]));
constraint masconocidos = max(suma);
constraint menosconocidos = min(suma);

%solve minimize menosconocidos;
% conocete a ti mismo
constraint forall(i in 1..n)(t[i,i]=1);

constraint forall(a,b,c,d in 1..n where a<b /\ b<c /\ c<d)
                 (
                   exists(i in [a,b,c,d])
                         (t[i,a] /\ t[i,b] /\ t[i,c] /\ t[i,d])
                 );
                 
output([show(masconocidos)," ", show(menosconocidos),"\n"]);
output([show(if (t[i,j]) then 1 else 0 endif)++         
           (if (j==n) then "\n" else "  " endif) | i in 1..n,
                                                   j in 1..n]); 