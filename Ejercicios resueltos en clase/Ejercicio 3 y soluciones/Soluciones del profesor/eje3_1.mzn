% Use this editor as a MiniZinc scratch book

int:n=5;
array [1..n,1..n] of var bool:t;

% cada columna un true
constraint forall(c in 1..n)(sum([t[f,c]| f in 1..n])==1);
%constraint 

solve minimize abs(sum([t[i,i] | i in 1..n])      % diagonal + 
                   -
                   sum([t[i,n-i+1] | i in 1..n])      % diagonal -
                   );

output([show(if (t[i,j]) then 1 else 0 endif)++         
           (if (j==n) then "\n" else "  " endif) | i in 1..n,
                                                   j in 1..n]); 