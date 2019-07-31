%% int suma (int n) {
%%   s = 1;
%%   for (int x = 1; x <= n; x++)
%%        s += x;
%%   return s
%% }
%% esperamos que suma(n) = n*(n+1) div 2 para todo n (1..10)

predicate suma(var int: n, var int:r) = r = 1+sum(x in 1..n)(x);

var 1..10: n;
var int: obtained;
var int: expected;

constraint expected = n*(n+1) div 2;

% Si encuentro algun valor que cumple este constraint, es que esta mal
constraint suma(n,obtained) /\ obtained != expected; 
