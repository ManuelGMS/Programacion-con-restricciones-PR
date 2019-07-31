int:n=10;
int:masgrande = 1000;

set of int:s = 1..n;
set of int:elementos = 1..masgrande;
%array [s] of var 1..1000:u;
array [s] of var elementos:u;
%array [s] of var 1..1000:v;
array [s] of var elementos:v;

constraint forall(i in s)(u[i]>i*v[i] /\ v[i]>i);

solve satisfy;

output [ "u["++show(i)++"]= "++show(u[i])++
         " v["++show(i)++"]= "++show(v[i])++"\n" | i in s]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

var set of 1..10:s;
var set of 1..10:t;

constraint card(s) = 6;
% constraint 4 in s;
% constraint (4 in s) == false;
constraint not (4 in s);
constraint t subset s /\ t != s;
constraint {1,8} subset t;

