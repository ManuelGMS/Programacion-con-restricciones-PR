  % PREDICADOS

predicate esPar(var int:x) = x mod 2 == 0;

var int: a;
var int: b;

constraint esPar(a) /\ esPar(b) /\ a != b;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
% Predicado que recive dos enteros y se asegura de que su suma no es cero
predicate sumaNoCero(var int:x, var int:y) = (x + y) != 0;

var -20..20: a;
var -20..20: b;

constraint esPar(a) /\ esPar(b) /\ sumaNoCero(a,b);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
%%% test
%%% int f(int x) { if (x > 0) return -x; else return x; }

predicate f_if(var int: x, var int: r) = 
      x > 0 /\ r = -x;

predicate f_else(var int: x, var int: r) = 
      not(x > 0) /\ r = x; 
      

var -2..2: a;
var int: r_if;
var -2..2: b;
var int: r_else;

constraint f_if(a, r_if);

constraint f_else(b, r_else);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

array [1..5] of var 1..10:r1;
array [1..10] of var 1..20:r2;

predicate esta(var int:x, array[int] of var 1..10:v) =
                exists(i in index_set(v))(v[i] == x);
                
var 1..10: a;

constraint esta(a,r1) /\ a > 1;

constraint esta(a,r2) /\ a > 1;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

predicate ordenado(array[int] of var int:a) = 
            let {int:ultimo = max(index_set(a))} in
                forall(i in index_set(a) where i != length(a))(a[i] <= a[i+1]);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set of int:CHAR = 0..5;
int: N = 5;
array [1..N] of var CHAR:s1;
array [1..N] of var CHAR:s2;

% Asegurarse de que:
% Contiene al menos un 0
% A partir del primer 0, todos los demas son 0
% [1,3,0,0,0]
predicate my_string(array[int] of var CHAR: s) =
          exists(i in index_set(s))(s[i] == 0 /\ forall(j in i+1..length(s))(s[j] == 0) /\forall(j in 1..i-1)(s[j] != 0));

predicate len(array[int] of var CHAR: s, var int: l2) = 
        let { var int: l } in 
          l >= min(index_set(s)) /\ l <= max(index_set(s)) /\
          forall(p1 in index_set(s) where p1 < l)(s[p1] != 0) /\ 
          forall(p2 in index_set(s) where p2 >= l)(s[p2] == 0) /\ l2 = l - 1;

constraint my_string(s1) /\ my_string(s2); 

var int: l1;
var int: l2;

constraint len(s1,l1) /\ len(s2,l2) /\ l2>l1; 
constraint s1[1] = 3 /\ s1[3] = 4;
