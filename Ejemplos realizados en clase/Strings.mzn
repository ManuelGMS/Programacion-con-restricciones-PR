
set of int:CHAR = 0..5;
int:N=5;
array [1..N] of var CHAR:s1;
array [1..N] of var CHAR:s2;

% asegurarse de que:
% 1) contiene al menos un 0
% 2) A partir del 0 todos los demas son 0
% [1,3,0,0,0]
% existe una posición tal que desde ella hasta el final todo son 0s
predicate my_string(array [int] of var CHAR:s) =
  exists(p in index_set(s))(
    forall(p1 in index_set(s) where p1<p)(s[p1]!=0) /\
    forall(p2 in index_set(s) where p2>=p)(s[p2]==0)     
  );
predicate len(array [int] of var CHAR:s, var int:l2) = 
    let {var int:l, set of int:indices=index_set(s)} in
    l >= min(indices) /\ l <= max(indices) /\
    forall(p1 in indices where p1<l)(s[p1]!=0) /\
    forall(p2 in indices where p2>=l)(s[p2]==0) /\ 
    l2 = l-1;   

constraint my_string(s1) /\ my_string(s2);

var int:l1;
var int:l2;
constraint len(s1,l1) /\ len(s2,l2) /\ l2>l1;
constraint len(s1,3);