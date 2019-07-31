include "alldifferent.mzn";

int: N=5;
array [1..N] of var 1..10:r1;
array [1..N] of var 1..20:r2;

predicate esta(var int:x, array[int] of var 1..10:v) =
                exists(i in index_set(v))(v[i] == x);

% a y b permutaciones
predicate permut(array[int] of var int:a, array[int] of var int:b) =
                forall(i in index_set(a))(esta(a[i],b));
                                
                                                                                                
var 1..10: a;

constraint alldifferent(r1) /\ alldifferent(r2);
constraint r1[1] != r2[1];

constraint permut(r1,r2);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

include "alldifferent.mzn";

int: N=5;
array [1..N] of var 1..10:r1 = [4,2,3,8,1];
array [1..N] of var 1..20:r2;

predicate esta(var int:x, array[int] of var 1..10:v) =
                exists(i in index_set(v))(v[i] == x);

% a y b permutaciones
predicate permut(array[int] of var int:a, array[int] of var int:b) =
                forall(i in index_set(a))(esta(a[i],b));
                                
predicate ordenado(array[int] of var int:a) = 
                forall(i in index_set(a) where i != length(a))(a[i] <= a[i+1]);
%predicate ordenado(array[int] of var int:a) = 
%forall(i in index_set(a) where i != max(index_set(a)))(a[i] <= a[i+1]);
                                                                                                                                                                  
var 1..10: a;

constraint alldifferent(r1) /\ alldifferent(r2);
constraint r1[1] != r2[1];
constraint permut(r1,r2) /\ ordenado(r2);
