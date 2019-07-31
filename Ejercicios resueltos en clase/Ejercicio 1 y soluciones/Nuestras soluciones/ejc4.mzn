include "alldifferent.mzn";
var 1..8: A;
var 1..8: B;
var 1..8: C;
var 1..8: D;
var 1..8: E;
var 1..8: F;
var 1..8: G;
var 1..8: H;

constraint alldifferent([A,B,C,D,E,F,G,H]);
constraint abs(A-B) != 1 /\ abs(A-C) != 1 /\ abs(A-D) != 1;
constraint abs(B-C) != 1 /\ abs(B-E) != 1 /\ abs(B-F) != 1;
constraint abs(C-D) != 1 /\ abs(C-E) != 1 /\ abs(C-F) != 1 /\ abs(C-G) != 1;
constraint abs(D-F) != 1 /\ abs(D-G) != 1;
constraint abs(E-F) != 1 /\ abs(E-H) != 1;
constraint abs(F-G) != 1 /\ abs(F-H) != 1;
constraint abs(G-H) != 1;

solve satisfy;
