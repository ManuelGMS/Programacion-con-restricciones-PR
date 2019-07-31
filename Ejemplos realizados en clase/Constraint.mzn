var -10..10 : a;
var -10..10 : b;

constraint (a = b*b*b) /\ (a*a > 0);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

var -10..10 : a;
var -10..10 : b;

constraint not(a==0) /\ not(b<a);
