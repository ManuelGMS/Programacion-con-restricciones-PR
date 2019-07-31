int:gavillas=100;
int:mgavillas = gavillas*2;
int:personas=100;
int:gh = 3*2;
int:gm = 2*2;
int:gn = 1;

var int:h;
var int:m;
var int:n;

constraint h>=0 /\ m>=0 /\ n>=0;
constraint h+m+n=personas;
constraint  gh*h+gm*m+gn*n = mgavillas;
constraint 5*h==m;

solve satisfy;