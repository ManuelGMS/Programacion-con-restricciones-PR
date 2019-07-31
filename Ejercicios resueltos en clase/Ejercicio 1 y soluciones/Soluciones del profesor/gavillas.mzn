int:gavillas=100;
int:personas=100;
int:gh = 3;
int:gm = 2;
float:gn = 0.5;

var int:h;
var int:m;
var int:n;

constraint h>=0 /\ m>=0 /\ n>=0;
constraint h+m+n=personas;
constraint  gh*h+gm*m+gn*n = gavillas;
constraint 5*h==m;

solve satisfy;