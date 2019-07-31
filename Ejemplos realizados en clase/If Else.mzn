var -10..10 : a;
var -10..10 : b;

%b es 1 si par(a) y 0 si !par(a)

%constraint a mod 2 == 0 -> b == 1 /\ a mod 2 == 1 -> b == 0

constraint if a mod 2 == 0 then b == 1 else b == 0;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

var -10..10 : a;
var -10..10 : b;

constraint (a == 4 -> b == 5) /\ (a != 4 -> (b >= 0 /\ b <=3 /\ a > b /\ a <b+2)); % IF-ELSE

%constraint if a == 4 then b == 5 else (a != 4 -> (b >= 0 /\ b <=3 /\ a > b /\ a <b+2)) endif;
