% Bipartido puede ser si no hay ciclo de longitud impar

% variable

var int : nc; % Tiene que ser un dominio finito

var int : a;
var int : b;
var int : c;
var int : d;
var int : e;

%restricciones

constraint a >= 1 /\ a <= nc;
constraint b >= 1 /\ b <= nc;
constraint c >= 1 /\ c <= nc;
constraint d >= 1 /\ d <= nc;
constraint e >= 1 /\ e <= nc;

constraint a != c /\ b != c /\ d != c /\ e != c;

constraint d != e; % Nueva arista
constraint b != e; % Nueva arista
constraint d != b; % Nueva arista

solve minimize nc;