int : MAX_DIAS = 15;

float : TOTAL_HIERRO = 40.0 * 2;
float : TOTAL_CARBON = 13.0 * 2;

var float : gano;
var float : a; % Toneladas de producto A
var float : b; % Toneladas de producto B
var int : dias;


constraint a >= 0 /\ b >= 0; % Solo se pueden no negativas.

constraint dias = a * 3 + b * 4;

constraint a * 3 + b * 4 <= MAX_DIAS;

constraint gano = 18.5 * a + 20.7 * b;

constraint 14 * a + 2.1 * b <= TOTAL_HIERRO;

constraint 2 * a + 11 * b <= TOTAL_CARBON;

solve maximize gano;