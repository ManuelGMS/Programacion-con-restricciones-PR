%Programación con restricciones - práctica 1

%#Nombre1: MANUEL GUERRERO MOÑÚS
%#Nombre2: ALEJANDRO CILLEROS GARRUDO
%#Nombre3:

%#Observaciones:

  %En el modelo 1, caballeros True y Duendes False

%#P1 Modelo:
var bool:A;
var bool:B;

var bool:DA;
constraint DA == (B == false);
constraint if A then DA else not(DA) endif;

var bool:DB;
constraint DB == (A == false);
constraint if B then DB else not(DB) endif;

solve satisfy;


%#P2 Modelo:
var bool:A;
var bool:DA;

var bool: diceHoy;
var bool: diceManana;

constraint dia == 1 -> diceHoy = true /\ diceManana = false;
constraint dia == 2 -> diceHoy = false /\ diceManana = true;
constraint dia == 3 -> diceHoy = true /\ diceManana = false;
constraint dia == 4 -> diceHoy = false /\ diceManana = false;
constraint dia == 5 -> diceHoy = false /\ diceManana = true;
constraint dia == 6 -> diceHoy = true /\ diceManana = false;
constraint dia == 7 -> diceHoy = false /\ diceManana = true;
constraint DA == (diceManana == true);
constraint (A = true) <-> (DA /\ (diceHoy == true));
constraint (A = false) <-> (not(DA) /\ (diceHoy == false));

solve satisfy;

%#P3 Modelo:
var 2..5:tenis;
var 2..5:futbol;
var 2..5:baloncesto;
var 2..5:rugby;

constraint tenis + futbol + baloncesto + rugby = 14;
constraint tenis < futbol;
constraint rugby = 3 + baloncesto;
constraint tenis != futbol /\ tenis != baloncesto /\ tenis != rugby;
constraint futbol != baloncesto /\ futbol != rugby;
constraint baloncesto != rugby;

solve satisfy;

%#P4 Modelo:
% Edades puestas de 1 a 3 para que no saque demasiadas soluciones
var 1..3:eherminia;
var 1..3:ebertoldo;
var 1..3:ecasemira;

var bool: DA;
var bool: DB;
var bool: DC;

constraint DA = (ecasemira > eherminia);
constraint DB = (eherminia > ebertoldo);
constraint DC = (ecasemira > ebertoldo);
constraint DA = true /\ DB = true;

solve satisfy;

%#P5 Modelo:
var -20.0..20.0:a1;
var -20.0..20.0:b1;
var -20.0..20.0:a2;
var -20.0..20.0:b2;
var -20.0..20.0:a3;
var -20.0..20.0:b3;
var 0..7: y;

constraint a1*4 + b1 = 3 /\ a2*4 + b2 = 3;
constraint a1*2 + b1 = 1 /\ a3*2 + b3 = 1;
constraint a2*6 + b2 = y /\ a3*6 + b3 = y;

solve satisfy;

%#P6 Modelo:

