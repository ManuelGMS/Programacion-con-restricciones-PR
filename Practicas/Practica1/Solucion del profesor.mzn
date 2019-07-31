Programación con restricciones - práctica 1

#Nombre1: 
#Nombre2:
#Nombre3:

#Observaciones:

#P1 Modelo:

var bool :A; % false duende, true caballero
var bool :B;

constraint A <-> not(B);

solve satisfy;

#P2 Modelo:

var 1..7:hoy;
var 1..7:manana;

%%%%%%%%%%%%%%% restricciones
% para que tras el domingo vaya el lunes
constraint manana = hoy mod 7 +1;
% si hoy toca verdad, mañana también debe pasar
constraint (hoy==1 \/ hoy==3 \/ hoy==6) -> (manana==1 \/ manana==3 \/ manana==6);
% si hoy miente, mañana también
constraint (hoy==2 \/ hoy==4 \/ hoy==5 \/ hoy==7) -> (manana==2 \/ manana==4 \/ manana==5 \/ manana==7);

solve satisfy;

#P3 Modelo:

var 1..3:bertoldo;
var 1..3:herminia;
var 1..3:casimira;
var bool:C1;
var bool:C2;
var bool:C3;

constraint C1 == ( casimira > herminia);
constraint C2 == ( herminia > bertoldo);
constraint C3 == ( casimira > bertoldo);
constraint C1 /\ C2;


solve satisfy;
output [show(C3)];



#P4 Modelo:

include "alldifferent.mzn";
var 2..5:futbol;
var 2..5:tenis;
var 2..5:baloncesto;
var 2..5:rugby;

constraint alldifferent([futbol, tenis, baloncesto, rugby]);
constraint futbol + tenis + baloncesto + rugby == 14;
constraint tenis < futbol;
constraint rugby == baloncesto + 3;

solve satisfy;

#P6 Modelo:

int:N=1000;
var 1..N:a;
constraint sum([i | i in 1..a-1 where a mod i==0]) == a;
solve satisfy;     

#P5 Modelo:

int:p1x=4;
int:p1y=3;
int:p2x=2;
int:p2y=1;
int:p3x=6;

var 0..7:p3y;

var -20.0..20.0:a1;
var -20.0..20.0:b1;
var -20.0..20.0:a2;
var -20.0..20.0:b2;
var -20.0..20.0:a3;
var -20.0..20.0:b3;

constraint 3 = a1*4 + b1;
constraint 3 = a2*4 + b2;

constraint 1 = a1*2 + b1;
constraint 1 = a3*2 + b3;

constraint  p3y= a3*6 + b3;
constraint  p3y= a2*6 + b2;

# P6 restricciones adicionales
var float:AB;
var float:CA;
var float:BC;
int:y=2;
float:x=5.5;

constraint AB= (y-p1y)*(p2x-p1x) - (x-p1x)*(p2y-p1y); % producto vectorial
constraint CA=(y-p3y)*(p1x-p3x) - (x-p3x)*(p1y-p3y);
constraint BC=(y-p2y)*(p3x-p2x) - (x-p2x)*(p3y-p2y);

constraint AB*BC>0 /\ BC*CA>0; % todos del mismo signo

