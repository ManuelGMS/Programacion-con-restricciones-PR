% MANUEL GUERRERO MOÑÚS
% ALEJANDRO CILLEROS GARRUDO

int: MaxH = 8;
int: N = 5;
int: D = 7;
set of int:trabajadores = 1..N;
set of int:dias = 1..D;
set of int:horas = 0..MaxH;

  % EJC 1
array [trabajadores,dias] of var horas: t;

  % EJC 2
array [trabajadores] of var int: horasPersona;

  % EJC 3
constraint forall(i in trabajadores)(horasPersona[i] = sum(j in dias)(t[i,j]));

  % EJC 4
array [dias] of var int: horasDia;

  % EJC 5
constraint forall(j in dias)(horasDia[j] = sum(i in trabajadores)(t[i,j]));

  % EJC 6
constraint forall(i in trabajadores)(exists(j in dias)(t[i,j] = 0));

  % EJC 7
constraint forall(i in trabajadores)(horasPersona[i] >= (MaxH*(D-3)));

  % EJC 8
constraint forall(i in trabajadores)(forall(j in 1..D-1)(t[i,j] < MaxH \/ (t[i,j] = MaxH /\ t[i,j+1] < MaxH)));

  % EJC 9
constraint sum(i in dias)(horasDia[i]) >= 225;

  % EJC 10
var dias: totalDias;

constraint exists(i in dias)(forall(j in 1..i)(horasDia[j] != 0) /\ forall (j in i+1..D)(horasDia[j] == 0) /\ totalDias = i);

solve minimize totalDias;

output([show(t[i,j]) ++ " " ++
           (if (j == D) then "\n" else " " endif) | i in trabajadores, j in dias]);

output(["\nHorasPersona\n"]);     
output([show(horasPersona[i]) ++ " " | i in trabajadores]);

output(["\n" ++ "\nHorasDia\n"]);     
output([show(horasDia[i]) ++ " " | i in dias]);