% Nombre1: MANUEL GUERRERO MOÑÚS
% Nombre2: ALEJANDRO CILLEROS GARRUDO
% Nombre3:

%%%%%%%%%%%%%%%%%%%%%%%% no tocar %%%%%%%%%%%
% num ciudades
int:n=5;
% longitud max. del recorrido
int:max=n;
int:FLAG=0;
% dinero que cuesta ir de x a y: t[x,y]
array [1..n,1..n] of int:t=
  [|    0, 100,  10,  420,   0 |
       50,   0,   0,   0,   0 |
        0,   0,   0, 200,  75 |
      150, 100,   0,   0,   0 |
        0, 100,   0, 120,   0 |];
        
int:nentregas=4;
% cada fila es de la forma origen,fin,número de paquetes
array [1..nentregas,1..3] of int:entregas = 
   [| 1,2,5 |
      2,4,3 |
      4,3,50 |
      1,5,6 |];

%%%%%%%%%%


%ciudad origen
int:ini=2;
%ciudad destino
int:fin=4;
%%%%%%%%%%%%%%%%%%%%%%%%%%% Solución 
        
%1) declarar el array 'trayecto' de tamaño 1..max y valores de 0..n
array [1..max] of var 0..n: trayecto;

% declarar posfin, la longitud real del trayecto.
var 2..max: posfin;

% 2) La primera ciudad del trayecto sera ini, la última (posición posfin) ser fin
constraint trayecto[1] = ini;
constraint trayecto[posfin] = fin;

% 3) restricción para todo i>posfin trayecto[i]=FLAG y si i<=posfin, trayecto[i]!=FLAG
constraint forall(i in 1..posfin)(trayecto[i] != FLAG);
constraint forall(i in posfin+1..max)(trayecto[i] == FLAG);

% 4) los valores en trayecto desde 1 hasta posfin ambos incluidos, deben ser diferentes
constraint forall(i,j in 1..posfin where i != j)(trayecto[i] != trayecto[j]);
    
% 5)  dos posiciones consecutivas en el tramo 1..posfin deben cumplir 
% que hay camino entre ellas
constraint forall(i in 1..posfin-1)(t[trayecto[i],trayecto[i+1]] > 0);


% 6 array coste
array [1..max-1] of var 0..max(t): coste;
constraint forall(i in posfin..max-1)(coste[i] == FLAG);


% 7 constraint para rellenar el cose
constraint forall(i in 1..posfin-1)(coste[i]==t[trayecto[i],trayecto[i+1]]);


% 8 array coste tiene un elemento menos, declararlo de 1 hasta max-1
% tener en cuenta que ahora tiene de valores válidos desde 1 hasta posfin-1
% declarar la variable costetotal y la restricción que segura que tiene
% la suma de todos los costes. minimizar el resultados 
var int:costeTotal;
constraint costeTotal = sum(coste);

  %solve minimize costeTotal;

% 9 minimizar el número de trayectos

  solve minimize posfin;


% 10
array [1..nentregas,1..max] of var 0..n: caminos;
array [1..nentregas] of var 2..max: posfins;

constraint forall(i in 1..nentregas)(caminos[i,1] = ini);
constraint forall(i in 1..nentregas)(caminos[i,posfins[i]] = fin);

constraint forall(i in 1..nentregas, j in 1..posfins[i])(caminos[i,j] != FLAG);
constraint forall(i in 1..nentregas, j in posfins[i]+1..max)(caminos[i,j] == FLAG);

constraint forall(k in 1..nentregas, i,j in 1..posfins[k] where i != j)(caminos[k,i] != caminos[k,j]);

constraint forall(k in 1..nentregas, i in 1..posfins[k]-1)(t[caminos[k,i],caminos[k,i+1]] > 0);

array [1..nentregas, 1..max-1] of var 0..max(t): costesTotales;
constraint forall(k in nentregas, i in posfins[k]..max-1)(costesTotales[k,i] == FLAG);

constraint forall(k in 1..nentregas, i in 1..posfins[k]-1)(costesTotales[k,i]==t[caminos[k,i],caminos[k,i+1]]);

%var int:costeTotal;
%constraint costeTotal = sum([caminos]);

%solve minimize costeTotal;