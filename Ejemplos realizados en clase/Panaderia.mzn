% Data file for simple production planning model
int:nproducts = 2; %banana cakes and chocolate cakes
set of int: Products = 1..nproducts;

%profit per unit for each product
array[Products] of int: profit = [400, 450]; %in cents
array[Products] of string:pname = ["banana-cake", "cocoa-cake"];

int: nresources = 5; %flour, banana, sugar, butter, cocoa
set of int: Resources = 1..nresources;

%amount of each resource available
array[Resources] of int: capacity = [4000, 6, 2000, 500, 500];
array[Resources] of string: rname = ["flour","banana","sugar","butter","cocoa"];
array [Products,Resources] of int: consumption= [| 250, 2, 75, 100, 0, | 200, 0, 150, 150, 75 |];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
array [Products] of var int:cuantos;
array [Resources] of var int:gastados;
%%%% cuantos no puede ser negativo
constraint forall(i in Products)(cuantos[i]>=0);

%%%% No hemos gastado demasiado material
constraint forall(i in Resources)
                 (gastados[i] = 
                      sum(j in Products)(
                          cuantos[j] *consumption[j,i]));
% para cada recurso, asegurar que no se ha gastado más de lo que
% se dispone
constraint  forall(i in Resources)(gastados[i] <= capacity[i]);                         

var int:dinero;
constraint dinero = sum(j in Products)(cuantos[j]*profit[j]);

solve maximize dinero;

output(["Ganado: "++show(dinero)++"\n"]);
output([ pname[i]++": "++show(cuantos[i])++"\n" | i in Products] );
output([ rname[i]++": "++show(gastados[i])++" -- " | i in Resources] );