% num. de items disponibles
int: n = 5;

% rango desde 1 al num. items
set of int: Items = 1..n;

% lo que cuesta cada objeto
array[Items] of int: precio = [1000,1500,3000,800,900];

% lo que pesa cada objeto
array[Items] of int: peso = [6,7,10,8,4];

% peso max. que cabe en la maletin
int: peso_max = 15;

%%%% un conjunto de variables de decision
var set of Items: maletin;

%%%% restricciones

  % constraint sum([bool2int(i in maletin)*peso[i] | i in Items]) <= peso_max;

  constraint sum([peso[i] | i in Items where i in maletin]) <= peso_max;

%%%% resolver maximizando la ganancia
solve maximize sum([bool2int(i in maletin)*precio[i] | i in Items]);

%%%% mostrar el resultado
output [show(maletin)];
