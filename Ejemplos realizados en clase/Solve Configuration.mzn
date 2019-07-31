var 1..10:x;
var 1..10:y;

constraint x < 2*y;
% 2y - x sea lo mayor posible, y dentro de eso,
% y lo menor posible 

%solve :: int_search([y,x], smallest, indomain_min, complete) satisfy;

var int: o1;
constraint o1 = -(2*y-x);
var bool: sePuede;

constraint sePuede = not(y < 5);

solve :: int_search([sePuede,o1,x,y], input_order, indomain_min, complete) satisfy;
