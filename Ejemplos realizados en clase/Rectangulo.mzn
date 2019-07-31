% Mayor coordenada en el rectangulo

% rect. representado por x,y,lx,ly
array[1..4] of float: r1 = [20.0, 20.0,10.0,10.0];
array[1..4] of float: r2 = [25.0, 28.0,5.0,5.0];

var float:x;
var float:y;

predicate enElRectangulo(var float:x0, var float:y0, array[1..4] of float: r) =
      x0>=r[1] /\ x0<=r[1]+r[3] /\ y0>=r[2] /\ y0<=r[2]+r[4];

constraint (enElRectangulo(x,y,r1)) /\ (enElRectangulo(x,y,r2));

solve maximize (x+y);

output["x: ",show(x), ". y:",show(y)];
