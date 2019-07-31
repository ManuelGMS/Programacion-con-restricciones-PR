% Use this editor as a MiniZinc scratch book

%%%%%%% España coloreada

% número máximo de colores 
int: nc = 3;

% color de cada comunidad
var 1..nc: ga;  var 1..nc:can; var 1..nc: pv;  var 1..nc:nav; var 1..nc:ara;   var 1..nc:cat; 
var 1..nc: as; var 1..nc: cyl; var 1..nc: rio; var 1..nc:mad; var 1..nc: clm; var 1..nc:val; 
var 1..nc: mur; var 1..nc:and; var 1..nc: ext;

% restricciones
constraint ga!=as; constraint ga!=cyl; constraint as!=cyl; constraint as!=can;
constraint can!=pv; constraint can!=cyl; constraint pv != nav; constraint pv!= rio;
constraint nav != rio; constraint nav!=ara; constraint ara != cat; constraint ara != val;
constraint  rio!= cyl; constraint pv != cyl; constraint cat != val; 
constraint cyl != mad;
constraint cyl != ext; constraint cyl != clm; constraint ext != clm; constraint ara != clm;
constraint mad != clm; constraint clm != val;  constraint ext != and; constraint clm != mur;
constraint and != mur; constraint mur != val; constraint rio!=ara; 

% resolver
solve satisfy;

% mostrar la solución
output ["galicia=", show(ga), "\t cantabria=", show(can), "\t país vasco=", show(pv), "\n",
"aragón=", show(ara), "\t cataluña=", show(cat), "\t asturias=", show(as), "\t navarra=", show(nav),  "\n",
"castilla la mancha=", show(clm), "\t castilla y león=", show(cyl), "\t rioja=", show(rio), "\n",
"madrid=", show(mad), "\t valencia=", show(val), "\t murcia=", show(mur), "\n",
"andalucía=", show(and), "\t extremadura=", show(ext), "\n"];
