var bool: A;
var bool: B;
var bool: C;

var bool: DA; % Lo que dice A
var bool: DB;
var bool: DC;

constraint DA = (A+B+C==1);
constraint A <-> not(DA);

constraint DB = (A+B+C==2);
constraint B <-> not(DB);

constraint DC = (A+B+C==3);
constraint C <-> not(DC);

solve satisfy;