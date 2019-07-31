var bool:A;
var bool:B;
var bool:C;

var bool:DA;
constraint DA== (A+B+C==1);
constraint if A then not(DA) else DA endif;

var bool:DB;
constraint DB== (A+B+C==2);
constraint if B then not(DB) else DB endif;

var bool:DC;
constraint DC== (A+B+C==3);
constraint if C then not(DC) else DC endif;

solve satisfy;

