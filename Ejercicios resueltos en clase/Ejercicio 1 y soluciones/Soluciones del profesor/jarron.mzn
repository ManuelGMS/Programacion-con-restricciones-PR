var bool:aniceto;
var bool:bertoldo;
var bool:herminia;

constraint bertoldo==herminia;
constraint aniceto -> bertoldo;
constraint not(bertoldo /\ herminia);

solve satisfy;