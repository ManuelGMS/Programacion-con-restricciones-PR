var bool: bertoldo;
var bool: herminia;
var bool: aniceto;

constraint (bertoldo = true /\ herminia = true) \/ (bertoldo = false /\ herminia = false); % bertoldo == herminia 

constraint if aniceto = true then bertoldo = true endif;

constraint (bertoldo = true /\ herminia = false) \/ (bertoldo = false /\ herminia = true) \/ (bertoldo = false /\ herminia = false); % not bertoldo /\ herminia

solve satisfy;