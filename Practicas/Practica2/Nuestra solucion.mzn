% nombre1 MANUEL GUERRERO MOÑÚS
% nombre2 ALEJANDRO CILLEROS GARRUDO
% nombre3

%%%%%%%%%%%%%% No tocar esta parte
int:estudiantes=10;
int:preguntas=5;
int:puntuaciones=10;
array[1..estudiantes,1..preguntas] of 0..puntuaciones:examenes=
      [| 
      1,6, 0,3,0 |
      1,2, 4,4,5 | 
      6,3, 7,4,10 | 
      5,3, 8,4,6 |  
      4,3, 7,4,10, |  
      6,3, 7,4,10, |  
      3,3, 7,4,10, |  
      4,3, 7,2,6, |  
      0,3, 8,4,3, |  
      2,3, 7,8,10   
           |] ;
%%%%%%%%%%%%%%%

% 1 	Definir un array 'pesos' de enteros, con un entero por cada pregunta (utilizar las constantes definidas al principio del modelo, en este caso será un array de 1 a ‘preguntas’
array [1..preguntas] of var int: pesos; 

% 2 restricción que asegura que 1<=pesos[i]<=10 para todo i
constraint forall([1 <= pesos[i] /\ pesos[i] <= 10 | i in 1..preguntas]);

% 3 La suma de los pesos debe ser 10
constraint sum(pesos) == 10;

% 4 Nota del estudiante 3. (es un valor entre 0 y 100). 
% Calcularla teniendo en cuenta los pesos de cada pregunta.
var int:nota3;
constraint nota3 <= 100 /\ nota3 >= 0;
constraint nota3 = sum([pesos[i] * examenes[3,i] | i in 1..preguntas]);


% 5 Objetivo: que el estudiante 3 saque la máxima nota posible

  %solve maximize nota3;

% 6 declararse el array 'notas' como un array de variables enteras, 
% una nota por estudiante. Ojo: recordar comentar la respuesta 5
array [1..estudiantes] of var int: notas; 

% 7 Asegurarse de que notas[e] tiene la puntuación del estudiante e, para todo e
constraint forall([notas[e] = sum([pesos[i] * examenes[e,i] | i in 1..preguntas]) | e in 1..estudiantes]);

% 8 Minimizar el número de aprobados (estudiantes con nota >=50)
% Hay que suspender a todo el mundo

 % solve minimize sum([notas[e]>=50 | e in 1..estudiantes]);

% 9 Comentar el anterior y escribir un objetivo para que la nota mínima sea lo más alta posible

  % solve maximize min(notas);

% 10. Comentar el anterior.
% Obligar a que el número de estudiantes que saca la nota más alta sea lo mayor posible.

solve maximize sum([notas[e] == max(notas) | e in 1..estudiantes]);