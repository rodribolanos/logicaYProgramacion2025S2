symbol(a).
symbol(b).
symbol(c).
symbol(d).
symbol(1).
symbol(2).

tieneEstrella(star(_)).
tieneEstrella(concat(A, B)) :- tieneEstrella(A) ; 
                               tieneEstrella(B).
tieneEstrella(or(A, B)) :-  tieneEstrella(A) ; 
                            tieneEstrella(B).

longitudMaxima(empty, 0).
longitudMaxima(concat(A, B), N) :- longitudMaxima(A, NA), longitudMaxima(B, NB), N is NA + NB.
longitudMaxima(or(A, B), N) :- longitudMaxima(A, NA), longitudMaxima(B, NB), N is max(NA, NB).
longitudMaxima(A, 1) :- symbol(A).

cadena([]).
cadena([S|SS]) :- symbol(S), cadena(SS).

/* match_inst(+Cadena,+RegEx): 
dada una cadena y una expresion regular, es verdadero cuando Cadena es aceptada por RegEx. */

match_inst([], empty).
match_inst([S], S) :- symbol(S). %Pattern match con misma variable, si las variables son distintas no entra por aca.
match_inst([], star(_)).
match_inst(Cadena, or(R1, R2)) :- match_inst(Cadena, R1); match_inst(Cadena, R2).  
/*Es una cadena valida si es la cadena de la primer rama o la cadena de la segunda */

match_inst(Cadena, concat(R1, R2)) :- append(Combinacion1, Combinacion2, Cadena), match_inst(Combinacion1, R1),  match_inst(Combinacion2, R2). 
/* El arbol va a difurcarse por todas las combinaciones de desconstruccion de la lista Cadena. 
Alguna combinacion debe coincidir con ser un match */

match_inst(Cadena, star(R)) :- append(P1, P2, Cadena), match_inst(P1, R), match_inst(P2, star(R)). 
/*Partir la cadena de tal manera que en alguna combinacion el resultante de la partida sea exactamente B y 
las repeticiones de B en la lista.
Caso pensado en clase: [a,b,a,b,a,b,a,b]. star(concat(a,b)). Debe partir la lista en [a,b] [a,b,a,b,a,b] */


/* Definir el predicado match(?Cadena,+RegEx) que extienda match_inst para
que adem´as pueda generar todas las cadenas aceptadas por la expresi´on regular
RegEx. */

match([], empty).
match(X, concat(R1, R2)) :- match(X1, R1), match(X2, R2), append(X1, X2, X).
match(X, or(R1, R2)) :- match(X, R1); match(X, R2).
match([], star(_)).
match(X, star(R)) :- match(X1, R), X1 /= [], match(X2, star(R)), append(X1, X2, X).
match([S], S) :- symbol(S). 
/* Si el regex de param es un symbol, devolvemos el symbol dentro de una lista */


diferencia(X, R1, R2) :- match(X, R1), not(match_inst(X, R2)).