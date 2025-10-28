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
cadena([H|T]) :- symbol(H), cadena(T).

/* match_inst(+Cadena,+RegEx): 
dada una cadena y una expresion regular, es verdadero cuando Cadena es aceptada por RegEx. */

match_inst([], empty).
match_inst([A], A) :- symbol(A). %Pattern match con misma variable, si las variables son distintas no entra por aca.
match_inst([], star(_)).
match_inst(Cadena, or(A, B)) :- match_inst(Cadena, A); match_inst(Cadena, B).  
/*Es una cadena valida si es la cadena de la primer rama o la cadena de la segunda */

match_inst(Cadena, concat(A, B)) :- append(Combinacion1, Combinacion2, Cadena), match_inst(Combinacion1, A),  match_inst(Combinacion2, B). 
/* El arbol va a difurcarse por todas las combinaciones de desconstruccion de la lista Cadena. 
Alguna combinacion debe coincidir con ser un match */

match_inst(Cadena, star(B)) :- append(P1, P2, Cadena), match_inst(P1, B), match_inst(P2, star(B)). 
/*Partir la cadena de tal manera que en alguna combinacion el resultante de la partida sea exactamente B y 
las repeticiones de B en la lista.
Caso pensado en clase: [a,b,a,b,a,b,a,b]. star(concat(a,b)). Debe partir la lista en [a,b] [a,b,a,b,a,b] */


/* Definir el predicado match(?Cadena,+RegEx) que extienda match_inst para
que adem´as pueda generar todas las cadenas aceptadas por la expresi´on regular
RegEx. */

match([], empty).
match(X, concat(A, B)) :- match(X1, A), match(X2, B), append(X1, X2, X).
match(X, or(A, B)) :- match(X, A); match(X, B).
match([], star(_)).
match(X, star(A)) :- match(X1, A), X1 /= [], match(X2, star(A)), append(X1, X2, X).
match([B], B) :- symbol(B). 
/* Si el regex de param es un symbol, devolvemos el symbol dentro de una lista */