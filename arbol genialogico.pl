abuelopaterno(fermin).
abuelapaterna(guadalupe).
tiapaterna(carmen).
tiapaterna(lupe).
tiapaterna(angelica).
tiapaterna(belen).
tiopaterno(margarito).
tiopaterno(chema).
primopaterno(ramon).
primopaterno(gaby).
primopaterno(yolanda).
padre(alejandro).

abuelomaterno(sacramento).
abuelamaterna(rosa).
tiomaterno(ruben).
tiamaterna(carmenguillen).
madre(rosamaria).

hermanode(alejandro, carmen). 
hermanode(alejandro, lupe).
hermanode(alejandro, belen).
hermanode(alejandro, margarito).
hermanode(alejandro, chema).

hermanode(lupe, alejandro).
hermanode(lupe, carmen).
hermanode(lupe, belen).
hermanode(lupe, margarito).
hermanode(lupe, chema).

hermanode(belen, alejandro).
hermanode(belen, carmen).
hermanode(belen, lupe).
hermanode(belen, margarito).
hermanode(belen, chema).

hermanode(margarito, alejandro).
hermanode(margarito, carmen).
hermanode(margarito, lupe).
hermanode(margarito, belen).
hermanode(margarito, chema).

hermanode(chema, alejandro).
hermanode(chema, carmen).
hermanode(chema, lupe).
hermanode(chema, belen).
hermanode(chema, margarito).

hermanode(alejandrochavez, leo).
hermanode(alejandrochavez, andrea).
hermanode(alejandrochavez, roman).

hermanode(leo, alejandrochavez).
hermanode(leo, andrea).
hermanode(leo, roman).

hermanode(andrea, alejandrochavez).
hermanode(andrea, leo).
hermanode(andrea, roman).

hermanode(roman, alejandrochavez).
hermanode(roman, leo).
hermanode(roman, andrea).

padrede(fermin, carmen).
padrede(fermin, lupe).
padrede(fermin, margarito).
padrede(fermin, chema).
padrede(fermin, alejandro).

madrede(guadalupe, carmen).
madrede(guadalupe, lupe).
madrede(guadalupe, margarito).
madrede(guadalupe, chema).

madrede(carmen, ramon).
madrede(belen, gaby).
madrede(rosamaria, alejandrochavez).
madrede(rosamaria, leo).
madrede(rosamaria, andrea).
madrede(rosamaria, roman).
padrede(chema, yolanda).
padrede(alejandro, alejandrochavez).
padrede(alejandro, leo).
padrede(alejandro, andrea).
padrede(alejandro, roman).

abuelode(fermin, ramon).
abuelode(fermin, gaby).
abuelode(fermin, yolanda).
abuelode(fermin, alejandrochavez).
abuelode(fermin, leo).
abuelode(fermin, andrea).
abuelode(fermin, roman).

abuelade(guadalupe, ramon).
abuelade(guadalupe, gaby).
abuelade(guadalupe, yolanda).
abuelade(guadalupe, alejandrochavez).
abuelade(guadalupe, leo).
abuelade(guadalupe, andrea).
abuelade(guadalupe, roman).

tiode(alejandro, ramon).
tiode(lupe, ramon).
tiode(belen, ramon).
tiode(margarito, ramon).
tiode(chema, ramon).

tiode(alejandro, gaby).
tiode(lupe, gaby).
tiode(belen, gaby).
tiode(margarito, gaby).
tiode(chema, gaby).

tiode(lupe, alejandrochavez).
tiode(belen, alejandrochavez).
tiode(margarito, alejandrochavez).
tiode(chema, alejandrochavez).

tiode(lupe, leo).
tiode(belen, leo).
tiode(margarito, leo).
tiode(chema, leo).

tiode(lupe, andrea).
tiode(belen, andrea).
tiode(margarito, andrea).
tiode(chema, andrea).

tiode(lupe, roman).
tiode(belen, roman).
tiode(margarito, roman).
tiode(chema, roman).

% Reglas
abuelo(X, Y) :- padrede(X, Z), padrede(Z, Y).
abuela(X, Y) :- madrede(X, Z), madrede(Z, Y).

hermano(X, Y) :- padrede(Z, X), madrede(M, X), padrede(Z, Y), madrede(M, Y), X \= Y.
hermana(X, Y) :- hermano(X, Y), mujer(X).

tio(X, Y) :- hermanode(X, Z), padrede(Z, Y).
tia(X, Y) :- hermanode(X, Z), padrede(Z, Y), mujer(X).

primo(X, Y) :- tiode(Z, X), padrede(Z, Y).
prima(X, Y) :- primo(X, Y), mujer(X).

abuelosde(X, L):- findall(I, abuelode(I,X), P), findall(J, abuelade(J,X), M), append(P, M, L).
padresde(X, L):- findall(I, padrede(I,X), P), findall(J, madrede(J,X), M), append(P, M, L). 
hermanosde(X, L):- findall(I, hermanode(I,X), L).
tiosde(X, L):- findall(I, tiode(I, X), L).
primosde(X, L):- findall(I, primo(I, X), L).


% Hechos adicionales para definir género
mujer(carmen).
mujer(lupe).
mujer(angelica).
mujer(belen).
mujer(gaby).
mujer(yolanda).
mujer(rosamaria).
mujer(guadalupe).
mujer(rosa).
mujer(carmenguillen).
mujer(andrea).

hombre(fermin).
hombre(alejandro).
hombre(margarito).
hombre(chema).
hombre(ramon).
hombre(alejandrochavez).
hombre(leo).
hombre(roman).
hombre(sacramento).
hombre(ruben).