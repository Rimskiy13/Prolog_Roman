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
hermano(alejandrochavez).
hermano(leo).
hermana(andrea).

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

tiade(belen, ramon).

abuelo(X, Y):- padrede(X,Z) , padrede(Z,Y).
hermano(X,Y):- padrede(Z, X), madrede(M, X).
tio(X, Y):- padrede(Z, X) , madrede(M, X), hermanode(H, X).