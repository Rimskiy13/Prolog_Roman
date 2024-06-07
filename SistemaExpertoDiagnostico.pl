%%declaraciones de enfermedades
enfermedad(gripe).
enfermedad(rubeola).
enfermedad(malaria).
enfermedad(hepatitis).
enfermedad(tuberculosis).
enfermedad(anemia).

%determinar un síntoma a que enfermedad(es) pertecene 

sintomade(tos, gripe). %la tos es síntoma de gripe
sintomade(cansancio, gripe). %el cansancio es síntoma de gripe
sintomade(fiebre, gripe). %la fiebre es síntoma de gripe
sintomade(dolorcabeza, gripe). %dolor de cabeza es síntoma de gripe

sintomade(nauseas, hepatitis). %las nauseas son síntoma de hepatitis
sintomade(diarrea, hepatitis). %la diarrea es síntoma de hepatitis
sintomade(ictericia, hepatitis). %la ictericia es síntoma de hepatitis

sintomade(cansancio, anemia). %cansancio es síntomade anemia
sintomade(apatia, anemia). %apatía es síntoma deanemia
sintomade(nausea, anemia). %las nauseas son síntomas de anemia

sintomade(tos, tuberculosis). %la tos es síntoma de la tuberculosis
sintomade(cansancio, tuberculosis). %el cansancio es síntoma de tuberculosis
sintomade(fiebre, tuberculosis). %la fiebre es síntoma de la tuberculosis
sintomade(escalofrios, tuberculosis). %los escalofríos es síntoma de tuberculosis

sintomade(escalofrios, malaria). %los escalofríos son síntomas de la malaria
sintomade(fiebre , malaria). %la fiebre es síntomade malaria
sintomade(diarrea , malaria). %la diarrea es síntoma de malaria
sintomade(ictericia, malaria). %la ictericia es síntoma de malaria

sintomade(fiebre, rubeola). %la fiebre es síntomade rubéola
sintomade(jaqueca, rubeola). %la jaqueca es síntoma de rubéola
sintomade(secrecion, rubeola). %la secreción es síntoma de rubeola

% Reglas para determinar que probabilidad una persona puede tener una
% enfermedad X dado n síntomas
% %La función buscar busca las enfermedades que contengan como mínimo los síntomas
%suministrados
% % primer parámetro (lista de enfermedades)
% % segundo parámetro (Enfermedad)

% %tercer parámetro cantidad de ocurrencias
% %(a decir verdad tendría que ser la misma cantidad que el arreglo entregado)

buscar([], E , 0).
buscar(X , E , 1) :- sintomade(X, E).
buscar([X|Xs] , E , P) :- enfermedad(E) , buscar(X, E , S1) , buscar(Xs , E ,S2) , P is S1 + S2.
cantSint(E , C) :- findall(X , sintomade(X, E) , L) ,length(L , R), C is R.