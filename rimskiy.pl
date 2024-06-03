
rimskiy:-	writeln('Hola , mi nombre es  Rimskiy tu  chatbot favorito!!!,
	me especializo en terraria,
	hasme preguntas solo en minisculas o si no me podria morir'),
	readln(Input),
	rimskiy(Input),!.
rimskiy(Input):- Input == ['Adios'],
	writeln('Adios. espero poder verte ayudado.'), !.
rimskiy(Input):- Input == ['Adios', '.'],
	writeln('Adios. espero poder verte ayudado.'), !.
rimskiy(Input):- Input == ['adios', '.'],
	writeln('Adios. espero poder verte ayudado.'), !.
rimskiy(Input):- Input == ['adios'],
	writeln('Adios. espero poder verte ayudado.'), !.
rimskiy(Input) :-
	template(Stim, Resp, IndStim),
	match(Stim, Input),
	% si he llegado aquí es que he
	% hallado el template correcto:
	replace0(IndStim, Input, 0, Resp, R),
	writeln(R),
	readln(Input1),
	rimskiy(Input1), !.

template([hola, mi, nombre, es, s(_), '.'], ['Hola', 0, 'Como', estas, tu, '?'], [4]).
template([buendia, mi, nombre, es, s(_), '.'], ['buen dia', 'Como', estas, tu, 0, '?'], [4]).
template([hola, ',', mi, nombre, es, s(_), '.'], ['Hola', 0, 'Como', estas, tu, '?'], [5]).
template([buendia, ',', mi, nombre, es, s(_), '.'], ['Buendia', 'Como', estas, tu, 0, '?'], [5]).
template([hola,que,tal,_], ['Hola, que tal como estas.'], []). %1
template([hola,_], ['Hola :)'], []). %2
template([yo,soy,s(_),quien,eres,'?','.'], ['Hola', 0,'yo', 'soy', 'Rimskiy'], [2]).%3
template([s(_), es, el, mejor, juego, que, existe, '.'], ['Bueno', ' me alegra que te guste', 0, 'pero no coincido con tus gustos'], [0]). %4
template(['me','gusta','comer',s(_),'pero','prefiero','comer',s(_),'.'], ['Yo prefiero comer',0,'pero tambien me gusta comer',1,'.'], [3,7]). %5
template(['en','terraria','el',s(_),'es','el','mejor','npc','.'], ['Mmmm...','el',0,'podria ser el mejor NPC','.'], [3]). %6
template(['la','clase',s(_),'es','la','mejor','clase','de','terraria','.'], ['La clase',0,'es buena, pero la clase a distancia concidero que podria ser la mejor','.'], [2]). %7
template(['la',s(_),'es','la','mejor','arma cuerpo a cuerpo','de terraria','.'], ['Si!!! la',0,'es la mejor arma cuerpo a cuerpo :D','.'], [1]). %8
template([eres,un,robot,'.'], ['No soy un robot, que irrespetuoso >:v'], []). %9
template([terraria,es,mejor,que,minecraft,'.'], ['Bueno creo que deberias probar Minecraft'], []). %10
template([quien, es, bartola ,'?'], ['Bartola es una mujer muy hermosa, unica en su belleza, sus ojos son como la Luna porque con solo mirarlos me trasnportan a ella.'], []). %11
template(['en terraria si sumo:',s(_),'+',s(_),'obtengo la venus magnum','?'], ['Si juntas',0,'con',1,'no se lo que obtendrias pero seguramante obtendrias algo epico'], [1,3]). %12
template(['que es ser inmortal','?',_], ['Bueno alguien inmortal es quien no experimenta un ciclo natural de la vida, como yo :D'], []). %13
template(['puedo preguntarte algo','?',_], ['Si claro lo que gustes preguntarme :)'], []). %14
template(['eres un buen chatbot',_], [':o, gracias por conciderarme un buen chatbot :) :D'], []). %15
template(['chatgpt es mejor que tu','?',_], ['No, chatgpt es una copia barata de mi'], []). %16
template(['generame un codigo en java',_], ['Oh mira creo que me hablan del mas alla...'], []). %17
template(['cuanto es (x+8/2)(x^2/2)',_], ['Oye oye mas tranquilo, es mejor que hagas tu solito tu tarea :)'], []). %18
template(['tu nombre es',s(_),'.'], ['No mi nombre es Rimskiy no me llamo',0], []). %19
template(['mi nombre es',s(_),'.'], ['Oh mucho gusto',0], []). %20
template([buendia, _], ['Buendia', 'Como', estas, '?'], []).
template([yo, s(_), yo, soy, s(_),'.'], [por, que, 0, eres, 1, '?'], [1, 4]).

%---------------------------------------------------------------------------------------------------------------------
% Sistema Experto Diagnostico
% Hepatitis 
% Herpes zoster 
% Ebola 

%%declaraciones de enfermedades
enfermedad(hepatitis).
enfermedad(herpeszoster).
enfermedad(ebola).

%determinar un síntoma a que enfermedad(es) pertecene 

sintomade(nauseas, hepatitis). %las nauseas son síntoma de hepatitis
sintomade(diarrea, hepatitis). %la diarrea es síntoma de hepatitis
sintomade(ictericia, hepatitis). %la ictericia es síntoma de hepatitis
sintomade(fiebre, hepatitis).
sintomade(dolorestomacal, hepatitis).
sintomade(orinaoscura, hepatitis).

sintomade(fiebre, herpeszoster).
sintomade(escalofrio, herpeszoster).
sintomade(dolorcabeza, herpeszoster).
sintomade(dolorarticular, herpeszoster).
sintomade(malestargeneral, herpeszoster).
sintomade(hormigueo, herpeszoster).
sintomade(ardor, herpeszoster).
sintomade(ampollas, herpeszoster).
sintomade(fiebre, herpeszoster).

sintomade(fiebre, ebola).
sintomade(escalofrio, ebola).
sintomade(dolorcabeza, ebola).
sintomade(dolorgarganta, ebola).
sintomade(dolormuscular, ebola).
sintomade(debilidad, ebola).
sintomade(fatiga, ebola).
sintomade(erupcion, ebola).
sintomade(dolorabdominal, ebola).
sintomade(diarrea, ebola).
sintomade(vomito, ebola).
sintomade(sangrado, ebola).

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

%%función que devuelve la cantidad de síntomas totales de la enfermedad seleccionada
cantSint(E , C) :- findall(X , sintomade(X, E) , L) ,length(L , R), C is R.

diagnostico([X|Xs] , E , K) :- buscar([X|Xs] , E ,P) , cantSint(E , T) , K is P * 100 / T.

%declaraciones de los hechos para determinar medicina de una enfermedad
%que medicamento debo tomar dependiendo la enfermedad
medicinade(pastillas, hepatitis). %pastillas es medicamento de hepatitis%declaración de reglas
medicinade(entecavir, hepatitis).
medicinade(tenofivir, hepatitis).
medicinade(lamivudina, hepatitis).
medicinade(adefovir, hepatitis).
medicinade(telbivudina, hepatitis).
medicinade(interferon, hepatitis).

medicinade(aciclovir, herpeszoster).
medicinade(famciclovir, herpeszoster).
medicinade(valacilovir, herpeszoster).

medicinade(rvsvzebov, ebola).

recetade(M, S):-sintomade(S, Z),medicinade(M, Z).

especialistade(endocrinologia, hepatitis).
especialistade(hepatologia, hepatitis).
especialistade(gastroenterologia, hepatitis).

especialistade(dermatologo, herpeszoster).

especialistade(epidemiologo, ebola).

%-----------
%Reglas
atiende_especialista(E, S):- sintomade(S,Z),especialistade(E, Z).

% esta regla es para hacer consultas ejemplo
% Si tengo enfermedad gripe por ejemplo que especialista y que medicina
% me receta
mereceta(Es, M, E):-medicinade(M, E),sintomade(S, E), atiendeespecialista(Es,S).

%---------------------------------------------------------------------------------------------------------------------
% Fin Sistema Experto Diagnostico

% pregunta algo que le gusta a rimskiy
template([te, gustan, las, s(_), _], [flagLike], [3]).
template([te, gustan, los, s(_), _], [flagLike], [3]).

 % pregunta algo que hace Rimskiy %
template([tu, haces, s(_), _], [flagDo], [2]).
% pregunta algo que es Rimskiy %
template([que, eres, tu, s(_)], [flagIs], [2]).
template([eres, s(_), '?'], [flagIs], [2]).

% Jefes del modo normal en Terraria %
template([el, jefe,s(_),es,del,modo,normal,'?'], [flagJefenormal], [2]).

% Jefes del modo dificil en Terraria %
template([el, jefe,s(_),es,del,modo,dificil,'?'], [flagJefedificil], [2]).

% que puedo crear con el yunque %
template([con, el, yunque, puedo, crear, s(_),'?'], [flagYunque], [5]).

% de acuerdo a mis sintomas que enfermedad tengo %
template([tengo, los, sintomas, [_], tengo, s(_),'?'], [flagTieneEnfermedad], [3,5]).

% templates para que me diga los sintomas de las enfermedades %
template([cuales, son, los, sintomas, de, s(_), s(_), s(_),_], [flagSintomasHepatitisTres], [5,6,7]).
template([como, saber, si, padesco, s(_),'?'], [flagSintomasHepatitis], [4]).
template([que, sintomas, debo, tener, para, que, me, diagnostiquen, s(_)], [flagSintomasHepatitisDos], [8]).
template([que, sintomas, se, padece, de, s(_)], [flagSintomasHepatitis], [5]).
template([que, sintomas, existen, de, el, s(_)], [flagSintomasHepatitis], [5]).
template([dime, los, sintomas, de, s(_)], [flagSintomasHepatitis], [4]).
template([cuales, son, los, sintomas, de, el, s(_)], [flagSintomasHepatitis], [6]).
template([cuales, son, los, sintomas, de, s(_),'.'], [flagSintomasHepatitis], [5]).
template([sintomas, de, s(_)], [flagSintomasHepatitis], [2]).
template([s(_),'.'], [flagSintomasHepatitis], [0]).
template([s(_), sintomas], [flagSintomasHepatitis], [0]).
template([sintomas, s()], [flagSintomasHepatitis], [1]).
template([s(_), es, sintoma, de, s(_),'?'], [flagSintomasDeHepatitis], [0,4]).
template([podria, tener, s(_)], [flagSintomasHepatitisDos], [2]).

% templates para que me diga los medicamentos de las enfermedades %
template([s(_), es, un, medicamento, para, el, s(_),'?'], [flagMedicamentosDeHepatitis], [0,6]).
template([dime, los, medicamentos, que, debo, tomar, si, tengo, s(_)], [flagMedicamentosHepatitis], [8]).
template([dime, medicamentos, para, s(_)], [flagMedicamentosHepatitis], [3]).
template([cuales, son, los, medicamentos, para, la, s(_)], [flagMedicamentosHepatitis], [6]).
template([cuales, son, los, medicamentos, para, el, s(_)], [flagMedicamentosHepatitis], [6]).
template([medicamentos, para, el, s(_)], [flagMedicamentosHepatitis], [3]).
template([medicamentos, para, s(_)], [flagMedicamentosHepatitis], [2]).
template([que, medicamentos, debo, tomar, para, el, s(_),'?'], [flagMedicamentosHepatitis], [6]).
template([que, medicamentos, hay, que, tomar, para, el, s(_),'?'], [flagMedicamentosHepatitis], [7]).
template([que, medicamentos, puedo, tomar, para, s(_),'?'], [flagMedicamentosHepatitis], [5]).
template([que, medicamentos, sirve, para, s(_)], [flagMedicamentosHepatitis], [4]).
template([medicamentos, s(_)], [flagMedicamentosHepatitis], [1]).
template([s(_), medicamentos], [flagMedicamentosHepatitis], [0]).
template([top, medicamentos, s(_)], [flagMedicamentosHepatitis], [2]).
template([s(_),'.'], [flagMedicamentosHepatitis], [0]).

% templates para que me diga los especialistas de las enfermedades %
template([quien, atiende, el, s(_)], [flagEspecialista], [3]).
template([que, especialista, atiende, el, s(_)], [flagEspecialista], [4]).
template([que, medico, revisa, el, s(_)], [flagEspecialista], [4]).
template([quien, me, atiende, mi, s(_)], [flagEspecialista], [4]).
template([que, especialista, ve, el, s(_)], [flagEspecialista], [4]).
template([quien, atiende, el, s(_)], [flagEspecialista], [3]).
template([nombra, los, especialistas, para, atender, el, s(_)], [flagEspecialista], [6]).
template([que, medicos, se, especilizan, en, s(_),'?'], [flagEspecialistaDos], [5]).
template([que, especialistas, estan, capacitados, para, atender, el, s(_),'?'], [flagEspecialistaDos], [7]).
template([quienes, son, los, profesionales, que, atienden, el, s(_),'?'], [flagEspecialistaDos], [7]).
template([si, voy, con, el, s(_), me, atiende, el, s(_),'?'], [flagEspecialistaTres], [4,8]).
template([si, acudo, con, el, s(_), me, atiende, el, s(_),'?'], [flagEspecialistaTres], [4,8]).
template([s(_),'.'], [flagEspecialistaCuatro], [0]).

template([como, estas, tu, '?'], [yo, estoy, bien, ',', gracias, por, preguntar, '.'], []).
template([yo, pienso, que, _], [bueno, esa, es, tu, opinion], []).
template([porque, _], [esa, no, es, una, buena, razon, '.'], []).
				  
template(_, ['Ups no te entendi :p', '.'], []). 
% Lo que le gusta a eliza : flagLike
elizaLikes(X, R):- likes(X), R = ['Si, me gusta', X].
elizaLikes(X, R):- \+likes(X), R = ['No, no me gusta', X].
likes(terraria).
likes(comida).
likes(hamburguesa).
likes(tacos).
likes(computadoras).
like(juegos).



% lo que hace eliza: flagDo
elizaDoes(X, R):- does(X), R = ['Si, a mi me gusta', X, 'y me gusta mucho'].
elizaDoes(X, R):- \+does(X), R = ['No, a mi no me gusta', X ,'prefiero hacer otra cosa'].
does(programar).
does(cocinar).
does(trabajar).
does(dibujar).
does(leer).

% lo que es eliza: flagIs
elizaIs(X, R):- is0(X), R = ['Si, yo soy', X].
elizaIs(X, R):- \+is0(X), R = ['No, yo no soy', X].
is0(chatbot).
is0(inteligente).
is0(astuto).
is0(bueno).
is0(asombroso).
is0(poderoso).

% respuesta de jefe normal: flagJefenormal
jefeNormal(X, R):- is0(X), R = ['Si, el jefe', X, 'es del modo normal'].
jefeNormal(X, R):- \+is0(X), R = ['No, el jefe', X, 'no es del modo normal'].
is0(reyslime).
is0(ojodecthulhu).
is0(devoramundos).
is0(cierviclope).
is0(esqueletron).
is0(avejareina).
is0(murocarnoso).

% respuesta de jefe ndificil: flagJefedificil
jefeDificil(X, R):- is0(X), R = ['Si, el jefe', X, 'es del modo dificil'].
jefeDificil(X, R):- \+is0(X), R = ['No, el jefe', X, 'no es del modo dificil'].
is0(reinaslime).
is0(eldestructor).
is0(gemelos).
is0(esqueletronmayor).
is0(plantera).
is0(emperatriz).
is0(golem).
is0(duque).
is0(sectalunar).
is0(moonlord).

% respuesta a que puedo hacer yunque: flagYunque
hacerYunque(X, R):- is0(X), R = ['Si, con el yunque puedes crear', X].
hacerYunque(X, R):- \+is0(X), R = ['No, requeires de otra estacin de trabajo para crear', X].
is0(armaduras).
is0(herramientas).
is0(armas).

tieneEnfermedad(X,Y,P,R):- buscar([X|Xs] , Y , P), R = ['Si :( de acuerdo a los sintomas presentados tienes',Y].
tieneEnfermedad(X,Y,P,R):- \+buscar([X|Xs] , Y , P), R = ['No :) de acuerdo a los sintomas no tienes',Y].

sintomasHepatitis(E, R):- findall(X, sintomade(X, E), S), R = ['Los sintomas de',E,'son ',S].
sintomasHepatitisDos(E, R):- findall(X, sintomade(X, E), S), R = ['Si tienes los sintomas', S, 'podrias tener', E]. 

sintomasDeHepatitisDos(S,E,R):- sintomade(S, E), R = ['Si el sintoma ',S,' es sintoma de ',E].
sintomasDeHepatitisDos(S,E,R):- \+sintomade(S, E), R = ['No el sintoma ',S,' no es sintoma de ',E].

sintomasDeHepatitisTres(E1, E2, E3, R1, R2, R3):- 
findall(X, sintomade(X, E1), S1), R1 = ['Los sintomas de',E1,'son ',S1],
findall(Y, sintomade(Y, E2), S2), R2 = ['Los sintomas de',E2,'son ',S2],
findall(Z, sintomade(Z, E3), S3), R3 = ['Los sintomas de',E3,'son ',S3].

medicamentoHepatitis(E, R):- findall(X, medicinade(X, E), S), R = ['Los medicamentos para ',E,' son ', S].
medicamentosDeHepatitis(M,E,R):- medicinade(M, E), R = ['Si, el medicamento ',M,' se usa para el ', E].
medicamentosDeHepatitis(M,E,R):- \+medicinade(M, E), R = ['No, el medicamento ',M,' no se usa para el ', E]. 

especialistaEnfermedad(E,R):- findall(X, especialistade(X, E), S), R = ['El/los espcilistas de ',E,' son ', S].
especialistaEnfermedadDos(E,R):- findall(X, especialistade(X, E), S), R = ['Los especialistas ',S,' atienden el ', E].
especialistaEnfermedadTres(E,F,R):- especialistade(E, F), R = ['Si, si vas con el ',E, ' te atiende el ',F].
especialistaEnfermedadTres(E,F,R):- \+especialistade(E, F), findall(X, especialistade(X, F), S), R = ['No, si vas con el ',E, ' no te atendera el ',F, ' seras referido a un especilista ',S].
especialistaEnfermedadCuatro(E, R):- findall(X, especialistade(X, E), S), R = ['Quien te atiende el ',E,' son los especialistas ', S].

match([],[]).
match([], _):- true.

match([S|Stim],[I|Input]) :-
	atom(S), % si I es un s(X) devuelve falso
	S == I,
	match(Stim, Input),!.

match([S|Stim],[_|Input]) :-
% I es un s(X), lo ignoro y continúo con el resto de la lista
	\+atom(S),
	match(Stim, Input),!.

replace0([], _, _, Resp, R):- append(Resp, [], R),!.

% Eliza likes:
replace0([I|_], Input, _, Resp, R):-
	nth0(I, Input, Atom),
	nth0(0, Resp, X),
	X == flagLike,
	elizaLikes(Atom, R).

% Eliza does:
replace0([I|_], Input, _, Resp, R):-
	nth0(I, Input, Atom),
	nth0(0, Resp, X),
	X == flagDo,
	elizaDoes(Atom, R).

% Eliza is:
replace0([I|_], Input, _, Resp, R):-
	nth0(I, Input, Atom),
	nth0(0, Resp, X),
	X == flagIs,
	elizaIs(Atom, R).

% Rimskiy jefe normal:
replace0([I|_], Input, _, Resp, R):-
	nth0(I, Input, Atom),
	nth0(0, Resp, X),
	X == flagJefenormal,
	jefeNormal(Atom, R).

% Rimskiy jefe dificil:
replace0([I|_], Input, _, Resp, R):-
	nth0(I, Input, Atom),
	nth0(0, Resp, X),
	X == flagJefedificil,
	jefeDificil(Atom, R).

% Rimskiy hacer yunque:
replace0([I|_], Input, _, Resp, R):-
	nth0(I, Input, Atom),
	nth0(0, Resp, X),
	X == flagYunque,
	hacerYunque(Atom, R).

% Rimskiy te dice si tienes la enfermedad:
replace0([X, J], Input, _, Resp, R) :- 
    nth0(X, Input, Atom),
	nth0(J, Input, Atom1),
	nth0(0, Resp, X),
    X == flagTieneEnfermedad,
	cantSint(Atom1,CantEnfermedad),
    tieneEnfermedad(Atom, Atom1,CantEnfermedad, R).

% Rimskiy te dice los sintomas de hepatitis:
replace0([I|_], Input, _, Resp, R) :- 
    nth0(I, Input, Atom),
	nth0(0, Resp, X),
    X == flagSintomasHepatitis,
    sintomasHepatitis(Atom,R).

% Rimskiy te dice los sintomas de hepatitis:
replace0([I|_], Input, _, Resp, R) :- 
    nth0(I, Input, Atom),
	nth0(0, Resp, X),
    X == flagSintomasHepatitisDos,
    sintomasHepatitisDos(Atom,R).

% Rimskiy te dice los sintomas de hepatitis:
replace0([I,J], Input, _, Resp, R) :- 
    nth0(I, Input, Atom),
	nth0(J, Input, Atom1),
	nth0(0, Resp, X),
    X == flagSintomasDeHepatitis,
    sintomasDeHepatitisDos(Atom, Atom1, R).

% Rimskiy te dice los sintomas de hepatitis:
replace0([I,J,K], Input, _, Resp, R1,R2,R3) :- 
    nth0(I, Input, Atom),
	nth0(J, Input, Atom1),
	nth0(K, Input, Atom2),
	nth0(0, Resp, X),
    X == flagSintomasHepatitisTres,
    sintomasDeHepatitisTres(Atom, Atom1, Atom2, R1, R2, R3).

% Rimskiy te dice los medicamnetos de hepatitis:
replace0([I|_], Input, _, Resp, R) :- 
    nth0(I, Input, Atom),
	nth0(0, Resp, X),
    X == flagMedicamentosHepatitis,
    medicamentoHepatitis(Atom,R).

% Rimskiy te dice los sintomas de hepatitis:
replace0([I,J], Input, _, Resp, R) :- 
    nth0(I, Input, Atom),
	nth0(J, Input, Atom1),
	nth0(0, Resp, X),
    X == flagMedicamentosDeHepatitis,
    medicamentosDeHepatitis(Atom, Atom1, R).

% Rimskiy te dice los especialistas de las enfermedades:
replace0([I|_], Input, _, Resp, R) :- 
    nth0(I, Input, Atom),
	nth0(0, Resp, X),
    X == flagEspecialista,
    especialistaEnfermedad(Atom, R).

% Rimskiy te dice los especialistas de las enfermedades:
replace0([I|_], Input, _, Resp, R) :- 
    nth0(I, Input, Atom),
	nth0(0, Resp, X),
    X == flagEspecialistaDos,
    especialistaEnfermedadDos(Atom, R).

% Rimskiy te dice los especialistas de las enfermedades:
replace0([I,J], Input, _, Resp, R) :- 
    nth0(I, Input, Atom),
	nth0(J, Input, Atom1),
	nth0(0, Resp, X),
    X == flagEspecialistaTres,
    especialistaEnfermedadTres(Atom, Atom1, R).

% Rimskiy te dice los especialistas de las enfermedades:
replace0([I|_], Input, _, Resp, R) :- 
    nth0(I, Input, Atom),
	nth0(0, Resp, X),
    X == flagEspecialistaCuatro,
    especialistaEnfermedadCuatro(Atom, R).

replace0([I|Index], Input, N, Resp, R):-
	length(Index, M), M =:= 0,
	nth0(I, Input, Atom),
	select(N, Resp, Atom, R1), append(R1, [], R),!.

replace0([I|Index], Input, N, Resp, R):-
	nth0(I, Input, Atom),
	length(Index, M), M > 0,
	select(N, Resp, Atom, R1),
	N1 is N + 1,
	replace0(Index, Input, N1, R1, R),!.


