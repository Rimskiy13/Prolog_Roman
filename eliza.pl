
eliza:-	writeln('Hola , mi nombre es  Rimskiy tu  chatbot favorito!!!,
	me especializo en terraria,
	hasme preguntas solo en minisculas o si no me podria morir'),
	readln(Input),
	eliza(Input),!.
eliza(Input):- Input == ['Adios'],
	writeln('Adios. espero poder verte ayudado.'), !.
eliza(Input):- Input == ['Adios', '.'],
	writeln('Adios. espero poder verte ayudado.'), !.
eliza(Input):- Input == ['adios', '.'],
	writeln('Adios. espero poder verte ayudado.'), !.
eliza(Input):- Input == ['adios'],
	writeln('Adios. espero poder verte ayudado.'), !.
eliza(Input) :-
	template(Stim, Resp, IndStim),
	match(Stim, Input),
	% si he llegado aquí es que he
	% hallado el template correcto:
	replace0(IndStim, Input, 0, Resp, R),
	writeln(R),
	readln(Input1),
	eliza(Input1), !.

template([hola, mi, nombre, es, s(_), '.'], ['Hola', 0, 'Como', estas, tu, '?'], [4]).
template([buendia, mi, nombre, es, s(_), '.'], ['buen dia', 'Como', estas, tu, 0, '?'], [4]).
template([hola, ',', mi, nombre, es, s(_), '.'], ['Hola', 0, 'Como', estas, tu, '?'], [5]).
template([buendia, ',', mi, nombre, es, s(_), '.'], ['Buendia', 'Como', estas, tu, 0, '?'], [5]).
template([hola,que,tal,_], ['Hola, que tal como estas.'], []). %1
template([hola,_], ['Hola :)'], []). %2
template([yo,soy,s(_),quien,eres,'?','.'], ['Hola', 0,'yo', 'soy', 'Eliza'], [2]).%3
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

% Hepatitis, herpes zoster, ebola, medicamentos, docotores que lo pueden atender, especialista, sintomas

% pregunta algo que le gusta a eliza
template([te, gustan, las, s(_), _], [flagLike], [3]).
template([te, gustan, los, s(_), _], [flagLike], [3]).

 % pregunta algo que hace eliza
template([tu, eres, s(_), _], [flagDo], [2]).
% pregunta algo que es eliza
template([que, eres, tu, s(_)], [flagIs], [2]).
template([eres, s(_), '?'], [flagIs], [2]).

% Jefes del modo normal en Terraria
template([el, jefe,s(_),es,del,modo,normal,'?'], [flagJefenormal], [2]).

% Jefes del modo dificil en Terraria
template([el, jefe,s(_),es,del,modo,dificil,'?'], [flagJefedificil], [2]).

% que puedo crear con el yunque
template([con, el, yunque, puedo, crear, s(_),'?'], [flagYunque], [5]).

template([como, estas, tu, '?'], [yo, estoy, bien, ',', gracias, por, preguntar, '.'], []).
template([yo, pienso, que, _], [bueno, esa, es, tu, opinion], []).
template([porque, _], [esa, no, es, una, buena, razon, '.'], []).

				  
template(_, ['Ups',no,te,entendi,':p', '.'], []). 
% Lo que le gusta a eliza : flagLike
elizaLikes(X, R):- likes(X), R = ['Yeah', i, like, X].
elizaLikes(X, R):- \+likes(X), R = ['Nope', i, do, not, like, X].
likes(apples).
likes(ponies).
likes(zombies).
likes(manzanas).
likes(computadoras).
like(carros).



% lo que hace eliza: flagDo
elizaDoes(X, R):- does(X), R = ['Yes', i, X, and, i, love, it].
elizaDoes(X, R):- \+does(X), R = ['No', i, do, not, X ,'.', it, is, too, hard, for, me].
does(study).
does(cook).
does(work).

% lo que es eliza: flagIs
elizaIs(X, R):- is0(X), R = ['Yes', yo, soy, X].
elizaIs(X, R):- \+is0(X), R = ['No', i, am, not, X].
is0(dumb).
is0(weird).
is0(nice).
is0(fine).
is0(happy).
is0(redundant).

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

%% Sistema Experto Diagnostico %%
%% Hepatitis %%
%% Herpes zoster %%
%% Ebola %% 

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
