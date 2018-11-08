%%%%%%%%%% Interfaz de Usuario %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
startTransLog:-
    nl, write('Indique el idioma de entrada, ejemplo "ingles."'), nl,
    write('Write the language, example "english."'), nl,
    read(Language),
    nl, write('Escribe una frase entre parentesis cuadrados y separando cada palabra con comas, de esta forma: [esto, es, una, frase]'), nl,
    write('Write a phrase between square brackets and separating each word with commas, like this: [this, is, a, phrase]'), nl,
    write('ó parentesis cuadrados vacíos si desea detener el traductor '), nl,
    read(Text),
    transLog(Language, Text).

%%%%%%%%%% Reglas y funciones del traductor %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Finalizar la traduccion
transLog(_,[]):-
    write('Finalizando traduccion'), nl,
    write('Ending translation').

% Llamado a traduccion
transLog(Language, Text):- traducir(Traduction, Language, _, _, Text,[]),write(Traduction),nl,nl,nl, startTransLog.



% Traduccion de Español a Ingles
traducir(Traduccion, español, Num, Gen, S0, S):-
    sintagma_nominal(Num, Gen, S0, S, Traduccion).
traducir(Traduccion, español, Num, _, S0, S):-
    sintagma_verbal(Num, S0, S, Traduccion).
traducir(Traduccion, español, Num, Gen, S0, S):-
    sintagma_nominal(Num, Gen, S0, S1, TraducSN),
    sintagma_verbal(Num, S1, S, TraducSV),
    append(TraducSN, TraducSV, Traduccion).
traducir(Traduccion, spanish, Num, Gen, S0, S):-
    sintagma_nominal(Num, Gen, S0, S, Traduccion).
traducir(Traduccion, spanish, Num, _, S0, S):-
    sintagma_verbal(Num, S0, S, Traduccion).
traducir(Traduccion, spanish, Num, Gen, S0, S):-
    sintagma_nominal(Num, Gen, S0, S1, TraducSN),
    sintagma_verbal(Num, S1, S, TraducSV),
    append(TraducSN, TraducSV, Traduccion).

% Traduccion de Ingles a Español
traducir(Translation, ingles, Num, Gen, S0, S):-
    nominal_sintax(Num, Gen, S0, S, Translation).
traducir(Translation, ingles, Num, _, S0, S):-
    verbal_sintax(Num, S0, S, Translation).
traducir(Translation, ingles, Num, Gen, S0, S):-
    nominal_sintax(Num, Gen, S0, S1, TraducSN),
    verbal_sintax(Num, S1, S, TraducSV),
    append(TraducSN, TraducSV, Translation).
traducir(Translation, english, Num, Gen, S0, S):-
    nominal_sintax(Num, Gen, S0, S, Translation).
traducir(Translation, ingles, Num, _, S0, S):-
    verbal_sintax(Num, S0, S, Translation).
traducir(Translation, english, Num, Gen, S0, S):-
    nominal_sintax(Num, Gen, S0, S1, TraducSN),
    verbal_sintax(Num, S1, S, TraducSV),
    append(TraducSN, TraducSV, Translation).



% Reglas nominales (de Esp. a Ing.)
sintagma_nominal(Num, Genero, S0, S, [TraducSN]):-
    sujeto(Num, Genero, S0, S, TraducSN).
sintagma_nominal(Num, Genero, S0, S, [TraducSN]):-
    adjetivo(Num, Genero, S0, S, TraducSN).
sintagma_nominal(Num, Genero, S0, S, [TraducSN]):-
    determinante(Num, Genero, S0, S, TraducSN).
sintagma_nominal(_, _, S0, S, [TraducSN]):-
    expresion(S0, S, TraducSN).
sintagma_nominal(Num, _, S0, S, TraducSN):-
    signo(S0, S1, _),
    preguntas(Num, S1, S2, TraducP),
    signo(S2, S, Signo),
    append(TraducP, [Signo], TraducSN).
sintagma_nominal(Num, Genero, S0, S, TraducSN):-
    sujeto(Num, Genero, S0, S1, TraducS),
    adjetivo(Num, Genero, S1, S, TraducA),
    append([TraducA], [TraducS], TraducSN).
sintagma_nominal(Num, Genero, S0, S, TraducSN):-
    determinante(Num, Genero, S0, S1, TraducD),
    sujeto(Num, Genero, S1, S, TraducS),
    append([TraducD], [TraducS], TraducSN).
sintagma_nominal(Num, Genero, S0, S, TraducSN):-
    determinante(Num, Genero, S0, S1, TraducD),
    sujeto(Num, Genero, S1, S2, TraducS),
    adjetivo(Num, Genero, S2, S, TraducA),
    append([TraducD], [TraducA], TraducAUX),
    append(TraducAUX, [TraducS], TraducSN).

preguntas(Num, S0, S, TraducPS):-
    pregunta(Num, S0, S1, TraducP),
    sintagma_verbal(Num, S1, S, TraducSV),
    append([TraducP], TraducSV, TraducPS).
preguntas(Num, S0, S, TraducPS):-
    sintagma_verbal(Num, S0, S1, TraducSV),
    sintagma_nominal(Num, _, S1, S, TraducSN),
    append(TraducSV, TraducSN, TraducPS).


% Reglas verbales (de Esp. a Ing.)
sintagma_verbal(Num, S0 , S, [TraducSV]):-
    verbo(Num, S0, S, TraducSV).
sintagma_verbal(Num, S0, S, TraducSV):-
    verbo(Num, S0, S1, TraducV),
    compl_dir(S1, S, TraducC),
    append([TraducV], TraducC, TraducSV).
sintagma_verbal(Num, S0, S, TraducSV):-
    compl_dir(S0, S1, TraducC),
    verbo(Num, S1, S, TraducV),
    append([TraducV], TraducC, TraducSV).

compl_dir(S0, S, TraducC):-
    sintagma_nominal(_, _, S0, S, TraducC).
compl_dir(S0, S, TraducC):-
    preposicion(S0, S1),
    sintagma_nominal(_, _, S1, S, TraducC).
compl_dir(S0, S, TraducC):-
    determinante(num, gen, S0, S1, TraducD),
    sintagma_nominal(num, gen, S1, S, TraducSN),%%%%%%%%%%%
    append([TraducD], TraducSN, TraducC).


% Nominal rules (Ing. to Esp.)
nominal_sintax(Num, Gen, S0, S, [TraducSN]):-
    subject(Num, Gen, S0, S, TraducSN).
nominal_sintax(Num, Gen, S0, S, [TraducSN]):-
    adjective(Num, Gen, S0, S, TraducSN).
nominal_sintax(Num, Gen, S0, S, [TraducSN]):-
    determinant(Num, Gen, S0, S, TraducSN).
nominal_sintax(_, _, S0, S, [TraducSN]):-
    expression(S0, S, TraducSN).
nominal_sintax(Num, _, S0, S, [TraducSN]):-
    question(Num, S0, S, TraducSN).
nominal_sintax(Num, Gen, S0, S, TraducSN):-
    adjective(Num, Gen, S0, S1, TraducA),
    subject(Num, Gen, S1, S, TraducS),
    append([TraducS], [TraducA], TraducSN).
nominal_sintax(Num, Gen, S0, S, TraducSN):-
    determinant(Num, Gen, S0, S1, TraducD),
    subject(Num, Gen, S1, S, TraducS),
    append([TraducD], [TraducS], TraducSN).
nominal_sintax(Num, Gen, S0, S, TraducSN):-
    determinant(Num, Gen, S0, S1, TraducD),
    adjective(Num, Gen, S1, S2, TraducA),
    subject(Num, Gen, S2, S, TraducS),
    append([TraducD], [TraducS], TraducAUX),
    append(TraducAUX, [TraducA], TraducSN).

% Verbal rules (Ing. to Esp.)
verbal_sintax(Num, S0, S, [TraducSV]):-
    verb(Num ,S0 , S, TraducSV).
verbal_sintax(Num, S0 , S, TraducSV):-
    verb(Num, S0, S1, TraducV),
    dir_compl(S1, S, TraducC),
    append([TraducV], TraducC, TraducSV).
verbal_sintax(Num, S0, S, TraducSV):-
    dir_compl( S0, S1, TraducC),
    verb(Num, S1, S, TraducV),
    append([TraducV], TraducC, TraducSV).

dir_compl(S0, S, TraducC):-
    nominal_sintax(_, _, S0, S, TraducC).
dir_compl(S0, S, TraducC):-
    determinant(num, gen, S0, S1, TraducD),%%%%%%%%%%%%%%%%%%%%%
    nominal_sintax(num, gen, S1, S, TraducSN),
    append([TraducD], TraducSN, TraducC).



% Diccionario
%%%%%%%%%%%%%%%%%%%%%%% ESPAÑOL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sujeto(singular, masc, [año|S], S, year).
sujeto(singular, masc, [hombre|S], S, man).
sujeto(singular, fem, [mujer|S], S, woman).
sujeto(plural, fem, [mujeres|S], S, women).
sujeto(plural, masc, [hombres|S], S, men).
sujeto(singular, fem, [manzana|S], S, apple).
sujeto(plural, fem, [manzanas|S], S, apples).
sujeto(singular, masc, [juan|S], S, juan).
sujeto(singular, fem, [maria|S], S, maria).
sujeto(singular, masc, [carro|S], S, car).
sujeto(plural, masc, [carros|S], S, cars).
sujeto(singular, masc, [prolog|S], S, prolog).
sujeto(singular, masc, [uno|S], S, one).
sujeto(plural, masc, [unos|S], S, one).
sujeto(singular, masc, [primero|S], S, first).
sujeto(plural, masc, [primeros|S], S, first).
sujeto(singular, masc, [logica|S], S, logic).
sujeto(singular, masc, [lenguaje|S], S, language).
sujeto(plural, masc, [lenguajes|S], S, languages).
sujeto(singular, masc, [artificial|S], S, artificial).
sujeto(singular, masc, [natural|S], S, natural).
sujeto(singular, masc, [marco|S], S, marco).
sujeto(singular, masc, [patrones|S], S, pattern).
sujeto(singular, masc, [arbol|S], S, tree).
sujeto(plural, masc, [arboles|S], S, trees).
sujeto(singular, masc, [sistema|S], S, system).
sujeto(plural, masc, [sistemas|S], S, systems).
sujeto(singular, masc, [teorema|S], S, theorem).
sujeto(singular, masc, [dia|S],S, day).
sujeto(plural, masc, [examenes|S], S, exams).
sujeto(singular, fem, [casa|S], S, house).
sujeto(singular, masc, [helado|S], S, icecream).
sujeto(singular, fem, [helado|S], S, icecream).
sujeto(plural, fem, [helados|S], S, icecreams).
sujeto(plural, masc, [helados|S], S, icecreams).
sujeto(plural, masc, [años|S], S, old).
sujeto(singular, masc, [hambre|S], S, hungry).
sujeto(singular, masc, [batido|S], S, milkshake).
sujeto(singular, fem, [mamá|S], S, mom).
sujeto(singular, masc, [libro|S], S, book).

signo([¿|S], S, _).
signo([?|S], S, ?).

pregunta(singular, [qué|S], S, what).
pregunta(singular, [cuál|S], S, what).
pregunta(singular, [cuánto|S], S, how_much).
pregunta(plural, [cuántos|S], S, how_much).
pregunta(singular, [dónde|S], S, where).
pregunta(singular, [cómo|S], S, how).
pregunta(singular, [quién|S], S, who).
pregunta(singular, [cuál|S], S, which).
pregunta(plural, [quiénes|S], S, who).
pregunta(plural, [cuáles|S], S, which).

expresion([hola|S], S, hello).
expresion([adios|S], S, bye).

%%%%%%%%%%%%%VERBOS EN PRESENTE%%%%%%
%%%%%%%%%%%%%%%amar%%%%%%%%%%%%%%%%%%%%
verbo(singular, [ama|S] , S, loves).
verbo(plural, [amamos|S] , S, love).
verbo(plural, [aman|S], S, love).

%%%%%%%%%%%%%%%%%%hacer%%%%%%%%%%%%%
verbo(singular, [hace|S], S, does).
verbo(plural, [hacen|S], S, do).
verbo(plura, [hacemos|S], S, do).

%%%%%%%%%%%%%%%comer%%%%%%%%%%%%%%%%%%%%
verbo(singular, [come|S], S, eats).
verbo(plural, [comen|S], S, eat).
verbo(plutral, [comemos|S], S, eat).

%%%%%%%%%%%%%%%venir%%%%%%%%%%%%%%%%%%%%
verbo(singular, [viene|S], S, comes).
verbo(plural, [vienen|S], S, come).
verbo(plural, [venimos|S], S, come).

%%%%%%%%%%%%%%%vivir%%%%%%%%%%%%%%%%%%%%
verbo(singular, [vive|S], S, lives).
verbo(plural, [viven|S], S, live).
verbo(plural, [vivimos|S], S, live).

%%%%%%%%%%%%%%%tener%%%%%%%%%%%%%%%%%%%%
verbo(singular, [tiene|S], S, has).
verbo(plural, [tienen|S], S, have).
verbo(plural, [tenemos|S], S, have).

%%%%%%%%%%%%%%%apreciar%%%%%%%%%%%%%%%%%%%%
verbo(singular, [aprecia|S], S, appreciates).
verbo(plural, [apreciamos|S], S, appreciate).
verbo(plural, [aprecian|S], S, appreciate).

%%%%%%%%%%%%%%%beber%%%%%%%%%%%%%%%%%%%%
verbo(singular, [bebe|S], S, drinks).
verbo(plural, [bebemos|S], S, drink).
verbo(plural, [beben|S], S, drink).

%%%%%%%%%%%%%%%poder%%%%%%%%%%%%%%%%%%%%
verbo(singular, [puede|S], S, can).
verbo(plural, [pueden|S], S, can).
verbo(plural, [podemos|S], S, can).

%%%%%%%%%%%%%%%leer%%%%%%%%%%%%%%%%%%%%
verbo(singular, [lee|S], S, reads).
verbo(plural, [leemos|S], S, read).
verbo(plural, [leen|S], S, read).

%%%%%%%%%%%%%%%mover%%%%%%%%%%%%%%%%%%%%
verbo(singular, [mueve|S], S, moves).
verbo(plural, [mueven|S], S, move).
verbo(plural, [movemos|S], S, move).

%%%%%%%%%%%%%%estar%%%%%%%%%%%%%%%%%%%%
verbo(singular, [esta|S], S, is).

%%%%%%%%%%%%%%%Ser/Estar%%%%%%%%%%%%%%%%%
verbo(singular, [es|S], S, is).

%%%%%%%%%%%%%%%%%%querer%%%%%%%%%%%%%%%%%
verbo(singular, [quiere|S], S, wants).

%%%%%%%%%%%%%VERBOS PASADO%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%amar%%%%%%%%%%%%%%%%%%%%
verbo(singular, [amaba|S], S, loved).
verbo(plural, [amábamos|S] , S, loved).
verbo(plural, [amaron|S], S, loved).

%%%%%%%%%%%%%%%%%%hacer%%%%%%%%%%%%%
verbo(singular, [hacía|S],S, did).
verbo(plural, [hacían|S], S, did).
verbo(plura, [hacíamos|S], S, did).

%%%%%%%%%%%%%%%comer%%%%%%%%%%%%%%%%%%%%
verbo(singular, [comía|S], S, ate).
verbo(plural, [comían|S], S, ate).
verbo(plutral, [comíamos|S], S, ete).

%%%%%%%%%%%%%%%venir%%%%%%%%%%%%%%%%%%%%
verbo(singular, [venía|S] , S, came).
verbo(plural, [vienen|S] , S, came).
verbo(plural, [venimos|S] , S, came).

%%%%%%%%%%%%%%%vivir%%%%%%%%%%%%%%%%%%%%
verbo(singular, [vivía|S], S, lived).
verbo(plural, [vivían|S], S, lived).
verbo(plural, [vivíamos|S], S, lived).

%%%%%%%%%%%%%%%tener%%%%%%%%%%%%%%%%%%%%
verbo(singular, [tenía|S], S, had).
verbo(plural, [tenían|S], S, had).
verbo(plural, [teníamos|S], S, had).

%%%%%%%%%%%%%%%apreciar%%%%%%%%%%%%%%%%%%%%
verbo(singular, [apreciaba|S], S, appreciated).
verbo(plural, [apreciaron|S], S, appreciated).
verbo(plural, [apreciábamos|S], S, appreciated).

%%%%%%%%%%%%%%%beber%%%%%%%%%%%%%%%%%%%%
verbo(singular, [bebía|S], S, drank).
verbo(plural, [bebían|S], S, drank).
verbo(plural, [bebiamos|S], S, drank).

%%%%%%%%%%%%%%%poder%%%%%%%%%%%%%%%%%%%%
verbo(singular, [podía|S], S ,could ).
verbo(plural, [podían|S], S, could).
verbo(plural, [podíamos|S], S, coul).

%%%%%%%%%%%%%%%leer%%%%%%%%%%%%%%%%%%%%
verbo(singular, [leaía|S], S, read).
verbo(plural, [leíamos|S], S, read).
verbo(plural, [leían|S], S, read).

%%%%%%%%%%%%%%%mover%%%%%%%%%%%%%%%%%%%%
verbo(singular, [movío|S], S, moved).
verbo(plural, [movieron|S], S, moved).
verbo(plural, [movíamos|S], S, moved).


determinante(singular, masc, [el|S], S, the).
determinante(singular, masc, [él|S], S, he).
determinante(singular, fem, [ella|S], S, she).
determinante(singular, fem, [la|S], S, the).
determinante(plural, masc, [los|S], S, the).
determinante(plural, fem, [las|S], S, the).
determinante(singular, masc, [un|S], S, a).
determinante(singular, fem, [un|S], S, a).
determinante(singular, fem, [una|S], S, a).
determinante(plural, fem, [unas|S], S, a).
determinante(plural, masc, [unos|S], S, a).
determinante(singular, masc, [un|S], S, an).
determinante(singular, fem, [una|S], S, an).
determinante(plural, fem, [unas|S], S, an).
determinante(plural, masc, [unos|S], S, an).
determinante(plural, masc, [ellos|S], S, they).
determinante(plural, fem, [ellas|S], S, they).
determinante(plural, masc, [aquellos|S], S, those).
determinante(plural, fem, [aquellas|S], S, those).
determinante(plural, masc, [esos|S], S, those).
determinante(plural, fem, [esas|S], S, those).
determinante(plural, masc, [nosotros|S], S, we).
determinante(plural, fem, [nosotras|S], S, we).
determinante(singular, masc, [con|S], S, with).
determinante(singular, fem, [con|S], S, with).
determinante(plural, masc, [con|S], S, with).
determinante(plural, masc, [con|S], S, with).
determinante(singular, masc, [tú|S], S, your).
determinante(singular, fem, [tú|S], S, your).


adjetivo(singular, masc, [inteligencia|S], S, intelligence).
adjetivo(singular, masc, [inteligente|S], S, intelligent).
adjetivo(plural, masc, [inteligentes|S], S, intelligent).
adjetivo(singular, masc, [rojo|S], S, red).
adjetivo(plural, masc, [rojos|S], S, red).
adjetivo(singular, fem, [roja|S], S, red).
adjetivo(plural, ferm, [rojas|S], S, red).
adjetivo(singular, masc, [rapido|S], S, fast).
adjetivo(plural, masc, [dificiles|S], S, difficult).
adjetivo(singular, masc, [dificil|S], S, difficult).
adjetivo(singular, masc, [azul|S], S, blue).
adjetivo(singular, fem, [azul|S], S, blue).
adjetivo(singular, masc, [favorito|S], S, favourite).

adjetivo(singular, masc, [si|S], S, yes).
adjetivo(singular, masc, [no|S], S, no).


preposicion([a|S], S).
preposicion([¿|S], S).

%%%%%%%%%%%%%%%%%%%%%%%% INGLES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subject(singular, masc, [year|S], S, año).
subject(singular, masc, [man|S], S, hombre).
subject(singular, fem, [woman|S], S, mujer).
subject(plural, fem, [women|S], S, mujeres).
subject(plural, masc, [men|S], S, hombre).
subject(singular, fem, [apple|S], S, manzana).
subject(singular, masc, [apple|S], S, manzana).
subject(plural, fem, [apples|S], S, manzanas).
subject(singular, masc, [juan|S], S, juan).
subject(singular, fem, [maria|S], S, maria).
subject(singular, masc, [car|S], S, carro).
subject(plural, masc, [cars|S], S, carros).
subject(singular, masc, [prolog|S], S, prolog).
subject(singular, masc, [one|S], S, uno).
subject(plural, masc, [one|S], S, unos).
subject(singular, masc, [first|S], S, primero).
subject(plural, masc, [first|S], S, primeros).
subject(singular, masc, [logic|S], S, logica).
subject(singular, masc, [language|S], S, lenguaje).
subject(plural, masc, [languages|S], S, lenguajes).
subject(singular, masc, [artificial|S], S, artificial).
subject(singular, masc, [natural|S], S, natural).
subject(singular, masc, [marco|S], S, marco).
subject(singular, masc, [pattern|S], S, patrones).
subject(singular, masc, [tree|S], S, arbol).
subject(plural, masc, [trees|S], S, arboles).
subject(singular, masc, [system|S], S, sistema).
subject(plural, masc, [systems|S], S, sistemas).
subject(singular, masc, [theorem|S], S, teorema).
subject(singular, masc, [day|S], S, dia).
subject(singular, fem, [home|S], S, casa).
subject(singular, masc, [icecream|S], S, helado).
subject(singular, masc, [tomato|S], S, tomate).
subject(singular, masc, [milkshake|S], S, batido).

question(singular, [what|S], S, qué).
question(singular, [how_much|S], S, cuánto).
question(plural, [how_much|S], S, cuántos).
question(singular, [where|S], S, dónde).
question(singular, [how|S], S, cómo).
question(singular, [who|S], S, quién).
question(singular, [wich|S], S, cuál).
question(plural, [who|S], S, quiénes).
question(plural, [which|S], S, cuáles).

expression([hello|S], S, hola).
expression([bye|S], S, adios).

%%%%%%%%%%%%%%%PRESENTE%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%amar%%%%%%%%%%%%%%%%%%%%
verb(singular, [loves|S] , S, ama).
verb(plural, [love|S] , S, amamos).
verb(plural, [love|S], S, aman).

%%%%%%%%%%%%%%%comer%%%%%%%%%%%%%%%%%%%%
verb(singular, [eats|S], S, come).
verb(plural, [eat|S], S, comen).
verb(plutral, [eat|S], S, comemos).

%%%%%%%%%%%%%%%venir%%%%%%%%%%%%%%%%%%%%
verb(singular, [comes|S] , S, viene).
verb(plural, [come|S] , S, vienen).
verb(plural, [come|S] , S, venimos).

%%%%%%%%%%%%%%%vivir%%%%%%%%%%%%%%%%%%%%
verb(singular, [lives|S], S, vive).
verb(plural, [live|S], S, viven).
verb(plural, [live|S], S, vivimos).

%%%%%%%%%%%%%%%tener%%%%%%%%%%%%%%%%%%%%
verb(singular, [has|S], S, tiene).
verb(plural, [have|S], S, tienen).
verb(plural, [have|S], S, tenemos).

%%%%%%%%%%%%%%%apreciar%%%%%%%%%%%%%%%%%%%%
verb(singular, [appreciates|S], S, aprecia).
verb(plural, [appreciate|S], S, apreciamos).
verb(plural, [appreciate|S], S, aprecian).

%%%%%%%%%%%%%%%beber%%%%%%%%%%%%%%%%%%%%
verb(singular, [drinks|S], S, bebe).
verb(plural, [drik|S], S, bebemos).
verb(plural, [drik|S], S, beben).

%%%%%%%%%%%%%%%poder%%%%%%%%%%%%%%%%%%%%
verb(singular, [can|S], S, puede).
verb(plural, [can|S], S, pueden).
verb(plural, [can|S], S, podemos).

%%%%%%%%%%%%%%%leer%%%%%%%%%%%%%%%%%%%%
verb(singular, [reads|S], S, lee).
verb(plural, [read|S], S, leemos).
verb(plural, [read|S], S, leen).

%%%%%%%%%%%%%%%mover%%%%%%%%%%%%%%%%%%%%
verb(singular, [moves|S], S, mueve).
verb(plural, [move|S], S, mueven).
verb(plural, [move|S], S, movemos).

%%%%%%%%%%%%%%%Ser/Estar%%%%%%%%%%%%%%%%%
verb(singular, [is|S], S, es).

%%%%%%%%%%%%%%PASADO%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%amar%%%%%%%%%%%%%%%%%%%%
verb(singular, [loved|S], S, amó).
verb(plural, [loved|S], S, amábamos).
verb(plural, [loved|S], S, amamos).

%%%%%%%%%%%%%%%comer%%%%%%%%%%%%%%%%%%%%
verb(singular, [ate|S], S, comía).
verb(plural, [ate|S], S, comían).
verb(plutral, [ate|S], S, comemos).

%%%%%%%%%%%%%%%venir%%%%%%%%%%%%%%%%%%%%
verb(singular, [came|S], S, vino).
verb(plural, [came|S], S, venían).
verb(plural, [came|S], S, venimos).

%%%%%%%%%%%%%%%vivir%%%%%%%%%%%%%%%%%%%%
verb(singular, [lived|S], S, vivía).
verb(plural, [lived|S], S, vivían).
verb(plural, [lived|S], S, vivíamos).

%%%%%%%%%%%%%%%tener%%%%%%%%%%%%%%%%%%%%
verb(singular, [had|S], S, tenía).
verb(plural, [had|S], S, tenían).
verb(plural, [had|S], S, teníamos).

%%%%%%%%%%%%%%%apreciar%%%%%%%%%%%%cs%%%%%%%%
verb(singular, [appreciated|S], S, apreció).
verb(plural, [appreciated|S], S, apreciabamos).
verb(plural, [appreciated|S], S, apreciaron).

%%%%%%%%%%%%%%%beber%%%%%%%%%%%%%%%%%%%%
verb(singular, [drank|S], S, bebío).
verb(plural, [drank|S], S, bebíamos).
verb(plural, [drank|S], S, bebían).

%%%%%%%%%%%%%%%poder%%%%%%%%%%%%%%%%%%%%
verb(singular, [can|S], S, puede).
verb(plural, [can|S], S, pueden).
verb(plural, [can|S], S, podemos).

%%%%%%%%%%%%%%%leer%%%%%%%%%%%%%%%%%%%%
verb(singular, [read|S], S, leía).
verb(plural, [read|S], S, leíamos).
verb(plural, [read|S], S, leían).

%%%%%%%%%%%%%%%mover%%%%%%%%%%%%%%%%%%%%
verb(singular, [moved|S], S, movía).
verb(plural, [moved|S], S, movieron).
verb(plural, [moved|S], S, movíeramos).

%%%%%%%%%%%%%%%querer%%%%%%%%%%%%%%%%%%%
verb(singular, [wants|S], S, quiere).

determinant(singular, masc, [the|S], S, el).
determinant(singular, masc, [he|S], S, él).
determinant(singular, fem, [she|S], S, ella).
determinant(singular, fem, [the|S], S, la).
determinant(plural, masc, [the|S], S, los).
determinant(plural, fem, [the|S], S,las).
determinant(singular, masc, [a|S], S, un).
determinant(singular, fem, [a|S], S, una).
determinant(plural, masc, [a|S], S, unos).
determinant(plural, fem, [a|S], S, unas).
determinant(singular, masc, [an|S], S, un).
determinant(singular, fem, [an|S], S, una).
determinant(plural, masc, [an|S], S, unos).
determinant(plural, fem, [an|S], S, unas).
determinant(plural, masc, [they|S], S, ellos).
determinant(plural, fem, [they|S], S, ellas).
determinant(plural, masc, [those|S], S, aquellos).
determinant(plural, fem, [those|S], S, aquellas).
determinant(plural, masc, [those|S], S, esos).
determinant(plural, fem, [those|S], S, esas).
determinant(singular, masc, [your|S], S, tú).

adjective(singular, masc, [intelligence|S], S, inteligencia).
adjective(singular, masc, [intelligent|S], S, inteligente).
adjective(plural, masc, [intelligent|S], S, inteligentes).
adjective(singular, masc, [red|S], S, rojo).
adjective(plural, masc, [reds|S], S, rojos).
adjective(singular, fem, [red|S], S, roja).
adjective(plural, ferm, [reds|S], S, rojas).
adjective(singular, masc, [fast|S], S, rapido).






















