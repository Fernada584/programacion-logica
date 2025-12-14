% ==========================================================
% ELIZA MEDICO - SISTEMA EXPERTO + CHATBOT AVANZADO
% VERSION FINAL CORREGIDA
% ==========================================================

% ==========================================================
% BASE DE CONOCIMIENTO (ACTIVIDAD 1)
% ==========================================================

tiene_sintoma(neumonia, tos).
tiene_sintoma(neumonia, fiebre).
tiene_sintoma(neumonia, dificultad_respirar).
tiene_sintoma(neumonia, malestar).
tiene_sintoma(neumonia, dolor_cabeza).

tiene_sintoma(meningitis, fiebre).
tiene_sintoma(meningitis, dolor_cabeza).
tiene_sintoma(meningitis, rigidez_cuello).
tiene_sintoma(meningitis, somnolencia).
tiene_sintoma(meningitis, vomito).
tiene_sintoma(meningitis, convulsiones).

tiene_sintoma(hepatitis_b, ictericia).
tiene_sintoma(hepatitis_b, fatiga).
tiene_sintoma(hepatitis_b, dolor_abdomen).
tiene_sintoma(hepatitis_b, orina_oscura).

% Tratamientos (Actividad 1)
tratamiento(neumonia, 'Antibioticos, hidratacion y reposo.').
tratamiento(meningitis, 'Antibioticos intravenosos y hospitalizacion.').
tratamiento(hepatitis_b, 'Antivirales, reposo y evitar alcohol.').

% ==========================================================
% SINTOMAS REGISTRADOS DEL PACIENTE
% ==========================================================

:- dynamic sintoma/2.

reset_paciente(P) :- retractall(sintoma(P,_)).

% ==========================================================
% DETECCION DE SINTOMAS AFIRMADOS Y NEGADOS
% ==========================================================

contiene_sintoma(Input, S) :-
    tiene_sintoma(_, S),
    atom_string(S, SS),
    sub_string(Input, _, _, _, SS).

sintoma_negado(Input, S) :-
    atom_string(S, SS),
    ( sub_string(Input, _, _, _, "no ");
      sub_string(Input, _, _, _, "no tengo ");
      sub_string(Input, _, _, _, "nunca ")),
    sub_string(Input, _, _, _, SS).

filtrar_sintomas([], _, [], []).
filtrar_sintomas([S|R], Input, [S|Neg], Af) :-
    sintoma_negado(Input, S), !,
    filtrar_sintomas(R, Input, Neg, Af).
filtrar_sintomas([S|R], Input, Neg, [S|Af]) :-
    filtrar_sintomas(R, Input, Neg, Af).

contiene_sintomas_filtrados(Input, AfirmadosUnicos, NegadosUnicos) :-
    findall(S, contiene_sintoma(Input, S), Todos),
    filtrar_sintomas(Todos, Input, Negados, Afirmados),
    sort(Negados, NegadosUnicos),
    sort(Afirmados, AfirmadosUnicos).


% ==========================================================
% DIAGNOSTICO BASICO (se usa en varias actividades)
% ==========================================================

diagnostico_basico(P, E) :-
    tiene_sintoma(E, S),
    sintoma(P, S).

% ==========================================================
% ACTIVIDAD 2: DIAGNOSTICO POR SINTOMA EXCLUSIVO
% ==========================================================

diagnostico_exclusivo(P, E) :-
    tiene_sintoma(E, S),
    \+ (tiene_sintoma(Otra, S), Otra \= E),
    sintoma(P, S).

% ==========================================================
% ACTIVIDAD 3: PROBABILIDAD
% ==========================================================

probabilidad(P, E, Porc) :-
    setof(Enf, S^tiene_sintoma(Enf,S), Enfermedades),
    member(E, Enfermedades),
    findall(S, tiene_sintoma(E,S), L1),
    length(L1, Total),
    Total > 0,
    findall(S2, (tiene_sintoma(E,S2), sintoma(P,S2)), L2),
    length(L2, C),
    Porc is (C * 100) / Total.

% ==========================================================
% ACTIVIDAD 4: DIAGNOSTICO PREVENTIVO
% ==========================================================

diagnostico_preventivo(P, E) :-
    probabilidad(P, E, Porc),
    Porc > 0,
    Porc < 100.

% ==========================================================
% ACTIVIDAD 5: ENFERMEDADES SIMILARES
% ==========================================================
listar_enfermedades_similares(E) :-
    findall(Otra,
        enfermedades_similares(E, Otra),
        Lista),
    sort(Lista, Unicas),
    ( Unicas = [] ->
        write('No existen enfermedades similares a '),
        write(E), nl
    ;
        write('Enfermedades similares a '),
        write(E), write(': '),
        write(Unicas), nl
    ).

% ==========================================================
% ACTIVIDAD 6: SINTOMAS CONTRADICTORIOS
% ==========================================================

contradictorio(fiebre, ictericia).
contradictorio(convulsiones, fatiga).
contradictorio(tos, dolor_abdomen).
contradictorio(somnolencia, dificultad_respirar).

sintomas_contradictorios(P) :-
    sintoma(P, S1),
    sintoma(P, S2),
    S1 \= S2,
    (contradictorio(S1,S2) ; contradictorio(S2,S1)),
    write('Alerta: sintomas contradictorios detectados: '),
    write(S1), write(' y '), write(S2), nl,
    !.

sintomas_contradictorios(_) :-
    write('No se detectan sintomas contradictorios.'), nl.

% ==========================================================
% ACTIVIDAD 7: ARBOL DE DECISION
% ==========================================================

arbol_diagnostico(P, neumonia) :-
    sintoma(P, tos),
    sintoma(P, dificultad_respirar), !.

arbol_diagnostico(P, meningitis) :-
    sintoma(P, rigidez_cuello),
    sintoma(P, convulsiones), !.

arbol_diagnostico(P, hepatitis_b) :-
    sintoma(P, ictericia),
    sintoma(P, orina_oscura), !.

% ==========================================================
% ACTIVIDAD 8: NIVEL DE RIESGO
% ==========================================================

% Enfermedades consideradas graves
grave(meningitis).
grave(hepatitis_b).


% Riesgo ALTO: enfermedad grave + probabilidad alta
riesgo(P, E, alto) :-
    grave(E),
    probabilidad(P, E, Pct),
    Pct >= 60.

% Riesgo MEDIO: enfermedad no grave o probabilidad moderada
riesgo(P, E, medio) :-
    \+ grave(E),
    probabilidad(P, E, Pct),
    Pct >= 30.

% Riesgo BAJO: pocos sintomas
riesgo(P, E, bajo) :-
    probabilidad(P, E, Pct),
    Pct > 0,
    Pct < 30.


% ==========================================================
% ACTIVIDAD 9: TRATAMIENTO COMBINADO
% ==========================================================

tratamiento_combinado(P, ListaTratamientos) :-
    findall(T,
        ( diagnostico_basico(P, E),
          tratamiento(E, T)
        ),
        L),
    sort(L, ListaTratamientos).
% ==========================================================
% ACTIVIDAD 10: RECOMENDACIONES
% ==========================================================

recomendacion(_, neumonia,
    'Reposo, hidratacion, antibioticos si fueron recetados y supervision medica.').

recomendacion(_, meningitis,
    'Acudir de inmediato a urgencias. Es una condicion grave que requiere atencion hospitalaria.').

recomendacion(_, hepatitis_b,
    'Reposo, antivirales si fueron recetados, evitar alcohol y seguimiento medico.').

% Por si llega una enfermedad sin recomendacion especifica:
recomendacion(_, _, 'No tengo una recomendacion especifica para esta condicion, consulta a un medico.').

% ==========================================================
% ACTIVIDAD 11: DIAGNOSTICAR Y TRATAR
% ==========================================================

diagnosticar_y_tratar(P, E, T) :-
    diagnostico_basico(P, E),
    tratamiento(E, T).

% ==========================================================
% ACTIVIDAD 12: REPORTE COMPLETO
% ==========================================================

reporte(P) :-
    write('=== REPORTE DEL PACIENTE ==='), nl,

    findall(S, sintoma(P,S), LS),
    write('Sintomas confirmados: '), write(LS), nl,

    findall(E, diagnostico_basico(P,E), LE1),
    sort(LE1, LE),
    write('Posibles enfermedades: '), write(LE), nl,

    write('Probabilidad por enfermedad:'), nl,
    forall(member(E, LE),
        (probabilidad(P,E,Pct),
         write('- '), write(E), write(': '), write(Pct), write('%'), nl)),

    write('Buscando sintomas contradictorios...'), nl,
    sintomas_contradictorios(P),

    write('Evaluacion de riesgo:'), nl,
    forall(
    riesgo(P, E, Nivel),
    ( write('- '), write(E),
      write(': '), write(Nivel), nl )
    ),


    (   arbol_diagnostico(P, DX)
    ->  write('Segun el arbol de decision, posible diagnostico: '), write(DX), nl
    ;   true ),

    (   LE = [Final|_]
    ->  write('Diagnostico sugerido: '), write(Final), nl,
        recomendacion(P, Final, R),
        write('Recomendacion: '), write(R), nl
    ;   write('No se encontro diagnostico definitivo.'), nl).

% ==========================================================
% PREDICCION DIRECTA (USADA EN ANALISIS PROFUNDO)
% ==========================================================

prediccion_directa(E) :-
    findall(S, sintoma(usuario,S), L),
    member(S, L),
    tiene_sintoma(E, S).

% ==========================================================
% MEJOR ENFERMEDAD POR PROBABILIDAD (AUXILIAR)
% ==========================================================

mejor_enfermedad(_, [E], E) :- !.
mejor_enfermedad(P, [E1,E2|Resto], Mejor) :-
    probabilidad(P, E1, P1),
    probabilidad(P, E2, P2),
    (   P1 >= P2
    ->  mejor_enfermedad(P, [E1|Resto], Mejor)
    ;   mejor_enfermedad(P, [E2|Resto], Mejor)
    ).

% ==========================================================
% ANALISIS PROFUNDO INTERACTIVO
% ==========================================================

realizar_analisis_profundo :-
    write('--- ANALISIS PROFUNDO INTERACTIVO ---'), nl,
    write('Respondere unas preguntas para afinar el diagnostico.'), nl, nl,

    % Preguntas dinamicas basadas en enfermedades sospechosas
    findall(S, sintoma(usuario,S), Confirmados),
( Confirmados = [] ->
    write('No tengo sintomas guardados. Primero dime algo como: tengo fiebre y tos.'), nl, !
; posibles_enfermedades(Confirmados, Enfermedades)
),


    (Enfermedades = [] ->
        write('No encontre enfermedades posibles con los sintomas dados.'), nl
    ;
        hacer_preguntas(Enfermedades),
        concluir_diagnostico
    ),
    nl.

% Detecta enfermedades relacionadas con sintomas mencionados
posibles_enfermedades(Sintomas, Enfermedades) :-
    findall(E,
        (tiene_sintoma(E,S), member(S,Sintomas)),
        L),
    sort(L, Enfermedades).

% Hace preguntas solo sobre sintomas faltantes
hacer_preguntas([]).
hacer_preguntas([E|Resto]) :-
    write('Analizando posible enfermedad: '), write(E), nl,
    findall(S, tiene_sintoma(E,S), SintomasE),
    preguntar_sintomas_faltantes(SintomasE),
    hacer_preguntas(Resto).

preguntar_sintomas_faltantes([]).
preguntar_sintomas_faltantes([S|R]) :-
    (sintoma(usuario,S) ->
        true
    ;
        format('Presentas el siguiente sintoma: ~w ? (si/no): ', [S]),
        read(Resp),
        (Resp = si -> assertz(sintoma(usuario,S)) ; true)
    ),
    preguntar_sintomas_faltantes(R).

% Concluir diagnostico final tras preguntas
concluir_diagnostico :-
    write('Calculando diagnostico probable...'), nl,
    findall(E, tiene_sintoma(E,_), Todas),
    sort(Todas, Unicas),
    mejor_enfermedad(usuario, Unicas, Mejor),
    write('Diagnostico final sugerido: '), write(Mejor), nl,
    probabilidad(usuario, Mejor, P),

write('Nivel de riesgo detectado:'), nl,
(   riesgo(usuario, Mejor, Nivel)
->  write('- '), write(Mejor),
    write(': riesgo '), write(Nivel), nl
;   write('- Riesgo bajo.'), nl
),


    write('Probabilidad estimada: '), write(P), write('%'), nl,
    (recomendacion(usuario, Mejor, R) ->
        write('Recomendacion: '), write(R), nl
    ; true).

% ==========================================================
% CHATBOT PRINCIPAL
% ==========================================================

inicio :-
    nl, write('Bienvenido, soy ELIZA medico.'), nl,
    write('Describe tus sintomas o haz preguntas.'), nl,
    conversacion.

conversacion :-
    write('> '),
    read_line_to_string(user_input, InputRaw),
    string_lower(InputRaw, Input),
    (Input = "adios" ->
        write('Cuidate mucho.'), nl
    ;
        procesar(Input),
        conversacion).

% ==========================================================
% MOTOR DE RESPUESTAS DEL CHATBOT
% ==========================================================

% Usuario describe sintomas
procesar(Input) :-
    contiene_sintomas_filtrados(Input, Afirmados, Negados),
    Afirmados \= [],
    reset_paciente(usuario),
    forall(member(S, Afirmados), assertz(sintoma(usuario, S))),
    write('Gracias por la informacion.'), nl,
    write('Sintomas afirmados: '), write(Afirmados), nl,
    (Negados \= [] ->
        write('Sintomas negados: '), write(Negados), nl
    ; true),
    write('Esto podria estar relacionado con:'), nl,
    listar_enfermedades_relacionadas(Afirmados),
    write('Deseas un analisis mas profundo?'), nl,
    !.

listar_enfermedades_relacionadas(Afirmados) :-
    findall(E, (member(S, Afirmados), tiene_sintoma(E, S)), L),
    sort(L, Unicas),
    forall(member(E, Unicas), (write('- '), write(E), nl)).


% Usuario dice SI
procesar(Input) :-
    member(Input, ["si","claro","ok","haz un analisis","analiza","profundo","dale"]),
    realizar_analisis_profundo, !.

% Usuario dice NO
procesar(Input) :-
    member(Input, ["no","nel","para nada","no gracias"]),
    write('De acuerdo. Puedes seguir describiendo sintomas o preguntando.'), nl, !.

% Pregunta sintomas de una enfermedad
procesar(Input) :-
    sub_string(Input,_,_,_,"sintoma"),
    extraer_enfermedad(Input,E),
    (E = desconocida ->
        write('No reconozco esa enfermedad.'), nl
    ;
        forall(tiene_sintoma(E,S),(write('- '),write(S),nl))
    ), !.

% Pregunta tratamiento
procesar(Input) :-
    sub_string(Input,_,_,_,"tratamiento"),
    extraer_enfermedad(Input,E),
    tratamiento(E,T),
    write('Tratamiento recomendado: '), write(T), nl, !.

% Enfermedades similares
procesar(Input) :-
    sub_string(Input,_,_,_,"similar"),
    extraer_enfermedad(Input,E),
    listar_enfermedades_similares(E), !.

% Saludo
procesar(Input) :-
    (sub_string(Input,_,_,_,"hola");
     sub_string(Input,_,_,_,"buenas")),
    write('Hola, como te sientes?'), nl, !.

% Usuario dice "mal"
procesar(Input) :-
    sub_string(Input, _,_,_, "mal"),
    write('Lamento que te sientas mal. Puedes decirme tus sintomas?'), nl, !.

% Pregunta directa sobre riesgo
procesar(Input) :-
    sub_string(Input, _, _, _, "riesgo"),
    write('Evaluando nivel de riesgo segun tus sintomas...'), nl,
    findall((E,N),
        riesgo(usuario, E, N),
        L),
    ( L = [] ->
        write('No se detecta riesgo con la informacion actual.'), nl
    ;
        forall(
            member((Enf,Nivel), L),
            ( write('- '), write(Enf),
              write(': riesgo '), write(Nivel), nl )
        )
    ),
    !.

% Pregunta por tratamiento combinado
procesar(Input) :-
    sub_string(Input, _, _, _, "tratamiento combinado"),
    tratamiento_combinado(usuario, L),
    write('Posibles tratamientos combinados:'), nl,
    forall(member(T, L),
        ( write('- '), write(T), nl )
    ),
    !.

% ==========================================================
% AUXILIARES DE ENFERMEDAD
% ==========================================================

extraer_enfermedad(Input, neumonia) :-
    sub_string(Input,_,_,_,"neumonia"), !.
extraer_enfermedad(Input, meningitis) :-
    sub_string(Input,_,_,_,"meningitis"), !.
extraer_enfermedad(Input, hepatitis_b) :-
    sub_string(Input,_,_,_,"hepatitis"), !.
extraer_enfermedad(_, desconocida).

listar_enfermedades_similares(desconocida) :-
    write('No reconozco esa enfermedad.'), nl, !.

listar_enfermedades_similares(E) :-
    findall(Otra, enfermedades_similares(E, Otra), L),
    (L = [] ->
        write('No existen enfermedades similares a '), write(E), nl
    ;
        write('Enfermedades similares a '), write(E), write(': '), write(L), nl
    ).




% ==========================================================
% BASE DE CONOCIMIENTO: PERSONAJES (DISNEY)
% ==========================================================

personaje(mickey).
personaje(minnie).
personaje(donald).
personaje(goofy).
personaje(stich).
personaje(rapunzel).
personaje(ariel).
personaje(woody).
personaje(buzz).    

tipo_personaje(mickey, raton).
tipo_personaje(minnie, raton).
tipo_personaje(donald, pato).
tipo_personaje(goofy, perro).
tipo_personaje(stich, alien).
tipo_personaje(rapunzel, humana).
tipo_personaje(ariel, sirena).
tipo_personaje(woody, juguete).
tipo_personaje(buzz, juguete).

personalidad(mickey, optimista).
personalidad(minnie, carinosa).
personalidad(donald, enojon).
personalidad(goofy, torpe).
personalidad(stich, travieso).
personalidad(rapunzel, curiosa).
personalidad(ariel, feliz).
personalidad(woody, lider).
personalidad(buzz, valiente).

pelicula(mickey, fantasia).
pelicula(donald, clasicos_disney).
pelicula(goofy, clasicos_disney).
pelicula(minnie, clasicos_disney).
pelicula(stich, lilo_y_stitch).
pelicula(rapunzel, enredados).
pelicula(ariel, la_sirenita).
pelicula(woody, toy_story).
pelicula(buzz, toy_story).


% ===============================
% CONSULTAS SOBRE PERSONAJES
% ===============================

% Pregunta que es un personaje
procesar(Input) :-
    sub_string(Input,_,_,_,"que es"),
    extraer_personaje(Input,P),
    tipo_personaje(P,T),
    write(P), write(' es un '), write(T), nl, !.

%preguntar personalidad
procesar(Input) :-
    sub_string(Input,_,_,_,"personalidad"),
    extraer_personaje(Input,P),
    personalidad(P,Per),
    write('La personalidad de '), write(P),
    write(' es '), write(Per), nl, !.

%preguntar pelicula
procesar(Input) :-
    sub_string(Input,_,_,_,"pelicula"),
    extraer_personaje(Input,P),
    pelicula(P,Film),
    write(P), write(' aparece en la pelicula '),
    write(Film), nl, !.

% ==========================================================
% Auxiliar para detectar personajes
% ==========================================================
extraer_personaje(Input, stich) :-
    sub_string(Input,_,_,_,"stich"), !.
extraer_personaje(Input, rapunzel) :-
    sub_string(Input,_,_,_,"rapunzel"), !.
extraer_personaje(Input, ariel) :-
    sub_string(Input,_,_,_,"ariel"), !.
extraer_personaje(Input, woody) :-
    sub_string(Input,_,_,_,"woody"), !.
extraer_personaje(Input, buzz) :-
    sub_string(Input,_,_,_,"buzz"), !.
extraer_personaje(_, desconocido).



% ==========================================================
% ARBOL GENEALOGICO
% ==========================================================


hombre(juan).
hombre(carlos).
hombre(pedro).
hombre(luis).

mujer(maria).
mujer(ana).
mujer(laura).
mujer(sofia).

padre(juan, carlos).
padre(juan, ana).

madre(maria, carlos).
madre(maria, ana).

padre(carlos, pedro).
madre(laura, pedro).

padre(luis, sofia).
madre(ana, sofia).

% ==========================================================
% REGLAS FAMILIARES
% ==========================================================

hermano(X,Y) :-
    padre(P,X), padre(P,Y),
    madre(M,X), madre(M,Y),
    X \= Y.

abuelo(A,N) :-
    padre(A,P),
    (padre(P,N); madre(P,N)).

abuela(A,N) :-
    madre(A,P),
    (padre(P,N); madre(P,N)).

tio(T,N) :-
    hermano(T,P),
    (padre(P,N); madre(P,N)).

primo(X,Y) :-
    padre(PX,X),
    padre(PY,Y),
    hermano(PX,PY),
    X \= Y.

%PADRE
procesar(Input) :-
    sub_string(Input,_,_,_,"padre"),
    extraer_persona(Input,P),
    ( padre(X,P) ->
        write('El padre de '), write(P), write(' es '), write(X), nl
    ;
        write('No tengo registrado el padre de '), write(P), nl
    ),
    !.

%MADRE
procesar(Input) :-
    sub_string(Input,_,_,_,"madre"),
    extraer_persona(Input,P),
    ( madre(X,P) ->
        write('La madre de '), write(P), write(' es '), write(X), nl
    ;
        write('No tengo registrada la madre de '), write(P), nl
    ),
    !.

%HERMANOS
procesar(Input) :-
    sub_string(Input,_,_,_,"hermano"),
    extraer_persona(Input,P),
    findall(H, hermano(H,P), L),
    ( L = [] ->
        write(P), write(' no tiene hermanos registrados.'), nl
    ;
        write('Los hermanos de '), write(P), write(' son: '), write(L), nl
    ),
    !.

%ABUELOS
procesar(Input) :-
    sub_string(Input,_,_,_,"abuelo"),
    extraer_persona(Input,P),
    findall(A, abuelo(A,P), L),
    ( L = [] ->
        write('No tengo informacion de los abuelos de '), write(P), nl
    ;
        write('Los abuelos de '), write(P), write(' son: '), write(L), nl
    ),
    !.

extraer_persona(Input, Persona) :-
    member(Persona, [juan,carlos,pedro,luis,maria,ana,laura,sofia]),
    atom_string(Persona,S),
    sub_string(Input,_,_,_,S),
    !.

procesar(_) :-
    write('No entendi tu pregunta. Puedes reformularla?'), nl.


