% ==========================================================
%   SISTEMA EXPERTO MEDICO - medicoextendido.pl
%   Base original + Actividades 1 a 12
% ==========================================================

% ==========================================================
% HECHOS: Enfermedades y Sintomas (BASE ORIGINAL)
% ==========================================================

tiene_sintoma(gripe, fiebre).
tiene_sintoma(gripe, dolor_cabeza).
tiene_sintoma(gripe, congestion).

tiene_sintoma(alergia, estornudos).
tiene_sintoma(alergia, picazon_ojos).
tiene_sintoma(alergia, congestion).

tiene_sintoma(migrana, dolor_cabeza_severo).
tiene_sintoma(migrana, sensibilidad_luz).
tiene_sintoma(migrana, nauseas).

tiene_sintoma(resfriado, estornudos).
tiene_sintoma(resfriado, congestion).
tiene_sintoma(resfriado, dolor_garganta).

% ==========================================================
% ACTIVIDAD 1: Nuevas enfermedades y sintomas
% ==========================================================

% covid
tiene_sintoma(covid, fiebre).
tiene_sintoma(covid, tos_seca).
tiene_sintoma(covid, dificultad_respirar).

% sinusitis
tiene_sintoma(sinusitis, dolor_cabeza).
tiene_sintoma(sinusitis, congestion).
tiene_sintoma(sinusitis, dolor_cara).

% ==========================================================
% HECHOS: Tratamientos (BASE ORIGINAL + NUEVOS)
% ==========================================================

tratamiento(gripe,     'Reposo, hidratacion, paracetamol y aislamiento.').
tratamiento(alergia,   'Antihistaminicos y evitar el alergeno conocido.').
tratamiento(migrana,   'Medicacion especifica, ambiente oscuro y tranquilo.').
tratamiento(resfriado, 'Liquidos calientes, descongestionantes y vitamina C.').

tratamiento(covid,     'Aislamiento, antipireticos, hidratacion y vigilancia de dificultad respiratoria.').
tratamiento(sinusitis, 'Analgesicos, descongestionantes y lavado nasal con solucion salina.').

% ==========================================================
% PREDICADO DINAMICO (BASE ORIGINAL)
% ==========================================================

:- dynamic sintoma/2.

reset_paciente(P) :- retractall(sintoma(P,_)).

% Sistema de interaccion
pregunta(Paciente, Sintoma) :-
    sintoma(Paciente, Sintoma), !.

pregunta(Paciente, Sintoma) :-
    write('¿El paciente '), write(Paciente),
    write(' tiene '), write(Sintoma), write('? (si/no): '),
    read(Resp),
    ( Resp = si ->
        assertz(sintoma(Paciente, Sintoma))
    ;
        fail
    ).

% ==========================================================
% DIAGNOSTICO BASICO (BASE ORIGINAL)
% ==========================================================

diagnostico_basico(Paciente, Enfermedad) :-
    tiene_sintoma(Enfermedad, S),
    pregunta(Paciente, S).

% ==========================================================
% DIAGNOSTICO COMPLETO (BASE ORIGINAL)
% ==========================================================

diagnostico_completo(Paciente, Enfermedad) :-
    findall(S, tiene_sintoma(Enfermedad, S), Lista),
    todos_confirmados(Paciente, Lista).

todos_confirmados(_, []).
todos_confirmados(Paciente, [S|R]) :-
    pregunta(Paciente, S),
    todos_confirmados(Paciente, R).

% ==========================================================
% DISTINCION FUERTE Y TRATAMIENTOS (BASE ORIGINAL)
% ==========================================================

distincion_fuerte(P, gripe) :-
    diagnostico_basico(P, gripe),
    pregunta(P, fiebre),
    \+ pregunta(P, estornudos).

distincion_fuerte(P, resfriado) :-
    diagnostico_basico(P, resfriado),
    pregunta(P, estornudos),
    \+ pregunta(P, fiebre).

obtener_tratamiento(P, Trat) :-
    (distincion_fuerte(P, E) ; diagnostico_basico(P, E)),
    tratamiento(E, Trat).

% ==========================================================
% SEVERIDAD (BASE ORIGINAL)
% ==========================================================

contar_sintomas_confirmados(P, Enfermedad, C) :-
    findall(S, (tiene_sintoma(Enfermedad,S), sintoma(P,S)), L),
    length(L, C).

severidad(P, E, 'Severa') :-
    contar_sintomas_confirmados(P, E, C), C >= 3, !.

severidad(P, E, 'Moderada') :-
    contar_sintomas_confirmados(P, E, C), C = 2, !.

severidad(P, E, 'Leve') :-
    contar_sintomas_confirmados(P, E, C), C = 1, !.

% ==========================================================
% ACTIVIDAD 2: Diagnostico por sintoma exclusivo
% ==========================================================
% diagnostico_exclusivo(Paciente, Enfermedad).
% Se cumple si el paciente confirma un sintoma que solo pertenece a esa enfermedad.

diagnostico_exclusivo(P, Enfermedad) :-
    tiene_sintoma(Enfermedad, S),
    \+ (tiene_sintoma(Otra, S), Otra \= Enfermedad),
    pregunta(P, S).

% ==========================================================
% ACTIVIDAD 3: Diagnostico por probabilidad
% ==========================================================
% probabilidad(Paciente, Enfermedad, Porcentaje).

probabilidad(P, Enfermedad, Porcentaje) :-
    findall(S, tiene_sintoma(Enfermedad,S), Todos),
    length(Todos, Total),
    Total > 0,
    findall(SC, (tiene_sintoma(Enfermedad,SC), sintoma(P,SC)), Confirmados),
    length(Confirmados, C),
    Porcentaje is (C * 100) / Total.

% ==========================================================
% ACTIVIDAD 4: Diagnostico preventivo
% ==========================================================
% diagnostico_preventivo(Paciente, Enfermedad).
% Se activa si el paciente tiene al menos 1 sintoma pero no todos.

diagnostico_preventivo(P, Enfermedad) :-
    contar_sintomas_confirmados(P, Enfermedad, C),
    findall(S, tiene_sintoma(Enfermedad,S), L),
    length(L, Total),
    C >= 1,
    C < Total.

% ==========================================================
% ACTIVIDAD 5: Enfermedades similares
% ==========================================================
% enfermedades_similares(E1, E2).
% Similares si comparten al menos dos sintomas.

enfermedades_similares(E1, E2) :-
    E1 \= E2,
    findall(S, (tiene_sintoma(E1,S), tiene_sintoma(E2,S)), L),
    length(L, C),
    C >= 2.

% ==========================================================
% ACTIVIDAD 6: Sintomas contradictorios
% ==========================================================

contradictorio(fiebre, picazon_ojos).
contradictorio(nauseas, estornudos).
contradictorio(dificultad_respirar, dolor_garganta).

sintomascontradictorios(P) :-
    sintoma(P, S1),
    sintoma(P, S2),
    S1 \= S2,
    (contradictorio(S1,S2) ; contradictorio(S2,S1)),
    write('El paciente tiene sintomas contradictorios: '),
    write(S1), write(' y '), write(S2), nl,
    !.

% ==========================================================
% ACTIVIDAD 7: Arbol de decision medico
% ==========================================================
% arbol_diagnostico(Paciente, Enfermedad).

arbol_diagnostico(P, gripe) :-
    pregunta(P, fiebre),
    pregunta(P, dolor_cabeza),
    pregunta(P, congestion),
    !.

arbol_diagnostico(P, resfriado) :-
    pregunta(P, estornudos),
    pregunta(P, dolor_garganta),
    \+ pregunta(P, fiebre),
    !.

arbol_diagnostico(P, alergia) :-
    pregunta(P, estornudos),
    pregunta(P, picazon_ojos),
    !.

arbol_diagnostico(P, migrana) :-
    pregunta(P, dolor_cabeza_severo),
    pregunta(P, sensibilidad_luz),
    !.

arbol_diagnostico(P, covid) :-
    pregunta(P, fiebre),
    pregunta(P, tos_seca),
    pregunta(P, dificultad_respirar),
    !.

arbol_diagnostico(P, sinusitis) :-
    pregunta(P, dolor_cabeza),
    pregunta(P, dolor_cara),
    pregunta(P, congestion),
    !.

% ==========================================================
% ACTIVIDAD 8: Enfermedades graves con riesgo
% ==========================================================
% riesgo(Paciente, Enfermedad, Nivel).

grave(covid).
grave(migrana).

riesgo(P, E, alto) :-
    grave(E),
    severidad(P, E, 'Severa'),
    !.

riesgo(P, E, medio) :-
    severidad(P, E, 'Moderada'),
    !.

riesgo(P, E, bajo) :-
    severidad(P, E, 'Leve'),
    !.

% ==========================================================
% ACTIVIDAD 9: Tratamiento combinado
% ==========================================================
% tratamiento_combinado(Paciente, Lista).

tratamiento_combinado(P, ListaTratamientos) :-
    findall(T, (diagnostico_basico(P, E), tratamiento(E, T)), L),
    sort(L, ListaTratamientos).

% ==========================================================
% ACTIVIDAD 10: Recomendaciones segun severidad
% ==========================================================
% recomendacion(Paciente, Enfermedad, Texto).

recomendacion(P, E, Texto) :-
    severidad(P, E, 'Severa'), !,
    Texto = 'Caso severo: acudir de inmediato a urgencias y no automedicarse.'.
recomendacion(P, E, Texto) :-
    severidad(P, E, 'Moderada'), !,
    Texto = 'Caso moderado: acudir a consulta medica pronto y seguir tratamiento.'.
recomendacion(P, E, Texto) :-
    severidad(P, E, 'Leve'), !,
    Texto = 'Caso leve: reposo, hidratacion y vigilancia de sintomas.'.

% ==========================================================
% ACTIVIDAD 11: Diagnosticar y tratar en un solo paso
% ==========================================================
% diagnosticar_y_tratar(Paciente, Diagnostico, Tratamiento).

diagnosticar_y_tratar(P, Diagnostico, Tratamiento) :-
    (   distincion_fuerte(P, Diagnostico)
    ;   diagnostico_completo(P, Diagnostico)
    ;   diagnostico_basico(P, Diagnostico)
    ),
    tratamiento(Diagnostico, Tratamiento).

% ==========================================================
% ACTIVIDAD 12: Reporte completo
% ==========================================================
% reporte(Paciente).

reporte(P) :-
    nl, write('================= REPORTE DEL PACIENTE '), write(P), write(' ================='), nl,

    % Sintomas confirmados
    findall(S, sintoma(P,S), Sintomas),
    write('Sintomas confirmados: '), write(Sintomas), nl,

    % Enfermedades posibles
    (   setof(E, S^(sintoma(P,S), tiene_sintoma(E,S)), EnfermedadesPosibles)
    ->  true
    ;   EnfermedadesPosibles = []
    ),
    write('Enfermedades posibles: '), write(EnfermedadesPosibles), nl,

    % Probabilidades
    write('Probabilidad por enfermedad:'), nl,
    forall(member(E1, EnfermedadesPosibles),
           (probabilidad(P,E1,Porc),
            write('  - '), write(E1), write(': '), write(Porc), write(' %'), nl)),
    
    % Elegir diagnostico final como el de mayor probabilidad
    mejor_enfermedad(P, EnfermedadesPosibles, DiagnosticoFinal),
    nl, write('Diagnostico final sugerido: '), write(DiagnosticoFinal), nl,

    % Severidad
    (   severidad(P, DiagnosticoFinal, Sev)
    ->  write('Severidad: '), write(Sev), nl
    ;   write('Severidad: no disponible.'), nl
    ),

    % Tratamiento
    (   tratamiento(DiagnosticoFinal, Trat)
    ->  write('Tratamiento recomendado: '), write(Trat), nl
    ;   write('Tratamiento: no disponible.'), nl
    ),

    % Recomendacion
    (   recomendacion(P, DiagnosticoFinal, TextoRec)
    ->  write('Recomendacion: '), write(TextoRec), nl
    ;   write('Recomendacion: no disponible.'), nl
    ),

    % Riesgo (opcional, si aplica)
    (   riesgo(P, DiagnosticoFinal, Nivel)
    ->  write('Nivel de riesgo: '), write(Nivel), nl
    ;   true
    ),

    nl, write('=============================================================='), nl.

% Auxiliar para encontrar la mejor enfermedad (mayor probabilidad)
mejor_enfermedad(_, [E], E) :- !.
mejor_enfermedad(P, [E1,E2|Resto], Mejor) :-
    probabilidad(P, E1, P1),
    probabilidad(P, E2, P2),
    (   P1 >= P2
    ->  mejor_enfermedad(P, [E1|Resto], Mejor)
    ;   mejor_enfermedad(P, [E2|Resto], Mejor)
    ).
