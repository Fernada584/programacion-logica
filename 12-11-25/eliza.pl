% ======================================================
% SISTEMA EXPERTO MEDICO "ELIZA"
% Version con menus anidados
% ======================================================

inicio :-
    nl, write('==============================================='), nl,
    write('          ELIZA - Sistema Medico en Prolog'), nl,
    write('==============================================='), nl, nl,
    write('Puedo brindarte informacion sobre estas enfermedades:'), nl,
    write('1. Neumonia'), nl,
    write('2. Meningitis'), nl,
    write('3. Hepatitis B'), nl,
    write('4. Salir'), nl, nl,
    write('Selecciona una opcion (1-4): '),
    read(Opcion),
    menu_principal(Opcion).

% -----------------------------------------
% Menu principal
% -----------------------------------------
menu_principal(1) :- menu_enfermedad(neumonia).
menu_principal(2) :- menu_enfermedad(meningitis).
menu_principal(3) :- menu_enfermedad(hepatitis_b).
menu_principal(4) :-
    nl, write('Cuidate mucho. Recuerda que esto no sustituye la consulta medica profesional.'), nl, !.
menu_principal(_) :-
    nl, write('Opcion no valida. Intenta de nuevo.'), nl, inicio.

% -----------------------------------------
% Submenu de cada enfermedad
% -----------------------------------------
menu_enfermedad(Enfermedad) :-
    repeat,
    nl, write('==============================='), nl,
    write(' Informacion sobre: '), write(Enfermedad), nl,
    write('==============================='), nl,
    write('1. Sintomas'), nl,
    write('2. Causas'), nl,
    write('3. Tratamiento / Medicamentos'), nl,
    write('4. Medico especialista'), nl,
    write('5. Centros de salud recomendados'), nl,
    write('6. Cuidados y precauciones'), nl,
    write('7. Volver al menu principal'), nl,
    write('Selecciona una opcion (1-7): '),
    read(Opcion),
    (   Opcion == 7 -> inicio
    ;   mostrar_info(Enfermedad, Opcion),
        fail
    ).

% -----------------------------------------
% Base de conocimiento por seccion
% -----------------------------------------
% NEUMONIA
mostrar_info(neumonia, 1) :-
    nl, write('Sintomas de la neumonia:'), nl,
    write('- Tos con flema o seca'), nl,
    write('- Fiebre y escalofrios'), nl,
    write('- Dolor en el pecho'), nl,
    write('- Dificultad para respirar o fatiga'), nl, !.
mostrar_info(neumonia, 2) :-
    nl, write('Causas de la neumonia:'), nl,
    write('- Infecciones por bacterias, virus o hongos'), nl,
    write('- Tabaquismo o enfermedades pulmonares cronicas'), nl, !.
mostrar_info(neumonia, 3) :-
    nl, write('Tratamiento y medicamentos para la neumonia:'), nl,
    write('- Antibioticos (si es bacteriana)'), nl,
    write('- Antivirales o antifungicos segun el agente'), nl,
    write('- Hidratacion y reposo'), nl, !.
mostrar_info(neumonia, 4) :-
    nl, write('Medico especialista:'), nl,
    write('- Neumologo o medico general'), nl, !.
mostrar_info(neumonia, 5) :-
    nl, write('Centros de salud recomendados:'), nl,
    write('- Hospital General de tu localidad'), nl,
    write('- Clinicas del IMSS o ISSSTE'), nl,
    write('- Centros de salud municipales'), nl, !.
mostrar_info(neumonia, 6) :-
    nl, write('Cuidados y precauciones:'), nl,
    write('- Evitar el tabaco y el alcohol'), nl,
    write('- Mantener buena higiene de manos'), nl,
    write('- Vacunarse contra la gripe y el neumococo'), nl, !.

% MENINGITIS
mostrar_info(meningitis, 1) :-
    nl, write('Sintomas de la meningitis:'), nl,
    write('- Dolor de cabeza intenso'), nl,
    write('- Fiebre alta'), nl,
    write('- Rigidez de cuello'), nl,
    write('- Nauseas, vomitos, somnolencia o confusion'), nl, !.
mostrar_info(meningitis, 2) :-
    nl, write('Causas de la meningitis:'), nl,
    write('- Infecciones virales o bacterianas (meningococo, neumococo, H. influenzae)'), nl, !.
mostrar_info(meningitis, 3) :-
    nl, write('Tratamiento y medicamentos para la meningitis:'), nl,
    write('- Antibioticos intravenosos si es bacteriana'), nl,
    write('- Antivirales si es viral'), nl,
    write('- Hospitalizacion y observacion medica'), nl, !.
mostrar_info(meningitis, 4) :-
    nl, write('Medico especialista:'), nl,
    write('- Neurologo o infectologo'), nl, !.
mostrar_info(meningitis, 5) :-
    nl, write('Centros de salud recomendados:'), nl,
    write('- Hospital de Especialidades o Neurologia'), nl,
    write('- Hospital General mas cercano'), nl, !.
mostrar_info(meningitis, 6) :-
    nl, write('Cuidados y precauciones:'), nl,
    write('- Vacunarse contra meningococo, neumococo y Haemophilus influenzae tipo b'), nl,
    write('- Evitar el contacto con personas infectadas'), nl,
    write('- Acudir a urgencias si hay fiebre alta con rigidez de cuello o convulsiones'), nl, !.

% HEPATITIS B
mostrar_info(hepatitis_b, 1) :-
    nl, write('Sintomas de la hepatitis B:'), nl,
    write('- Color amarillento de la piel y ojos (ictericia)'), nl,
    write('- Orina oscura y heces claras'), nl,
    write('- Fatiga, nauseas y vomitos'), nl,
    write('- Dolor abdominal o inflamacion del higado'), nl, !.
mostrar_info(hepatitis_b, 2) :-
    nl, write('Causas de la hepatitis B:'), nl,
    write('- Transmision por sangre, relaciones sexuales o de madre a hijo durante el parto'), nl, !.
mostrar_info(hepatitis_b, 3) :-
    nl, write('Tratamiento y medicamentos para la hepatitis B:'), nl,
    write('- Antivirales para controlar la infeccion cronica'), nl,
    write('- Reposo y dieta balanceada'), nl,
    write('- Evitar el alcohol y medicamentos hepatotoxicos'), nl, !.
mostrar_info(hepatitis_b, 4) :-
    nl, write('Medico especialista:'), nl,
    write('- Gastroenterologo o hepatologo'), nl, !.
mostrar_info(hepatitis_b, 5) :-
    nl, write('Centros de salud recomendados:'), nl,
    write('- Hospital de enfermedades hepaticas'), nl,
    write('- Hospital General de tu zona o clinica IMSS/ISSSTE'), nl, !.
mostrar_info(hepatitis_b, 6) :-
    nl, write('Cuidados y precauciones:'), nl,
    write('- Vacunarse contra la hepatitis B'), nl,
    write('- Evitar compartir agujas, rastrillos o cepillos de dientes'), nl,
    write('- Usar proteccion en relaciones sexuales'), nl,
    write('- Realizar analisis de sangre de forma periodica'), nl, !.

% Clausula comodin: ahora SOLO falla (no imprime nada).
mostrar_info(_, _) :- fail.
