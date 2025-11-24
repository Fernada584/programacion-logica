% ================================================
%      HISTORIAL DE SINTOMAS (dinamico)
% ================================================
:- dynamic historial/1.

% Limpia el historial al iniciar
inicio :-
    retractall(historial(_)),
    nl, write('==============================================='), nl,
    write('       ELIZA - Sistema Experto Medico en Prolog'), nl,
    write('==============================================='), nl, nl,
    write('Puedo responder preguntas sobre:'), nl,
    write('- Neumonia'), nl,
    write('- Meningitis'), nl,
    write('- Hepatitis B'), nl, nl,
    write('Tambien puedo analizar tus sintomas.'), nl,
    write('Ejemplo: "tengo fiebre y dolor de cabeza"'), nl,
    write('Despues puedes preguntar: "que enfermedad puedo tener?"'), nl,
    write('Escribe "adios" para salir.'), nl, nl,
    conversacion.


% ================================================
%  BUCLE DE CONVERSACION (lee textos completos)
% ================================================
conversacion :-
    write('> '),
    read_line_to_string(user_input, Input),
    procesar(Input).

procesar("adios") :-
    nl, write('Cuidate mucho. Recuerda que esto no sustituye la consulta medica profesional.'), nl, !.

procesar(Input) :-
    string_lower(Input, Lower),
    guardar_sintomas(Lower),         % intenta guardar sintomas
    explicar_y_responder(Lower),     % responde como chatbot
    conversacion.


% ================================================
%     SISTEMA EXPLICATIVO + RESPUESTA
% ================================================

explicar_y_responder(Lower) :-

    % ---- Diagnostico por historial ----
    (   sub_string(Lower,_,_,_,"enfermedad puedo tener")
    ->  diagnosticar, !
    ;

    % ---- NEUMONIA ----
    (sub_string(Lower,_,_,_,"neumonia"), sub_string(Lower,_,_,_,"sintoma"))
    -> explicar("sintoma","neumonia"), mostrar_info(neumonia,1)
    ;
    (sub_string(Lower,_,_,_,"neumonia"), sub_string(Lower,_,_,_,"causa"))
    -> explicar("causa","neumonia"), mostrar_info(neumonia,2)
    ;
    (sub_string(Lower,_,_,_,"neumonia"), sub_string(Lower,_,_,_,"tratamiento"))
    -> explicar("tratamiento","neumonia"), mostrar_info(neumonia,3)
    ;
    (sub_string(Lower,_,_,_,"neumonia"), sub_string(Lower,_,_,_,"medico"))
    -> explicar("medico","neumonia"), mostrar_info(neumonia,4)
    ;
    (sub_string(Lower,_,_,_,"neumonia"), sub_string(Lower,_,_,_,"centro"))
    -> explicar("centro","neumonia"), mostrar_info(neumonia,5)
    ;
    (sub_string(Lower,_,_,_,"neumonia"), sub_string(Lower,_,_,_,"cuidado"))
    -> explicar("cuidado","neumonia"), mostrar_info(neumonia,6)
    ;

    % ---- MENINGITIS ----
    (sub_string(Lower,_,_,_,"meningitis"), sub_string(Lower,_,_,_,"sintoma"))
    -> explicar("sintoma","meningitis"), mostrar_info(meningitis,1)
    ;
    (sub_string(Lower,_,_,_,"meningitis"), sub_string(Lower,_,_,_,"causa"))
    -> explicar("causa","meningitis"), mostrar_info(meningitis,2)
    ;
    (sub_string(Lower,_,_,_,"meningitis"), sub_string(Lower,_,_,_,"tratamiento"))
    -> explicar("tratamiento","meningitis"), mostrar_info(meningitis,3)
    ;
    (sub_string(Lower,_,_,_,"meningitis"), sub_string(Lower,_,_,_,"medico"))
    -> explicar("medico","meningitis"), mostrar_info(meningitis,4)
    ;
    (sub_string(Lower,_,_,_,"meningitis"), sub_string(Lower,_,_,_,"centro"))
    -> explicar("centro","meningitis"), mostrar_info(meningitis,5)
    ;
    (sub_string(Lower,_,_,_,"meningitis"), sub_string(Lower,_,_,_,"cuidado"))
    -> explicar("cuidado","meningitis"), mostrar_info(meningitis,6)
    ;

    % ---- HEPATITIS B ----
    (sub_string(Lower,_,_,_,"hepatitis"), sub_string(Lower,_,_,_,"sintoma"))
    -> explicar("sintoma","hepatitis b"), mostrar_info(hepatitis_b,1)
    ;
    (sub_string(Lower,_,_,_,"hepatitis"), sub_string(Lower,_,_,_,"causa"))
    -> explicar("causa","hepatitis b"), mostrar_info(hepatitis_b,2)
    ;
    (sub_string(Lower,_,_,_,"hepatitis"), sub_string(Lower,_,_,_,"tratamiento"))
    -> explicar("tratamiento","hepatitis b"), mostrar_info(hepatitis_b,3)
    ;
    (sub_string(Lower,_,_,_,"hepatitis"), sub_string(Lower,_,_,_,"medico"))
    -> explicar("medico","hepatitis b"), mostrar_info(hepatitis_b,4)
    ;
    (sub_string(Lower,_,_,_,"hepatitis"), sub_string(Lower,_,_,_,"centro"))
    -> explicar("centro","hepatitis b"), mostrar_info(hepatitis_b,5)
    ;
    (sub_string(Lower,_,_,_,"hepatitis"), sub_string(Lower,_,_,_,"cuidado"))
    -> explicar("cuidado","hepatitis b"), mostrar_info(hepatitis_b,6)
    ;

    % DEFAULT:
    write('No entendi tu pregunta. Puedes preguntar por sintomas, causas, tratamiento o medico.'), nl
    )).


% ================================================
%     SISTEMA EXPLICATIVO
% ================================================
explicar(Clave, Enfermedad) :-
    nl,
    write("Detecte la palabra relacionada con "), write(Clave), write("."), nl,
    write("Tambien detecte la enfermedad "), write(Enfermedad), write("."), nl,
    write("Por eso te muestro esta informacion:"), nl, nl.


% ================================================
%       GUARDAR HISTORIAL DE SINTOMAS
% ================================================
guardar_sintomas(Texto) :-
    sintomas_lista(Lista),
    forall(
        member(S, Lista),
        (   sub_string(Texto,_,_,_,S)
        ->  assertz(historial(S))
        ;   true
        )
    ).

sintomas_lista([
    "fiebre",
    "tos",
    "dolor de cabeza",
    "rigidez",
    "vomito",
    "nausea",
    "fatiga",
    "dificultad para respirar",
    "ictericia",
    "dolor abdominal"
]).


% ================================================
%         DIAGNOSTICO POR HISTORIAL
% ================================================
diagnosticar :-
    findall(S, historial(S), Sintomas),
    nl, write("He analizado tus sintomas: "), write(Sintomas), nl,

    (   coincide_meningitis(Sintomas)
    ->  write("Podria tratarse de MENINGITIS."), nl
    ;   coincide_neumonia(Sintomas)
    ->  write("Podria tratarse de NEUMONIA."), nl
    ;   coincide_hepatitis(Sintomas)
    ->  write("Podria tratarse de HEPATITIS B."), nl
    ;   write("No puedo identificar una enfermedad con los sintomas dados."), nl
    ).


% Reglas simples (puedes ampliarlas)
coincide_meningitis(S) :-
    member("fiebre", S),
    member("rigidez", S).

coincide_neumonia(S) :-
    member("tos", S),
    member("dificultad para respirar", S).

coincide_hepatitis(S) :-
    member("ictericia", S),
    member("dolor abdominal", S).
