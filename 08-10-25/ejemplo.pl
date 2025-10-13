% Hechos básicos - Padres
padre(julio).
padre(ramiro).
padre(carlos).
padre(roberto).
padre(diego).

% Hechos básicos - Madres
madre(ana).
madre(lucia).
madre(carmen).
madre(sofia).
madre(elena).

% Hechos básicos - Hombres
hombre(julio).
hombre(ramiro).
hombre(juan).
hombre(jose).
hombre(luis).
hombre(antonio).
hombre(carlos).
hombre(roberto).
hombre(diego).
hombre(miguel).
hombre(pedro).

% Hechos básicos - Mujeres
mujer(maria).
mujer(antonia).
mujer(ana).
mujer(lucia).
mujer(carmen).
mujer(sofia).
mujer(elena).
mujer(rosa).
mujer(laura).
mujer(patricia).

% Relaciones padre/madre de
padrede(jose, juan).
padrede(juan, jose).
padrede(juan, luis).
padrede(juan, maria).
padrede(juan, antonia).
padrede(jose, antonio).
padrede(carlos, miguel).
padrede(carlos, rosa).
padrede(roberto, pedro).
padrede(roberto, laura).
padrede(diego, patricia).

% Relaciones madre de
madrede(maria, juan).
madrede(ana, jose).
madrede(ana, luis).
madrede(lucia, maria).
madrede(lucia, antonia).
madrede(carmen, antonio).
madrede(sofia, miguel).
madrede(sofia, rosa).
madrede(elena, pedro).
madrede(elena, laura).
madrede(antonia, patricia).

% Regla: X es padre de Y si X es padre y es padre de Y
padre(X, Y) :- hombre(X), padrede(X, Y).

% Regla: X es madre de Y si X es mujer y es madre de Y
madre(X, Y) :- mujer(X), madrede(X, Y).

% Regla: X es hijo de Y
hijo(X, Y) :- hombre(X), (padrede(Y, X); madrede(Y, X)).

% Regla: X es hija de Y
hija(X, Y) :- mujer(X), (padrede(Y, X); madrede(Y, X)).

% Regla: X es abuelo de Y
abuelo(X, Y) :- hombre(X), padrede(X, Z), (padrede(Z, Y); madrede(Z, Y)).

% Regla: X es abuela de Y
abuela(X, Y) :- mujer(X), madrede(X, Z), (padrede(Z, Y); madrede(Z, Y)).

% Regla: X e Y son hermanos (mismo padre o misma madre, y ambos son hombres)
hermanos(X, Y) :- 
    hombre(X), 
    hombre(Y), 
    X \= Y,
    (
        (padrede(Z, X), padrede(Z, Y));
        (madrede(Z, X), madrede(Z, Y))
    ).

% Regla: X e Y son hermanas (mismo padre o misma madre, y ambas son mujeres)
hermanas(X, Y) :- 
    mujer(X), 
    mujer(Y), 
    X \= Y,
    (
        (padrede(Z, X), padrede(Z, Y));
        (madrede(Z, X), madrede(Z, Y))
    ).

% Regla: X e Y son hermano y hermana (independiente del género)
hermano_hermana(X, Y) :- 
    X \= Y,
    (
        (padrede(Z, X), padrede(Z, Y));
        (madrede(Z, X), madrede(Z, Y))
    ).

% Regla: X es tío de Y (hermano del padre o madre de Y)
tio(X, Y) :- 
    hombre(X),
    (padrede(Z, Y); madrede(Z, Y)),
    hermano_hermana(X, Z).

% Regla: X es tía de Y (hermana del padre o madre de Y)
tia(X, Y) :- 
    mujer(X),
    (padrede(Z, Y); madrede(Z, Y)),
    hermano_hermana(X, Z).

% Regla: X e Y son primos (sus padres son hermanos)
primos(X, Y) :- 
    X \= Y,
    (padrede(P1, X); madrede(P1, X)),
    (padrede(P2, Y); madrede(P2, Y)),
    P1 \= P2,
    hermano_hermana(P1, P2).

% Ejemplos de consultas:
% ?- hermanos(jose, luis).
% ?- hermanas(maria, antonia).
% ?- hermano_hermana(jose, maria).
% ?- primos(juan, antonio).
% ?- tio(jose, maria).
% ?- tia(antonia, patricia).
% ?- abuelo(juan, patricia).
% ?- abuela(maria, juan).