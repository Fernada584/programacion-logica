% multiplicacion(X, Y, Resultado) - multiplica X por Y usando sumas

% Caso base: cualquier número por 0 es 0
multiplicacion(_, 0, 0).

% Caso recursivo: X * Y = X + (X * (Y-1))
multiplicacion(X, Y, Resultado) :-
    Y > 0,
    Y1 is Y - 1,
    multiplicacion(X, Y1, Resultado1),
    Resultado is X + Resultado1.


% potencia(Base, Exponente, Resultado)

% Caso base: cualquier número elevado a 0 es 1
potencia(_, 0, 1).

% Caso recursivo: Base^Exp = Base * Base^(Exp-1)
potencia(Base, Exponente, Resultado) :-
    Exponente > 0,
    Exponente1 is Exponente - 1,
    potencia(Base, Exponente1, Resultado1),
    multiplicacion(Base, Resultado1, Resultado).