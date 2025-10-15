division(Dividendo, Divisor, 0, Dividendo) :-
    Dividendo < Divisor.

division(Dividendo, Divisor, Cociente, Resto) :-
    Dividendo >= Divisor,
    Dividendo1 is Dividendo - Divisor,
    division(Dividendo1, Divisor, Cociente1, Resto),  % ← AQUÍ se llama a sí misma (RECURSIÓN)
    Cociente is Cociente1 + 1.