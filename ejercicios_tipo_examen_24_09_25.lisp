#Ejercicio 1
Escribe una función llamada n-esimo que reciba como parámetros un número n y una lista lista. 
La función debe devolver el n-ésimo elemento de la lista utilizando únicamente las funciones car y cdr.

(defun n-esimo (n lista)
  (if (= n 0)
      (car lista)                   ; caso base → primer elemento
      (n-esimo (- n 1) (cdr lista)))); recursión sobre la cola

#Ejecicio 2
Escribe una función (filtra positivos de lista) que reciba una lista de números 
y devuelva una lista con solo los números positivos usa when, dentro de un mapear o loop

(defun filtra-positivos (lista)
  (remove nil
          (mapcar (lambda (x)
                    (when (> x 0) x)) ; solo devuelve si es positivo
                  lista)))
#Ejercicio 3
Escriba una función (clasifica número n) que devuelva negativo si n < 0, cero si n = 0, 
pequeño si 1<= n <= 10, mediano si 11<=n<=100 y grande si n>100 ejemplo: clasifica numero 57) = mediano

(defun clasifica-numero (n)
  (cond
    ((< n 0) 'negativo)
    ((= n 0) 'cero)
    ((and (>= n 1) (<= n 10)) 'pequeño)
    ((and (>= n 11) (<= n 100)) 'mediano)
    ((> n 100) 'grande)))

#Ejercicio 4
Saber si el número de una lista es para, hay una función en lisp qué solo te la da

(evenp 4)   ;; => T   (porque 4 es par)
(evenp 7)   ;; => NIL (porque 7 no es par)

#Ejercicio 5
Escribe una función que si la lista edta vacía devuelva lista vacía, 
si el primer elemento el número 3s mayo que 50 devuelva grande, 
si el primer elemento es una subiste devuelva sublusta detectada, en cualquier otro caso devuelva caso general

(defun analiza-lista (lista)
  (cond
    ;; caso 1: lista vacía
    ((null lista) '())

    ;; caso 2: primer elemento es un número > 50
    ((and (numberp (car lista)) (> (car lista) 50)) 'grande)

    ;; caso 3: primer elemento es otra lista
    ((listp (car lista)) 'sublista-detectada)

    ;; caso 4: cualquier otro caso
    (t 'caso-general)))
