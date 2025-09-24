;;; adivina.lisp - Juego "Adivina quién" simple en Common Lisp
(in-package :cl-user)

(defun limpiar (s)
  "Limpia una cadena eliminando espacios y convirtiendo a minúsculas."
  (string-downcase (string-trim " \t\n\r" s)))

(defun si? (s)
  "Determina si una respuesta es afirmativa."
  (let ((respuesta (limpiar s)))
    (member respuesta '("si" "sí" "s" "y" "yes" "1") :test #'string=)))

;; Base de datos de ejemplo: cada miembro es (NOMBRE . plist-de-props)
(defparameter *miembros*
  (list
   (cons "Mamá" '(:genero femenino :pelo largo :gafas nil :edad adulta :cocina si :instrumento nil))
   (cons "Papá" '(:genero masculino :pelo corto :gafas si :edad adulta :cocina no :instrumento guitarra))
   (cons "Hermano" '(:genero masculino :pelo corto :gafas nil :edad joven :cocina no :instrumento nil))
   (cons "Hermana" '(:genero femenino :pelo largo :gafas si :edad joven :cocina si :instrumento piano))
   (cons "Abuela" '(:genero femenino :pelo canoso :gafas si :edad mayor :cocina si :instrumento nil))
   (cons "Tío" '(:genero masculino :pelo corto :gafas nil :edad adulta :cocina no :instrumento sax))))
  
;; Lista de preguntas: cada elemento es (PROMPT PROPIEDAD VALOR-ESPERADO)
(defparameter *preguntas*
  (list
   '("¿Es mujer?" :genero femenino)
   '("¿Tiene el pelo largo?" :pelo largo)
   '("¿Usa gafas?" :gafas si)
   '("¿Es mayor/anciano?" :edad mayor)
   '("¿Es joven (adolescente/joven)?" :edad joven)
   '("¿Le gusta cocinar?" :cocina si)
   '("¿Toca algún instrumento (por ejemplo guitarra/piano)?" :instrumento si)))

(defun valor-de-prop (plist prop)
  "Devuelve el valor de prop en plist (o NIL si no existe)."
  (getf plist prop))

(defun coincide? (valor esperado)
  "Determina si un valor coincide con el esperado."
  (cond
    ((eq esperado 'si) (and valor (not (member valor '(nil no :no :nil)))))
    ((eq esperado 'no) (or (null valor) (member valor '(nil no :no :nil))))
    ((keywordp esperado) (eq valor esperado))
    ((stringp esperado) (and (stringp valor) (string= (string-downcase valor) (string-downcase esperado))))
    (t (eql valor esperado))))

(defun filtrar-candidatos (candidatos prop esperado respuesta-usuario)
  "Filtra candidatos según la respuesta del usuario."
  (let ((acepta (if (si? respuesta-usuario) t nil)))
    (remove-if-not
     (lambda (c)
       (let* ((plist (cdr c))
              (v (valor-de-prop plist prop))
              (cumple (coincide? v esperado)))
         (if acepta cumple (not cumple))))
     candidatos)))

(defun jugar-una-ronda ()
  "Juega una ronda del juego de adivinanzas."
  (format t "~%¡Juguemos a Adivina Quién! Piensa en una persona de la familia.~%~%")
  (let ((candidatos (copy-list *miembros*)))
    (dolist (preg *preguntas*)
      (when (> (length candidatos) 1)
        (destructuring-bind (prompt prop esperado) preg
          (format t "~a (si/no): " prompt)
          (let ((resp (limpiar (read-line))))
            (setf candidatos (filtrar-candidatos candidatos prop esperado resp))
            (format t "Quedan ~d candidatos.~%" (length candidatos))))))
    ;; Mostrar resultado
    (cond
      ((= (length candidatos) 1)
       (format t "~%¡Creo que es: ~a!~%" (car (first candidatos))))
      ((> (length candidatos) 1)
       (format t "~%No pude adivinar exactamente. Posibles candidatos:~%")
       (dolist (r candidatos) 
         (format t "- ~a~%" (car r))))
      ((zerop (length candidatos))
       (format t "~%No hay coincidencias con las respuestas dadas.~%")))))

(defun mostrar-miembros ()
  "Muestra todos los miembros de la base de datos."
  (format t "~%--- Miembros de la familia ---~%")
  (dolist (m *miembros*)
    (format t "- ~a: ~a~%" (car m) (cdr m))))

(defun añadir-miembro ()
  "Añade un nuevo miembro a la base de datos."
  (format t "Nombre: ")
  (let ((nombre (read-line))
        (plist '()))
    (format t "¿Es mujer? (si/no): ")
    (when (si? (read-line))
      (setf plist (list* :genero 'femenino plist)))
    (format t "¿Usa gafas? (si/no): ")
    (when (si? (read-line))
      (setf plist (list* :gafas 'si plist)))
    (format t "¿Le gusta cocinar? (si/no): ")
    (when (si? (read-line))
      (setf plist (list* :cocina 'si plist)))
    (push (cons nombre plist) *miembros*)
    (format t "Miembro añadido.~%")))

(defun menu-principal-debug ()
  (loop
    (format t "~%--- Menú Adivina Quién ---~%")
    (format t "1) Jugar~%2) Mostrar miembros~%3) Añadir miembro~%4) Salir~%> ")
    (let* ((input (read-line))
           (op (limpiar input)))
      (format t "DEBUG: Entrada original: '~a'~%" input)
      (format t "DEBUG: Después de limpiar: '~a'~%" op)
      (cond
        ((member op '("1" "jugar") :test #'string=) 
         (format t "¡Opción 1 reconocida!~%")
         (return))
        ((member op '("2" "mostrar") :test #'string=) 
         (format t "¡Opción 2 reconocida!~%")
         (return))
        (t 
         (format t "Opción no válida: '~a'~%" op))))))

;; Mensaje de carga
(format t "Carga completada. Ejecuta (menu-principal) para jugar.~%")