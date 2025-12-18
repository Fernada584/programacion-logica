(defpackage :eliza-medico
  (:use :cl)
  (:export :start :eliza))

(in-package :eliza-medico)

(setf *random-state* (make-random-state t))

;; ==========================================================
;; Base de conocimiento de enfermedades
;; ==========================================================

(defparameter *enfermedades*
  '(:neumonia :meningitis :hepatitis_b))

(defparameter *sintomas-por-enfermedad*
  '((:neumonia . (tos fiebre dificultad_respirar malestar dolor_cabeza))
    (:meningitis . (fiebre dolor_cabeza rigidez_cuello somnolencia vomito convulsiones))
    (:hepatitis_b . (ictericia fatiga dolor_abdomen orina_oscura))))

(defparameter *tratamientos-por-enfermedad*
  '((:neumonia . "Antibioticos, hidratacion y reposo.")
    (:meningitis . "Antibioticos intravenosos y hospitalizacion.")
    (:hepatitis_b . "Antivirales, reposo y evitar alcohol.")))

(defparameter *recomendaciones-por-enfermedad*
  '((:neumonia . "Reposo, hidratacion, antibioticos si fueron recetados y supervision medica.")
    (:meningitis . "Acudir de inmediato a urgencias. Es una condicion grave que requiere atencion hospitalaria.")
    (:hepatitis_b . "Reposo, antivirales si fueron recetados, evitar alcohol y seguimiento medico.")))

(defparameter *contradicciones*
  '((fiebre . ictericia)
    (convulsiones . fatiga)
    (tos . dolor_abdomen)
    (somnolencia . dificultad_respirar)))

(defparameter *enfermedades-graves* '(:meningitis :hepatitis_b))

;; ==========================================================
;; Personajes Disney
;; ==========================================================

(defparameter *personajes*
  '((mickey :tipo raton :personalidad optimista :pelicula fantasia)
    (minnie :tipo raton :personalidad carinosa :pelicula clasicos_disney)
    (donald :tipo pato :personalidad enojon :pelicula clasicos_disney)
    (goofy :tipo perro :personalidad torpe :pelicula clasicos_disney)
    (stich :tipo alien :personalidad travieso :pelicula lilo_y_stitch)
    (rapunzel :tipo humana :personalidad curiosa :pelicula enredados)
    (ariel :tipo sirena :personalidad feliz :pelicula la_sirenita)
    (woody :tipo juguete :personalidad lider :pelicula toy_story)
    (buzz :tipo juguete :personalidad valiente :pelicula toy_story)))

;; ==========================================================
;; Arbol familiar simple
;; ==========================================================

(defparameter *padre*
  '((juan . carlos)
    (juan . ana)
    (carlos . pedro)
    (luis . sofia)))

(defparameter *madre*
  '((maria . carlos)
    (maria . ana)
    (laura . pedro)
    (ana . sofia)))

(defparameter *hombres* '(juan carlos pedro luis))
(defparameter *mujeres* '(maria ana laura sofia))

;; ==========================================================
;; Utilidades generales
;; ==========================================================

(defparameter *paciente-sintomas*
  (make-hash-table :test 'equal))

(defun reset-paciente (p)
  (setf (gethash p *paciente-sintomas*) '()))

(defun sintomas-de-paciente (p)
  (copy-list (gethash p *paciente-sintomas*)))

(defun agregar-sintoma (p s)
  (let ((lst (gethash p *paciente-sintomas*)))
    (unless (member s lst)
      (setf (gethash p *paciente-sintomas*) (cons s lst)))))

(defun sintomas-conocidos ()
  (remove-duplicates (apply #'append (mapcar #'cdr *sintomas-por-enfermedad*)) :test #'eq))

(defun contains-substring-p (haystack needle)
  (search needle haystack :test #'char-equal))

(defun normalize-input (str)
  (string-downcase (string-trim '(#\Space #\Tab #\Return #\Newline) str)))

(defun detectar-sintomas (input)
  (let ((lower (normalize-input input)))
    (remove-if-not (lambda (s)
                     (contains-substring-p lower (string s)))
                   (sintomas-conocidos))))

(defun sintoma-negado-p (input symptom)
  (let* ((lower (normalize-input input))
         (sstr (string symptom)))
    (and (or (contains-substring-p lower "no ")
             (contains-substring-p lower "no tengo ")
             (contains-substring-p lower "nunca "))
         (contains-substring-p lower sstr))))

(defun separar-sintomas (input)
  (let ((todos (detectar-sintomas input))
        (neg '())
        (af '()))
    (dolist (s todos)
      (if (sintoma-negado-p input s)
          (push s neg)
          (push s af)))
    (values (remove-duplicates af) (remove-duplicates neg))))

(defun sintomas-por-enfermedad (enf)
  (cdr (assoc enf *sintomas-por-enfermedad*)))

(defun tratamientos-por-enfermedad (enf)
  (cdr (assoc enf *tratamientos-por-enfermedad*)))

(defun recomendacion-por-enfermedad (enf)
  (or (cdr (assoc enf *recomendaciones-por-enfermedad*))
      "No tengo una recomendacion especifica para esta condicion, consulta a un medico."))

(defun enfermedades-que-tienen-sintoma (s)
  (remove-if-not (lambda (e)
                   (member s (sintomas-por-enfermedad e)))
                 *enfermedades*))

(defun diagnostico-basico (pac enf)
  (some (lambda (s) (and (member s (sintomas-de-paciente pac))
                         (member s (sintomas-por-enfermedad enf))))
        (sintomas-por-enfermedad enf)))

(defun diagnostico-exclusivo (pac enf)
  (some (lambda (s)
          (and (member s (sintomas-de-paciente pac))
               (= (length (enfermedades-que-tienen-sintoma s)) 1)))
        (sintomas-por-enfermedad enf)))

(defun probabilidad (pac enf)
  (let* ((total (length (sintomas-por-enfermedad enf)))
         (confirmados (intersection (sintomas-de-paciente pac)
                                    (sintomas-por-enfermedad enf)))
         (n (length confirmados)))
    (if (> total 0)
        (* (/ (float n) total) 100.0)
        0.0)))

(defun diagnostico-preventivo (pac enf)
  (let ((p (probabilidad pac enf)))
    (and (> p 0) (< p 100))))

(defun enfermedades-similares (enf)
  (remove-if-not
    (lambda (otra)
      (let ((shared (intersection (sintomas-por-enfermedad enf)
                                  (sintomas-por-enfermedad otra))))
        (and (not (eq enf otra))
             (>= (length shared) 2))))
    *enfermedades*))

(defun contradiccion-detectada-p (pac)
  (let ((sins (sintomas-de-paciente pac)))
    (some (lambda (par)
            (and (member (car par) sins)
                 (member (cdr par) sins)))
          *contradicciones*)))

(defun riesgo (pac enf)
  (let ((p (probabilidad pac enf)))
    (cond ((and (member enf *enfermedades-graves*) (>= p 60)) :alto)
          ((>= p 30) :medio)
          ((> p 0) :bajo)
          (t :ninguno))))

(defun tratamientos-combinados (pac)
  (remove-duplicates
    (mapcan (lambda (enf)
              (when (diagnostico-basico pac enf)
                (list (tratamientos-por-enfermedad enf))))
            *enfermedades*)
    :test #'string=))

(defmacro when-let ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var ,@body)))
;; ==========================================================
;; Reportes y analisis
;; ==========================================================

(defun listar-enfermedades-relacionadas (sintomas)
  (let ((enfs (remove-duplicates
               (mapcan (lambda (s) (enfermedades-que-tienen-sintoma s)) sintomas))))
    (dolist (e enfs) (format t "- ~a~%" e))))

(defun mostrar-probabilidades (pac)
  (format t "Probabilidad por enfermedad:~%")
  (dolist (e *enfermedades*)
    (let ((p (probabilidad pac e)))
      (when (> p 0)
        (format t "- ~a: ~,2f%%~%" e p)))))

(defun mostrar-riesgo (pac)
  (format t "Evaluacion de riesgo:~%")
  (let ((algo nil))
    (dolist (e *enfermedades*)
      (let ((r (riesgo pac e)))
        (when (not (eq r :ninguno))
          (setf algo t)
          (format t "- ~a: riesgo ~a~%" e r))))
    (unless algo (format t "No se detecta riesgo con la informacion actual.~%"))))

(defun reporte (pac)
  (format t "=== REPORTE DEL PACIENTE ===~%")
  (let ((sins (sintomas-de-paciente pac)))
    (format t "Sintomas confirmados: ~a~%" sins)
    (let* ((posibles (remove-duplicates
                      (mapcan (lambda (e) (when (diagnostico-basico pac e) (list e)))
                              *enfermedades*))))
      (format t "Posibles enfermedades: ~a~%" posibles)
      (mostrar-probabilidades pac)
      (format t "Buscando sintomas contradictorios...~%")
      (if (contradiccion-detectada-p pac)
          (format t "Alerta: sintomas contradictorios detectados.~%")
          (format t "No se detectan sintomas contradictorios.~%"))
      (mostrar-riesgo pac)
      (when-let ((dx (or (and (member :neumonia posibles)
                              (member 'dificultad_respirar sins)
                              :neumonia)
                         (and (member :meningitis posibles)
                              (member 'rigidez_cuello sins)
                              (member 'convulsiones sins)
                              :meningitis)
                         (and (member :hepatitis_b posibles)
                              (member 'ictericia sins)
                              (member 'orina_oscura sins)
                              :hepatitis_b))))
        (format t "Segun el arbol de decision, posible diagnostico: ~a~%" dx))
      (when posibles
        (let ((final (first posibles)))
          (format t "Diagnostico sugerido: ~a~%" final)
          (format t "Recomendacion: ~a~%" (recomendacion-por-enfermedad final))))))
  (terpri))

(defun mejor-enfermedad (pac)
  (let (best bestp)
    (dolist (e *enfermedades* best)
      (let ((p (probabilidad pac e)))
        (when (and (> p 0) (or (null bestp) (> p bestp)))
          (setf best e
                bestp p))))))

(defun hacer-preguntas-faltantes (pac enf)
  (let* ((faltantes (set-difference (sintomas-por-enfermedad enf)
                                    (sintomas-de-paciente pac)))
         (restantes (copy-list faltantes)))
    (dolist (s restantes)
      (format t "Presentas el siguiente sintoma: ~a ? (si/no): " s)
      (finish-output)
      (let ((resp (normalize-input (or (read-line *standard-input* nil "") ""))))
        (when (member resp '("si" "s"))
          (agregar-sintoma pac s))))))

(defun analisis-profundo ()
  (format t "--- ANALISIS PROFUNDO INTERACTIVO ---~%")
  (let ((sins (sintomas-de-paciente 'usuario)))
    (if (null sins)
        (format t "No tengo sintomas guardados. Primero dime algo como: tengo fiebre y tos.~%")
        (let ((enfs (remove-duplicates
                     (mapcan (lambda (s) (enfermedades-que-tienen-sintoma s)) sins))))
          (if (null enfs)
              (format t "No encontre enfermedades posibles con los sintomas dados.~%")
              (progn
                (dolist (e enfs)
                  (format t "Analizando posible enfermedad: ~a~%" e)
                  (hacer-preguntas-faltantes 'usuario e))
                (format t "Calculando diagnostico probable...~%")
                (let ((mejor (mejor-enfermedad 'usuario)))
                  (when mejor
                    (let ((p (probabilidad 'usuario mejor)))
                      (format t "Diagnostico final sugerido: ~a~%" mejor)
                      (let ((r (riesgo 'usuario mejor)))
                        (if (eq r :ninguno)
                            (format t "Riesgo bajo.~%")
                            (format t "Riesgo ~a.~%" r)))
                      (format t "Probabilidad estimada: ~,2f%%~%" p)
                      (format t "Recomendacion: ~a~%" (recomendacion-por-enfermedad mejor)))))))))))
;; ==========================================================
;; Consultas Disney y familia
;; ==========================================================

(defun buscar-personaje (input)
  (find-if (lambda (entry)
             (contains-substring-p input (string (car entry))))
           *personajes*))

(defun responder-personaje (input)
  (let ((entry (buscar-personaje input)))
    (when entry
      (let* ((nombre (car entry))
             (tipo (getf (cdr entry) :tipo))
             (pers (getf (cdr entry) :personalidad))
             (film (getf (cdr entry) :pelicula)))
        (cond ((contains-substring-p input "que es")
               (format t "~a es un ~a.~%" nombre tipo))
              ((contains-substring-p input "personalidad")
               (format t "La personalidad de ~a es ~a.~%" nombre pers))
              ((contains-substring-p input "pelicula")
               (format t "~a aparece en la pelicula ~a.~%" nombre film)))))))

(defun padre-de (h)
  (mapcar #'car (remove-if-not (lambda (p) (eq (cdr p) h)) *padre*)))

(defun madre-de (h)
  (mapcar #'car (remove-if-not (lambda (p) (eq (cdr p) h)) *madre*)))

(defun hermanos-de (p)
  (let ((pad (padre-de p))
        (mad (madre-de p)))
    (remove p
            (remove-duplicates
             (intersection
              (mapcan (lambda (pa) (mapcar #'cdr (remove-if-not (lambda (pair) (eq (car pair) pa)) *padre*))) pad)
              (mapcan (lambda (ma) (mapcar #'cdr (remove-if-not (lambda (pair) (eq (car pair) ma)) *madre*))) mad))))))

(defun abuelos-de (p)
  (remove-duplicates
    (append (mapcan #'padre-de (padre-de p))
            (mapcan #'padre-de (madre-de p))
            (mapcan #'madre-de (padre-de p))
            (mapcan #'madre-de (madre-de p)))))

(defun manejar-familia (input)
  (let* ((personas '(juan carlos pedro luis maria ana laura sofia))
         (encontrada (find-if (lambda (p) (contains-substring-p input (string p))) personas)))
    (when encontrada
      (cond ((contains-substring-p input "padre")
             (if (padre-de encontrada)
                 (format t "El padre de ~a es ~a.~%" encontrada (first (padre-de encontrada)))
                 (format t "No tengo registrado el padre de ~a.~%" encontrada)))
            ((contains-substring-p input "madre")
             (if (madre-de encontrada)
                 (format t "La madre de ~a es ~a.~%" encontrada (first (madre-de encontrada)))
                 (format t "No tengo registrada la madre de ~a.~%" encontrada)))
            ((contains-substring-p input "hermano")
             (let ((hs (hermanos-de encontrada)))
               (if hs
                   (format t "Los hermanos de ~a son: ~a.~%" encontrada hs)
                   (format t "~a no tiene hermanos registrados.~%" encontrada))))
            ((contains-substring-p input "abuelo")
             (let ((ab (abuelos-de encontrada)))
               (if ab
                   (format t "Los abuelos de ~a son: ~a.~%" encontrada ab)
                   (format t "No tengo informacion de los abuelos de ~a.~%" encontrada))))))))
;; ==========================================================
;; Motor de conversacion
;; ==========================================================

(defparameter *yes-words* '("si" "s" "claro" "ok" "haz un analisis" "analiza" "profundo" "dale"))
(defparameter *no-words* '("no" "nel" "para nada" "no gracias"))

(defun extraer-enfermedad (input)
  (cond ((contains-substring-p input "neumonia") :neumonia)
        ((contains-substring-p input "meningitis") :meningitis)
        ((contains-substring-p input "hepatitis") :hepatitis_b)
        (t nil)))

(defun procesar-tratamiento (input)
  (let ((e (extraer-enfermedad input)))
    (if e
        (format t "Tratamiento recomendado: ~a~%" (tratamientos-por-enfermedad e))
        (format t "No reconozco esa enfermedad.~%"))))

(defun procesar-sintomas (input)
  (let ((e (extraer-enfermedad input)))
    (if e
        (dolist (s (sintomas-por-enfermedad e)) (format t "- ~a~%" s))
        (format t "No reconozco esa enfermedad.~%"))))

(defun procesar-similares (input)
  (let ((e (extraer-enfermedad input)))
    (if e
        (let ((sim (enfermedades-similares e)))
          (if sim
              (dolist (x sim) (format t "~a ~%" x))
              (format t "No existen enfermedades similares a ~a.~%" e)))
        (format t "No reconozco esa enfermedad.~%"))))

(defun procesar-sintomas-descritos (input)
  (multiple-value-bind (af neg) (separar-sintomas input)
    (when af
      (reset-paciente 'usuario)
      (dolist (s af) (agregar-sintoma 'usuario s))
      (format t "Gracias por la informacion.~%")
      (format t "Sintomas afirmados: ~a~%" af)
      (when neg (format t "Sintomas negados: ~a~%" neg))
      (format t "Esto podria estar relacionado con:~%")
      (listar-enfermedades-relacionadas af)
      t)))

(defun procesar-riesgo ()
  (format t "Evaluando nivel de riesgo segun tus sintomas...~%")
  (mostrar-riesgo 'usuario))

(defun procesar-tratamiento-combinado ()
  (let ((trats (tratamientos-combinados 'usuario)))
    (if trats
        (progn (format t "Posibles tratamientos combinados:~%")
               (dolist (t trats) (format t "- ~a~%" t)))
        (format t "No tengo tratamientos combinados con la informacion actual.~%"))))

(defun procesar-input (input)
  (cond
    ((procesar-sintomas-descritos input) nil)
    ((member input *yes-words* :test #'string=) (analisis-profundo) nil)
    ((member input *no-words* :test #'string=)
     (format t "De acuerdo. Puedes seguir describiendo sintomas o preguntando.~%") nil)
    ((contains-substring-p input "sintoma") (procesar-sintomas input) nil)
    ((contains-substring-p input "tratamiento combinado") (procesar-tratamiento-combinado) nil)
    ((contains-substring-p input "tratamiento") (procesar-tratamiento input) nil)
    ((contains-substring-p input "similar") (procesar-similares input) nil)
    ((or (contains-substring-p input "hola") (contains-substring-p input "buenas"))
     (format t "Hola, como te sientes?~%") nil)
    ((contains-substring-p input " mal")
     (format t "Lamento que te sientas mal. Puedes decirme tus sintomas?~%") nil)
    ((contains-substring-p input "riesgo") (procesar-riesgo) nil)
    ((responder-personaje input) nil)
    ((manejar-familia input) nil)
    (t (format t "No entendi tu pregunta. Puedes reformularla?~%") nil)))

(defun conversacion ()
  (format t "> ")
  (finish-output)
  (let ((line (read-line *standard-input* nil :eof)))
    (cond ((or (null line) (eq line :eof)) (format t "Cuidate mucho.~%"))
          (t (let ((input (normalize-input line)))
               (if (string= input "adios")
                   (format t "Cuidate mucho.~%")
                   (progn (procesar-input input)
                          (conversacion))))))))

(defun eliza ()
  (format t "Bienvenido, soy ELIZA medico.~%")
  (format t "Describe tus sintomas o haz preguntas.~%")
  (reset-paciente 'usuario)
  (conversacion))

(defun start () (eliza))

(eval-when (:load-toplevel :execute)
  (let ((*package* (find-package :cl-user)))
    (use-package :eliza-medico)))
