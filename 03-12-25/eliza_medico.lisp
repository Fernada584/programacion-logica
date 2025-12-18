;; ==========================================================
;; ELIZA MEDICO - SISTEMA EXPERTO + CHATBOT AVANZADO
;; VERSION FINAL CORREGIDA - TRADUCIDA A LISP
;; ==========================================================

;; ==========================================================
;; BASE DE CONOCIMIENTO (ACTIVIDAD 1)
;; ==========================================================

;; Sintomas de enfermedades
(defparameter *base-conocimiento*
  '((neumonia tos) (neumonia fiebre) (neumonia dificultad-respirar) 
    (neumonia malestar) (neumonia dolor-cabeza)
    (meningitis fiebre) (meningitis dolor-cabeza) (meningitis rigidez-cuello)
    (meningitis somnolencia) (meningitis vomito) (meningitis convulsiones)
    (hepatitis-b ictericia) (hepatitis-b fatiga) (hepatitis-b dolor-abdomen)
    (hepatitis-b orina-oscura)))

;; Tratamientos
(defparameter *tratamientos*
  '((neumonia "Antibioticos, hidratacion y reposo.")
    (meningitis "Antibioticos intravenosos y hospitalizacion.")
    (hepatitis-b "Antivirales, reposo y evitar alcohol.")))

;; ==========================================================
;; SINTOMAS REGISTRADOS DEL PACIENTE
;; ==========================================================

(defparameter *sintomas-paciente* (list (cons 'usuario nil)))

(defun reset-paciente (p)
  (let ((entry (assoc p *sintomas-paciente*)))
    (if entry
        (setf (cdr entry) nil)
        (push (cons p nil) *sintomas-paciente*))))

(defun agregar-sintoma (p s)
  (let ((entry (assoc p *sintomas-paciente*)))
    (if entry
        (pushnew s (cdr entry) :test #'equal)
        (push (cons p (list s)) *sintomas-paciente*))))

;; ==========================================================
;; DETECCION DE SINTOMAS AFIRMADOS Y NEGADOS
;; ==========================================================

(defun contiene-sintoma (input s)
  (let ((ss (string-downcase (symbol-name s))))
    (search ss input :test #'char-equal)))

(defun sintoma-negado (input s)
  (let ((ss (string-downcase (symbol-name s))))
    (and (or (search "no " input :test #'char-equal)
             (search "no tengo " input :test #'char-equal)
             (search "nunca " input :test #'char-equal))
         (search ss input :test #'char-equal))))

(defun filtrar-sintomas (sintomas input negados afirmados)
  (cond
    ((null sintomas) (values (reverse negados) (reverse afirmados)))
    ((sintoma-negado input (car sintomas))
     (filtrar-sintomas (cdr sintomas) input (cons (car sintomas) negados) afirmados))
    (t
     (filtrar-sintomas (cdr sintomas) input negados (cons (car sintomas) afirmados)))))

(defun contiene-sintomas-filtrados (input)
  (let ((todos nil))
    ;; Buscar todos los sintomas mencionados
    (dolist (enf-sint *base-conocimiento*)
      (let ((sintoma (second enf-sint)))
        (when (contiene-sintoma input sintoma)
          (pushnew sintoma todos :test #'equal))))
    
    (multiple-value-bind (neg afirm)
        (filtrar-sintomas todos input nil nil)
      (values (remove-duplicates afirm :test #'equal)
              (remove-duplicates neg :test #'equal)))))

;; ==========================================================
;; DIAGNOSTICO BASICO
;; ==========================================================

(defun diagnostico-basico (p e)
  (let ((sintomas-pac (cdr (assoc p *sintomas-paciente*))))
    (dolist (sint *base-conocimiento*)
      (when (and (equal (first sint) e)
                 (member (second sint) sintomas-pac :test #'equal))
        (return t)))))

;; ==========================================================
;; ACTIVIDAD 2: DIAGNOSTICO POR SINTOMA EXCLUSIVO
;; ==========================================================

(defun diagnostico-exclusivo (p e)
  (let ((sintomas-pac (cdr (assoc p *sintomas-paciente*))))
    (dolist (sint *base-conocimiento*)
      (when (and (equal (first sint) e)
                 (member (second sint) sintomas-pac :test #'equal))
        ;; Verificar que no haya otra enfermedad con este sintoma
        (let ((exclusivo t))
          (dolist (otro-sint *base-conocimiento*)
            (when (and (not (equal (first otro-sint) e))
                       (equal (second otro-sint) (second sint)))
              (setf exclusivo nil)))
          (when exclusivo (return t)))))))

;; ==========================================================
;; ACTIVIDAD 3: PROBABILIDAD
;; ==========================================================

(defun probabilidad (p e)
  (let ((total 0)
        (coinciden 0)
        (sintomas-enfermedad nil))
    ;; Contar total de sintomas de la enfermedad
    (dolist (sint *base-conocimiento*)
      (when (equal (first sint) e)
        (incf total)
        (push (second sint) sintomas-enfermedad)))
    
    ;; Contar sintomas que coinciden
    (let ((sintomas-pac (cdr (assoc p *sintomas-paciente*))))
      (dolist (sint-paciente sintomas-pac)
        (when (member sint-paciente sintomas-enfermedad :test #'equal)
          (incf coinciden))))
    
    (if (> total 0)
        (* (/ coinciden total) 100)
        0)))

;; ==========================================================
;; ACTIVIDAD 4: DIAGNOSTICO PREVENTIVO
;; ==========================================================

(defun diagnostico-preventivo (p e)
  (let ((porc (probabilidad p e)))
    (and (> porc 0) (< porc 100))))

;; ==========================================================
;; ACTIVIDAD 5: ENFERMEDADES SIMILARES
;; ==========================================================

(defun enfermedades-similares (e1 e2)
  (and (not (equal e1 e2))
       (let ((sintomas-e1 nil)
             (sintomas-e2 nil))
         (dolist (sint *base-conocimiento*)
           (when (equal (first sint) e1)
             (push (second sint) sintomas-e1))
           (when (equal (first sint) e2)
             (push (second sint) sintomas-e2)))
         (> (length (intersection sintomas-e1 sintomas-e2 :test #'equal)) 1))))

(defun listar-enfermedades-similares (e)
  (let ((similares nil))
    (dolist (enf-sint *base-conocimiento*)
      (let ((otra (first enf-sint)))
        (when (and (not (equal otra e))
                   (enfermedades-similares e otra))
          (pushnew otra similares :test #'equal))))
    (if similares
        (format t "Enfermedades similares a ~a: ~a~%" e similares)
        (format t "No existen enfermedades similares a ~a~%" e))))

;; ==========================================================
;; ACTIVIDAD 6: SINTOMAS CONTRADICTORIOS
;; ==========================================================

(defparameter *contradictorios*
  '((fiebre ictericia)
    (convulsiones fatiga)
    (tos dolor-abdomen)
    (somnolencia dificultad-respirar)))

(defun sintomas-contradictorios (p)
  (let ((sintomas-pac (cdr (assoc p *sintomas-paciente*))))
    (dolist (cont *contradictorios*)
      (let ((s1 (first cont))
            (s2 (second cont)))
        (when (and (member s1 sintomas-pac :test #'equal)
                   (member s2 sintomas-pac :test #'equal))
          (format t "Alerta: sintomas contradictorios detectados: ~a y ~a~%" s1 s2)
          (return-from sintomas-contradictorios t)))))
  (format t "No se detectan sintomas contradictorios.~%")
  nil)

;; ==========================================================
;; ACTIVIDAD 7: ARBOL DE DECISION
;; ==========================================================

(defun arbol-diagnostico (p)
  (let ((sintomas (cdr (assoc p *sintomas-paciente*))))
    (cond
      ((and (member 'tos sintomas :test #'equal)
            (member 'dificultad-respirar sintomas :test #'equal))
       'neumonia)
      ((and (member 'rigidez-cuello sintomas :test #'equal)
            (member 'convulsiones sintomas :test #'equal))
       'meningitis)
      ((and (member 'ictericia sintomas :test #'equal)
            (member 'orina-oscura sintomas :test #'equal))
       'hepatitis-b)
      (t nil))))

;; ==========================================================
;; ACTIVIDAD 8: NIVEL DE RIESGO
;; ==========================================================

(defparameter *enfermedades-graves* '(meningitis hepatitis-b))

(defun grave (e)
  (member e *enfermedades-graves* :test #'equal))

(defun riesgo (p e)
  (let ((porc (probabilidad p e)))
    (cond
      ((and (grave e) (>= porc 60)) 'alto)
      ((and (not (grave e)) (>= porc 30)) 'medio)
      ((and (> porc 0) (< porc 30)) 'bajo)
      (t nil))))

;; ==========================================================
;; ACTIVIDAD 9: TRATAMIENTO COMBINADO
;; ==========================================================

(defun tratamiento-combinado (p)
  (let ((tratamientos nil))
    (dolist (enf-trat *tratamientos*)
      (let ((enfermedad (first enf-trat)))
        (when (diagnostico-basico p enfermedad)
          (pushnew (second enf-trat) tratamientos :test #'equal))))
    tratamientos))

;; ==========================================================
;; ACTIVIDAD 10: RECOMENDACIONES
;; ==========================================================

(defun recomendacion (p e)
  (declare (ignore p))
  (cond
    ((equal e 'neumonia) "Reposo, hidratacion, antibioticos si fueron recetados y supervision medica.")
    ((equal e 'meningitis) "Acudir de inmediato a urgencias. Es una condicion grave que requiere atencion hospitalaria.")
    ((equal e 'hepatitis-b) "Reposo, antivirales si fueron recetados, evitar alcohol y seguimiento medico.")
    (t "No tengo una recomendacion especifica para esta condicion, consulta a un medico.")))

;; ==========================================================
;; ACTIVIDAD 11: DIAGNOSTICAR Y TRATAR
;; ==========================================================

(defun diagnosticar-y-tratar (p)
  (let ((resultados nil))
    (dolist (enf-trat *tratamientos*)
      (let ((enfermedad (first enf-trat)))
        (when (diagnostico-basico p enfermedad)
          (push (list enfermedad (second enf-trat)) resultados))))
    resultados))

;; ==========================================================
;; ACTIVIDAD 12: REPORTE COMPLETO
;; ==========================================================

(defun reporte (p)
  (format t "=== REPORTE DEL PACIENTE ===~%")
  
  (let ((sintomas (cdr (assoc p *sintomas-paciente*))))
    (format t "Sintomas confirmados: ~a~%" sintomas)
    
    ;; Posibles enfermedades
    (let ((enfermedades-posibles nil))
      (dolist (enf-sint *base-conocimiento*)
        (let ((enfermedad (first enf-sint)))
          (when (and (member (second enf-sint) sintomas :test #'equal)
                     (not (member enfermedad enfermedades-posibles :test #'equal)))
            (push enfermedad enfermedades-posibles))))
      
      (format t "Posibles enfermedades: ~a~%" enfermedades-posibles)
      
      ;; Probabilidad por enfermedad
      (format t "Probabilidad por enfermedad:~%")
      (dolist (enf enfermedades-posibles)
        (let ((porc (probabilidad p enf)))
          (format t "- ~a: ~a%~%" enf porc)))
      
      ;; Sintomas contradictorios
      (format t "Buscando sintomas contradictorios...~%")
      (sintomas-contradictorios p)
      
      ;; Evaluacion de riesgo
      (format t "Evaluacion de riesgo:~%")
      (dolist (enf enfermedades-posibles)
        (let ((nivel-riesgo (riesgo p enf)))
          (when nivel-riesgo
            (format t "- ~a: ~a~%" enf nivel-riesgo))))
      
      ;; Arbol de decision
      (let ((dx (arbol-diagnostico p)))
        (when dx
          (format t "Segun el arbol de decision, posible diagnostico: ~a~%" dx)))
      
      ;; Diagnostico sugerido y recomendacion
      (if enfermedades-posibles
          (let ((enf-primera (first enfermedades-posibles)))
            (format t "Diagnostico sugerido: ~a~%" enf-primera)
            (format t "Recomendacion: ~a~%" (recomendacion p enf-primera)))
          (format t "No se encontro diagnostico definitivo.~%")))))

;; ==========================================================
;; PREDICCION DIRECTA
;; ==========================================================

(defun prediccion-directa (e)
  (let ((sintomas-pac (cdr (assoc 'usuario *sintomas-paciente*))))
    (dolist (sint *base-conocimiento*)
      (when (and (equal (first sint) e)
                 (member (second sint) sintomas-pac :test #'equal))
        (return t)))))

;; ==========================================================
;; MEJOR ENFERMEDAD POR PROBABILIDAD
;; ==========================================================

(defun mejor-enfermedad (p enfermedades)
  (if (null (cdr enfermedades))
      (car enfermedades)
      (let* ((e1 (car enfermedades))
             (e2 (cadr enfermedades))
             (p1 (probabilidad p e1))
             (p2 (probabilidad p e2)))
        (if (>= p1 p2)
            (mejor-enfermedad p (cons e1 (cddr enfermedades)))
            (mejor-enfermedad p (cons e2 (cddr enfermedades)))))))

;; ==========================================================
;; ANALISIS PROFUNDO INTERACTIVO
;; ==========================================================

(defun realizar-analisis-profundo ()
  (format t "--- ANALISIS PROFUNDO INTERACTIVO ---~%")
  (format t "Respondere unas preguntas para afinar el diagnostico.~%~%")
  
  (let ((sintomas-confirmados (cdr (assoc 'usuario *sintomas-paciente*))))
    (if (null sintomas-confirmados)
        (format t "No tengo sintomas guardados. Primero dime algo como: tengo fiebre y tos.~%")
        (let ((enfermedades-posibles (posibles-enfermedades sintomas-confirmados)))
          (if (null enfermedades-posibles)
              (format t "No encontre enfermedades posibles con los sintomas dados.~%")
              (progn
                (hacer-preguntas enfermedades-posibles)
                (concluir-diagnostico)))))))

(defun posibles-enfermedades (sintomas)
  (let ((enfermedades nil))
    (dolist (sint sintomas)
      (dolist (enf-sint *base-conocimiento*)
        (when (equal (second enf-sint) sint)
          (pushnew (first enf-sint) enfermedades :test #'equal))))
    enfermedades))

(defun hacer-preguntas (enfermedades)
  (dolist (e enfermedades)
    (format t "Analizando posible enfermedad: ~a~%" e)
    (let ((sintomas-enfermedad nil))
      (dolist (sint *base-conocimiento*)
        (when (equal (first sint) e)
          (push (second sint) sintomas-enfermedad)))
      (preguntar-sintomas-faltantes sintomas-enfermedad))))

(defun preguntar-sintomas-faltantes (sintomas)
  (dolist (s sintomas)
    (let ((sintomas-actuales (cdr (assoc 'usuario *sintomas-paciente*))))
      (unless (member s sintomas-actuales :test #'equal)
        (format t "Presentas el siguiente sintoma: ~a? (si/no): " s)
        (force-output)
        (let ((resp (string-downcase (read-line))))
          (when (search "si" resp :test #'char-equal)
            (agregar-sintoma 'usuario s)))))))



(defun concluir-diagnostico ()
  (format t "Calculando diagnostico probable...~%")
  (let ((todas-enfermedades nil))
    (dolist (enf-sint *base-conocimiento*)
      (pushnew (first enf-sint) todas-enfermedades :test #'equal))
    
    (let ((mejor (mejor-enfermedad 'usuario todas-enfermedades)))
      (format t "Diagnostico final sugerido: ~a~%" mejor)
      (let ((porc (probabilidad 'usuario mejor)))
        (format t "Nivel de riesgo detectado:~%")
        (let ((nivel-riesgo (riesgo 'usuario mejor)))
          (if nivel-riesgo
              (format t "- ~a: riesgo ~a~%" mejor nivel-riesgo)
              (format t "- Riesgo bajo.~%")))
        (format t "Probabilidad estimada: ~a%~%" porc)
        (format t "Recomendacion: ~a~%" (recomendacion 'usuario mejor))))))

;; ==========================================================
;; CHATBOT PRINCIPAL
;; ==========================================================

(defun inicio ()
  (format t "~%Bienvenido, soy ELIZA medico.~%")
  (format t "Describe tus sintomas o haz preguntas.~%")
  (conversacion))

(defun conversacion ()
  (format t "> ")
  (force-output)
  (let ((input-raw (read-line)))
    (let ((input (string-downcase input-raw)))
      (cond
        ((search "adios" input :test #'char-equal)
         (format t "Cuidate mucho.~%"))
        (t
         (procesar input)
         (conversacion))))))

;; ==========================================================
;; MOTOR DE RESPUESTAS DEL CHATBOT
;; ==========================================================

(defun listar-enfermedades-relacionadas (afirmados)
  (let ((enfermedades nil))
    (dolist (s afirmados)
      (dolist (enf-sint *base-conocimiento*)
        (when (equal (second enf-sint) s)
          (pushnew (first enf-sint) enfermedades :test #'equal))))
    (dolist (e enfermedades)
      (format t "- ~a~%" e))))

(defun extraer-enfermedad (input)
  (cond
    ((search "neumonia" input :test #'char-equal) 'neumonia)
    ((search "meningitis" input :test #'char-equal) 'meningitis)
    ((search "hepatitis" input :test #'char-equal) 'hepatitis-b)
    (t 'desconocida)))

(defun procesar (input)
  (cond
    ;; Usuario describe sintomas (CORREGIDO)
    ((multiple-value-bind (afirmados negados)
         (contiene-sintomas-filtrados input)
       (when afirmados
         (reset-paciente 'usuario)
         (dolist (s afirmados)
           (agregar-sintoma 'usuario s))
         (format t "Gracias por la informacion.~%")
         (format t "Sintomas afirmados: ~a~%" afirmados)
         (when negados
           (format t "Sintomas negados: ~a~%" negados))
         (format t "Esto podria estar relacionado con:~%")
         (listar-enfermedades-relacionadas afirmados)
         (format t "Deseas un analisis mas profundo?~%")
         t))
    
    ;; Usuario dice SI
    ((find input '("si" "claro" "ok" "haz un analisis" "analiza" "profundo" "dale") 
           :test #'string-equal)
     (realizar-analisis-profundo) t)
    
    ;; Usuario dice NO
    ((find input '("no" "nel" "para nada" "no gracias") :test #'string-equal)
     (format t "De acuerdo. Puedes seguir describiendo sintomas o preguntando.~%") t)
    
    ;; Pregunta sintomas de una enfermedad
    ((search "sintoma" input :test #'char-equal)
     (let ((e (extraer-enfermedad input)))
       (if (eq e 'desconocida)
           (format t "No reconozco esa enfermedad.~%")
           (progn
             (dolist (sint *base-conocimiento*)
               (when (equal (first sint) e)
                 (format t "- ~a~%" (second sint))))
             t))))
    
    ;; Pregunta tratamiento
    ((search "tratamiento" input :test #'char-equal)
     (let ((e (extraer-enfermedad input)))
       (let ((trat (second (assoc e *tratamientos* :test #'equal))))
         (if trat
             (format t "Tratamiento recomendado: ~a~%" trat)
             (format t "No tengo informacion de tratamiento para esa enfermedad.~%")))
       t))
    
    ;; Enfermedades similares
    ((search "similar" input :test #'char-equal)
     (let ((e (extraer-enfermedad input)))
       (listar-enfermedades-similares e)
       t))
    
    ;; Saludo
    ((or (search "hola" input :test #'char-equal)
         (search "buenas" input :test #'char-equal))
     (format t "Hola, como te sientes?~%") t)
    
    ;; Usuario dice "mal"
    ((search "mal" input :test #'char-equal)
     (format t "Lamento que te sientas mal. Puedes decirme tus sintomas?~%") t)
    
    ;; Pregunta directa sobre riesgo
    ((search "riesgo" input :test #'char-equal)
     (format t "Evaluando nivel de riesgo segun tus sintomas...~%")
     (let ((resultados nil))
       (dolist (enf-sint *base-conocimiento*)
         (let ((e (first enf-sint)))
           (let ((nivel (riesgo 'usuario e)))
             (when nivel
               (push (list e nivel) resultados)))))
       (if resultados
           (dolist (res resultados)
             (format t "- ~a: riesgo ~a~%" (first res) (second res)))
           (format t "No se detecta riesgo con la informacion actual.~%")))
     t)
    
    ;; Pregunta por tratamiento combinado
    ((search "tratamiento combinado" input :test #'char-equal)
     (let ((trats (tratamiento-combinado 'usuario)))
       (format t "Posibles tratamientos combinados:~%")
       (dolist (treat trats)
         (format t "- ~a~%" treat)))
     t)
    
    ;; BASE DE CONOCIMIENTO: PERSONAJES (DISNEY)
    ;; Pregunta que es un personaje
    ((search "que es" input :test #'char-equal)
     (let ((p (extraer-personaje input)))
       (unless (eq p 'desconocido)
         (let ((tipo-personaje (get-tipo-personaje p)))
           (format t "~a es un ~a~%" p tipo-personaje))))
     t)
    
    ;; Preguntar personalidad
    ((search "personalidad" input :test #'char-equal)
     (let ((p (extraer-personaje input)))
       (unless (eq p 'desconocido)
         (let ((pers (get-personalidad p)))
           (format t "La personalidad de ~a es ~a~%" p pers))))
     t)
    
    ;; Preguntar pelicula
    ((search "pelicula" input :test #'char-equal)
     (let ((p (extraer-personaje input)))
       (unless (eq p 'desconocido)
         (let ((film (get-pelicula p)))
           (format t "~a aparece en la pelicula ~a~%" p film))))
     t)
    
    ;; Consultas familiares
    ((search "padre" input :test #'char-equal)
     (let ((persona (extraer-persona input)))
       (when persona
         (let ((padre (get-padre persona)))
           (if padre
               (format t "El padre de ~a es ~a~%" persona padre)
               (format t "No tengo registrado el padre de ~a~%" persona)))))
     t)
    
    ((search "madre" input :test #'char-equal)
     (let ((persona (extraer-persona input)))
       (when persona
         (let ((madre (get-madre persona)))
           (if madre
               (format t "La madre de ~a es ~a~%" persona madre)
               (format t "No tengo registrada la madre de ~a~%" persona)))))
     t)
    
    ((search "hermano" input :test #'char-equal)
     (let ((persona (extraer-persona input)))
       (when persona
         (let ((hermanos (get-hermanos persona)))
           (if hermanos
               (format t "Los hermanos de ~a son: ~a~%" persona hermanos)
               (format t "~a no tiene hermanos registrados.~%" persona)))))
     t)
    
    ((search "abuelo" input :test #'char-equal)
     (let ((persona (extraer-persona input)))
       (when persona
         (let ((abuelos (get-abuelos persona)))
           (if abuelos
               (format t "Los abuelos de ~a son: ~a~%" persona abuelos)
               (format t "No tengo informacion de los abuelos de ~a~%" persona)))))
     t)
    
    ;; Default
    (t (format t "No entendi tu pregunta. Puedes reformularla?~%"))))

;; ==========================================================
;; BASE DE CONOCIMIENTO: PERSONAJES (DISNEY)
;; ==========================================================

(defparameter *tipos-personaje*
  '((mickey raton) (minnie raton) (donald pato) (goofy perro)
    (stich alien) (rapunzel humana) (ariel sirena) (woody juguete) (buzz juguete)))

(defparameter *personalidades*
  '((mickey optimista) (minnie carinosa) (donald enojon) (goofy torpe)
    (stich travieso) (rapunzel curiosa) (ariel feliz) (woody lider) (buzz valiente)))

(defparameter *peliculas*
  '((mickey fantasia) (donald clasicos-disney) (goofy clasicos-disney)
    (minnie clasicos-disney) (stich lilo-y-stitch) (rapunzel enredados)
    (ariel la-sirenita) (woody toy-story) (buzz toy-story)))

(defun get-tipo-personaje (p)
  (second (assoc p *tipos-personaje* :test #'equal)))

(defun get-personalidad (p)
  (second (assoc p *personalidades* :test #'equal)))

(defun get-pelicula (p)
  (second (assoc p *peliculas* :test #'equal)))

;; ==========================================================
;; Auxiliar para detectar personajes
;; ==========================================================

(defun extraer-personaje (input)
  (cond
    ((search "mickey" input :test #'char-equal) 'mickey)
    ((search "minnie" input :test #'char-equal) 'minnie)
    ((search "donald" input :test #'char-equal) 'donald)
    ((search "goofy" input :test #'char-equal) 'goofy)
    ((search "stich" input :test #'char-equal) 'stich)
    ((search "rapunzel" input :test #'char-equal) 'rapunzel)
    ((search "ariel" input :test #'char-equal) 'ariel)
    ((search "woody" input :test #'char-equal) 'woody)
    ((search "buzz" input :test #'char-equal) 'buzz)
    (t 'desconocido)))

;; ==========================================================
;; ARBOL GENEALOGICO
;; ==========================================================

(defparameter *hombres* '(juan carlos pedro luis))
(defparameter *mujeres* '(maria ana laura sofia))

(defparameter *padres* '((juan carlos) (juan ana) (carlos pedro) (luis sofia)))
(defparameter *madres* '((maria carlos) (maria ana) (laura pedro) (ana sofia)))

;; ==========================================================
;; REGLAS FAMILIARES
;; ==========================================================

(defun get-padre (persona)
  (let ((padre nil))
    (dolist (rel *padres*)
      (when (equal (second rel) persona)
        (setf padre (first rel))))
    padre))

(defun get-madre (persona)
  (let ((madre nil))
    (dolist (rel *madres*)
      (when (equal (second rel) persona)
        (setf madre (first rel))))
    madre))

(defun get-hermanos (persona)
  (let ((hermanos nil)
        (padre-p (get-padre persona))
        (madre-p (get-madre persona)))
    (when (and padre-p madre-p)
      (dolist (rel-p *padres*)
        (when (and (equal (first rel-p) padre-p)
                   (not (equal (second rel-p) persona)))
          (push (second rel-p) hermanos)))
      (dolist (rel-m *madres*)
        (when (and (equal (first rel-m) madre-p)
                   (not (equal (second rel-m) persona)))
          (push (second rel-m) hermanos))))
    (remove-duplicates hermanos :test #'equal)))

(defun get-abuelos (persona)
  (let ((abuelos nil)
        (padre-p (get-padre persona))
        (madre-p (get-madre persona)))
    (when padre-p
      (let ((abuelo-p (get-padre padre-p))
            (abuela-p (get-madre padre-p)))
        (when abuelo-p (push abuelo-p abuelos))
        (when abuela-p (push abuela-p abuelos))))
    (when madre-p
      (let ((abuelo-m (get-padre madre-p))
            (abuela-m (get-madre madre-p)))
        (when abuelo-m (push abuelo-m abuelos))
        (when abuela-m (push abuela-m abuelos))))
    abuelos))

;; ==========================================================
;; Auxiliar para extraer personas del arbol genealogico
;; ==========================================================

(defun extraer-persona (input)
  (dolist (persona (append *hombres* *mujeres*))
    (let ((nombre (string-downcase (symbol-name persona))))
      (when (search nombre input :test #'char-equal)
        (return-from extraer-persona persona))))
  nil)

;; ==========================================================
;; INICIALIZAR SISTEMA
;; ==========================================================

(defun inicializar-sistema ()
  (setf *sintomas-paciente* (list (cons 'usuario nil)))
  (format t "Sistema ELIZA MEDICO inicializado.~%")
  (inicio))

;; Para iniciar el sistema, ejecutar: (inicializar-sistema)