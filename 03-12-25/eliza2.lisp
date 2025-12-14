;;; ==========================================================
;;; SISTEMA EXPERTO MÉDICO - Versión Common Lisp
;;; ==========================================================

(defparameter *sintomas-paciente* (make-hash-table))

(defun reset-paciente (p)
  (setf (gethash p *sintomas-paciente*) nil))

(defun registrar-sintoma (pac s)
  (pushnew s (gethash pac *sintomas-paciente*)))

(defun tiene-sintoma-paciente? (pac s)
  (member s (gethash pac *sintomas-paciente*) :test #'equal))

(defun preguntar (pac s)
  (if (tiene-sintoma-paciente? pac s)
      t
      (progn
        (format t "~%¿El paciente ~a tiene ~a? (si/no): " pac s)
        (let ((resp (read)))
          (if (equal resp 'si)
              (progn (registrar-sintoma pac s) t)
              nil)))))

;;; ==========================================================
;;; BASE DE CONOCIMIENTO: SÍNTOMAS POR ENFERMEDAD
;;; ==========================================================

(defparameter *enfermedades*
  '((gripe fiebre dolor_cabeza congestion)
    (alergia estornudos picazon_ojos congestion)
    (migrana dolor_cabeza_severo sensibilidad_luz nauseas)
    (resfriado estornudos congestion dolor_garganta)
    ;; nuevas (actividad 1)
    (covid fiebre tos_seca dificultad_respirar)
    (sinusitis dolor_cabeza congestion dolor_cara)))

(defun sintomas-de (enfermedad)
  (second (assoc enfermedad *enfermedades*)))

;;; ==========================================================
;;; TRATAMIENTOS
;;; ==========================================================

(defparameter *tratamientos*
  '((gripe "Reposo, hidratación, paracetamol y aislamiento.")
    (alergia "Antihistamínicos y evitar el alérgeno conocido.")
    (migrana "Medicacion específica y ambiente oscuro.")
    (resfriado "Líquidos calientes y descongestionantes.")
    (covid "Aislamiento y antipiréticos.")
    (sinusitis "Descongestionantes y lavado nasal.")))

(defun tratamiento (e)
  (second (assoc e *tratamientos*)))

;;; ==========================================================
;;; DIAGNÓSTICO BÁSICO
;;; ==========================================================

(defun diagnostico-basico (pac e)
  (some (lambda (s) (preguntar pac s)) (sintomas-de e)))

;;; ==========================================================
;;; DIAGNÓSTICO COMPLETO
;;; ==========================================================

(defun diagnostico-completo (pac e)
  (every (lambda (s) (preguntar pac s)) (sintomas-de e)))

;;; ==========================================================
;;; DISTINCIÓN FUERTE
;;; ==========================================================

(defun distincion-fuerte (pac e)
  (cond
    ((eq e 'gripe)
     (and (preguntar pac 'fiebre)
          (preguntar pac 'dolor_cabeza)
          (not (preguntar pac 'estornudos))))
    ((eq e 'resfriado)
     (and (preguntar pac 'estornudos)
          (preguntar pac 'dolor_garganta)
          (not (preguntar pac 'fiebre))))
    (t nil)))

(defun tratamiento-de-paciente (pac)
  (loop for (e . t) in *tratamientos*
        when (or (distincion-fuerte pac e)
                 (diagnostico-completo pac e)
                 (diagnostico-basico pac e))
          return (tratamiento e)))

;;; ==========================================================
;;; SEVERIDAD
;;; ==========================================================

(defun contar-sintomas-confirmados (pac e)
  (count-if (lambda (s) (tiene-sintoma-paciente? pac s))
            (sintomas-de e)))

(defun severidad (pac e)
  (let ((c (contar-sintomas-confirmados pac e)))
    (cond ((>= c 3) 'severa)
          ((= c 2) 'moderada)
          ((= c 1) 'leve)
          (t 'ninguna))))

;;; ==========================================================
;;; ACTIVIDAD 2: Diagnóstico por síntoma exclusivo
;;; ==========================================================

(defun sintoma-exclusivo? (s enf)
  (not (exists (lambda (e) (and (not (eq e enf))
                                (member s (sintomas-de e))))
               (mapcar #'car *enfermedades*))))

(defun diagnostico-exclusivo (pac e)
  (exists (lambda (s)
            (and (sintoma-exclusivo? s e)
                 (preguntar pac s)))
          (sintomas-de e)))

;;; ==========================================================
;;; ACTIVIDAD 3: Probabilidad
;;; ==========================================================

(defun probabilidad (pac e)
  (let* ((lista (sintomas-de e))
         (total (length lista))
         (confirmados
          (count-if (lambda (s)
                      (tiene-sintoma-paciente? pac s))
                    lista)))
    (* 100 (/ confirmados total))))

;;; ==========================================================
;;; ACTIVIDAD 4: Diagnóstico preventivo
;;; ==========================================================

(defun diagnostico-preventivo (pac e)
  (let* ((c (contar-sintomas-confirmados pac e))
         (total (length (sintomas-de e))))
    (and (>= c 1) (< c total))))

;;; ==========================================================
;;; ACTIVIDAD 5: Enfermedades similares
;;; ==========================================================

(defun enfermedades-similares (e1 e2)
  (let ((inter (intersection (sintomas-de e1)
                             (sintomas-de e2)
                             :test #'equal)))
    (>= (length inter) 2)))

;;; ==========================================================
;;; ACTIVIDAD 6: Síntomas contradictorios
;;; ==========================================================

(defparameter *contradicciones*
  '((fiebre picazon_ojos)
    (nauseas estornudos)
    (dificultad_respirar dolor_garganta)))

(defun contradiccion? (s1 s2)
  (or (equal (list s1 s2) (assoc s1 *contradicciones*))
      (equal (list s2 s1) (assoc s2 *contradicciones*))))

(defun sintomas-contradictorios (pac)
  (let ((lst (gethash pac *sintomas-paciente*)))
    (exists (lambda (a)
              (exists (lambda (b)
                        (and (not (equal a b))
                             (contradiccion? a b)))
                      lst))
            lst)))

;;; ==========================================================
;;; ACTIVIDAD 7: Árbol de diagnóstico
;;; ==========================================================

(defun arbol-diagnostico (pac)
  (cond
    ((and (preguntar pac 'fiebre)
          (preguntar pac 'dolor_cabeza)
          (preguntar pac 'congestion))
     'gripe)

    ((and (preguntar pac 'estornudos)
          (preguntar pac 'dolor_garganta)
          (not (preguntar pac 'fiebre)))
     'resfriado)

    ((and (preguntar pac 'estornudos)
          (preguntar pac 'picazon_ojos))
     'alergia)

    ((and (preguntar pac 'dolor_cabeza_severo)
          (preguntar pac 'sensibilidad_luz))
     'migrana)

    ((and (preguntar pac 'fiebre)
          (preguntar pac 'tos_seca)
          (preguntar pac 'dificultad_respirar))
     'covid)

    ((and (preguntar pac 'dolor_cabeza)
          (preguntar pac 'dolor_cara)
          (preguntar pac 'congestion))
     'sinusitis)))

;;; ==========================================================
;;; ACTIVIDAD 8: Nivel de riesgo
;;; ==========================================================

(defparameter *graves* '(covid migrana))

(defun riesgo (pac e)
  (cond
    ((and (member e *graves*)
          (equal (severidad pac e) 'severa)) 'alto)
    ((equal (severidad pac e) 'moderada) 'medio)
    ((equal (severidad pac e) 'leve) 'bajo)
    (t 'desconocido)))

;;; ==========================================================
;;; ACTIVIDAD 9: Tratamiento combinado
;;; ==========================================================

(defun tratamiento-combinado (pac)
  (remove-duplicates
   (loop for (e _) in *tratamientos*
         when (diagnostico-basico pac e)
           collect (tratamiento e))
   :test #'equal))

;;; ==========================================================
;;; ACTIVIDAD 10: Recomendación
;;; ==========================================================

(defun recomendacion (pac e)
  (case (severidad pac e)
    ('severa "Urgencias inmediatas, no automedicarse.")
    ('moderada "Acudir pronto a consulta médica.")
    ('leve "Reposo e hidratación.")
    (t "No hay suficiente información.")))

;;; ==========================================================
;;; ACTIVIDAD 11: Diagnosticar y tratar
;;; ==========================================================

(defun diagnosticar-y-tratar (pac)
  (dolist (e (mapcar #'car *enfermedades*))
    (when (or (distincion-fuerte pac e)
              (diagnostico-completo pac e)
              (diagnostico-basico pac e))
      (return (list e (tratamiento e))))))

;;; ==========================================================
;;; ACTIVIDAD 12: Reporte completo
;;; ==========================================================

(defun reporte (pac)
  (format t "~%=========== REPORTE DEL PACIENTE ~a ===========" pac)
  (let* ((sintomas (gethash pac *sintomas-paciente*))
         (posibles
           (loop for (e . _) in *enfermedades*
                 when (some (lambda (s) (member s sintomas :test #'equal))
                            (sintomas-de e))
                   collect e))
         (diagnostico
           (car (sort posibles #'> :key (lambda (e) (probabilidad pac e))))))

    (format t "~%Sintomas confirmados: ~a" sintomas)
    (format t "~%Enfermedades posibles: ~a" posibles)
    (format t "~%Probabilidades:")
    (dolist (e posibles)
      (format t "~%  - ~a: ~a %"
              e (probabilidad pac e)))

    (format t "~%Diagnóstico final: ~a" diagnostico)
    (format t "~%Severidad: ~a" (severidad pac diagnostico))
    (format t "~%Tratamiento: ~a" (tratamiento diagnostico))
    (format t "~%Recomendación: ~a" (recomendacion pac diagnostico))
    (format t "~%Riesgo: ~a" (riesgo pac diagnostico))

    (format t "~%==========================================~%")))

