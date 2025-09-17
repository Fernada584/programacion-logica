(defun recorre (lista)
  (when lista
    (let ((numero (car lista)))
      (when (listp numero)
        (print numero))
      (recorre (cdr lista)))))


(defun suma (a b)
  (+ a b))

(defun areacuadro (a b)
  (* a b))

(defun factorial(x)
    (if (= x 0)
    1
    (* x (factorial (- x 1))))
)

(defun fibo (x)
  (if (< x 2)
        1
        (+ (fibo (x - x 1)) (fibo (- x 2)))
  )
)
