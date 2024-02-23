;;;;;;;;;;;;Funcion COND;;;;;;;;;;;;
(defun condtest(a b)
    (cond 
        ((< a b)(print " a es menor que b"))
        ((> a b)(print " a es mayor que b"))
    )
)

;;;;;;;;;;;;Funcion CASE;;;;;;;;;;;;
(defun casetest(a)
    (case a
        ((1) (format t "Suma~%")
            (princ "Dame un numero:")
            (setq a (read))
            (princ "Dame otro numero:")
            (setq b (read))
            (format t "Resultado:~A" (suma a b))
        )

        ((2) (format t "Resta~%")
            (princ "Dame un numero:")
            (setq a (read))
            (princ "Dame otro numero:")
            (setq b (read))
            (format t "Resultado:~A" (resta a b))
        )
    )
)

(defun suma(a b)
    (+ a b)
)

(defun resta(a b)
    (- a b)
)

;;;;;;;;;;;;FUNCION OR;;;;;;;;;;;;
(defun ortest (a b)
    (or (> a b) (< a b)
        (princ "Entro a la funcion or")
    )
)