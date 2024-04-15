;;; Ejercicio 1 ;;;
(defun suma(limite)
	(if (= limite 0)
		0
		(+ limite (suma(- limite 1)))   
	)
)

;;; Ejercicio 4 ;;;
(defun Devuelve3(list)
	(caddr '(list))
)

;;; Ejercicio 5 ;;;
(defun DevuelveCabezaCola(list)
	(setq a '(car '(list)))
	(setq b '(cdr '(list)))
	(append cabeza cola)
)

;;; Ejercicio 3 ;;;
(defun E3()
	(setq x1 '(COCHE MOTO TREN))
	(setq x2 '(EDUARDO PEDRO ANTONIO))
	(append x1 x2)
)