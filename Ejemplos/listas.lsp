(defun ejercicio1lista()
    (setq a '((cadddr(cddddr '(a b c d e f g h))))) ;Primer ejercicio

    (setq b '((caddr(cddddr '(a b c d e f g h)))))  ;Segundo ejercicio

    (setq c '((cadr(cddddr '(a b c d e f g h)))))   ;Tercer ejercicio

    (setq d '((caddr '(a b c d e f g h))))          ;Cuarto ejercicio

    (format t "Ejercicio 1: ~A~%Ejercicio 2: ~A~%Ejercicio 3: ~A~%Ejercicio 4: ~A~%" a b c d)
)