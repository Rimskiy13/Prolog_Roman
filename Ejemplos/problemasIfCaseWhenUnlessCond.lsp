(defun Problema1()
    (defvar *precioPantalon* 100)
    (format t "Bienvenido a Dickies~%Ingrese la cantidad de pantalones a comprar: ")
    (setq cantPantalones (read))
    (cond
        ((< cantPantalones 5)
            (format t "Tienda Dickies~%Cantidad pantalones: ~A~%Total: ~A" 
                cantPantalones 
                (* cantPantalones *precioPantalon*)
            )
        )
        (   
            (if (>= cantPantalones 5)
                (if (< cantPantalones 12)
                    (format t "Tienda Dickies~%Cantidad pantalones: ~A~%Descuento: ~A~%Total: ~A"
                        cantPantalones
                        (* 0.15 cantPantalones *precioPantalon*)
                        (- (* cantPantalones *precioPantalon*) (* 0.15 cantPantalones *precioPantalon*))
                    )
                ) 
            )
        )
        (
            
        )
    )
)