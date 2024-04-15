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
                    (format t "Tienda Dickies~%Cantidad pantalones: ~A~%Descuento: ~A~%Subtotal: ~A~%Total: ~A"
                        cantPantalones
                        (* 0.15 cantPantalones *precioPantalon*)
                        (* cantPantalones *precioPantalon*)
                        (- (* cantPantalones *precioPantalon*) (* 0.15 cantPantalones *precioPantalon*))
                    )
                ) 
            )
        )
        ((>= cantPantalones 12)
            (format t "Tienda Dickies~%Cantidad pantalones: ~A~%Descuento: ~A~%Subtotal: ~A~%Total: ~A"
                cantPantalones
                (* 0.3 cantPantalones *precioPantalon*)
                (* cantPantalones *precioPantalon*)
                (- (* cantPantalones *precioPantalon*) (* 0.3 cantPantalones *precioPantalon*))
            )    
        )
    )
)

(defun Problema2()
    (format t "Banco la Pelona~%Solicitud de prestamo~%Responda las siguientes preguntas~A")
    (format t "Nombre: ")
    (setq nombre (read))
    (format t "Historial crediticio b-buneo m-malo: ")
    (setq hCrediticio (read))
    (format t "Cantidad del prestamo: ")
    (setq cantidadPrestamo (read))
    (format t "Salario anual persivido: ")
    (setq salario (read))
    (format t "Valor de otras propiedades: ")
    (setq valorPropiedades (read))
    (if (eq hCrediticio b)
        (puntos)
        (format t "Lo sentimos no podemos proceder con el prestamo")
    )
)

(defun puntos()
    
)