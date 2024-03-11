(defparameter *nodes*
    '( (Guia(
            (Especialidad (Te muestra los crafteos posibles con un material determinado))
            (Requisitos (Tener una vivienda disponible en el mundo))
            (Arma (Arco y flecha))
            (Gusta (
                (Bioma(Bosque))
                (NPC(Zoologa Buhonero))
            ))
            (Odia (Oceano))
            (Ama (Nadie))
            (Disgusta (NPC(Steampunker)))
            (Vecinos (NPC(Mercader Zoologa)))
        ))
        (Mercader(
            (Especialidad(Vende objetos básicos	))
            (Requisitos(Conseguir 50 monedas de plata))
            (Arma (Cuchillo arrojadizo))
            (Gusta (
                (Bioma(Bosque))
                (NPC(Golfista Enfermera))
            ))
            (Odia (Desierto))
            (Ama (Nadie))
            (Disgusta (NPC(Recaudador de impuestos)))
            (Vecinos(NPC(Zoologa Guia)))
        ))
    )
)

(defun recorre()
    `(Al ,(car *nodes*) )
)