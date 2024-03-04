(defun funcion1()  
    `(Hola mundo ,(suma 6 7))
)

(defun suma(a b)
    (+ a b)
)

(defun lista()
    (mapcar #'cadr '((foo bar) (baz qux)))
)