(defun my-length (list)
    (if list
        (1+ (my-length (cdr list)))
        0
    )
)

(defun recorre1(list)
    (format " ~A ->" (car list))
    (if list 
        (recorre1 (cdr list))
    )
)

(defun recorre2(list)
    (print (car list))
    (if list
        (recorre2 (cdr list))
    )
)