(defun factorial(a)
    (if (= a 0)
        1
        (* a (factorial (- a 1)))
    )
)