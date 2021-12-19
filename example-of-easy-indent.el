(indention/define-for-major-mode porth
                                 porth
                                 :rules
                                 (list
                                  (indention/make-rule
                                   (lambda () (insert "   "))
                                   (lambda () (even-p (line-number-at-pos)))
                                   )
                                  ))

(defun even-p (n)
    "Is even `N`?."
    (eq (% n 2) 0)
    )


