(message "%s" (macroexpand
               '(indention/define-for-major-mode
                 joker joker
                 :one-indent "  "
                 :rules
                 (list
                  (indention/make-rule
                   :indent-func 'indention/increment-indent-level
                   :predicate (lambda () (s-contains-p "{" (thing-at-point 'line t)))
                   :check-on-prev-line)
                  (indention/make-rule
                   :indent-func 'indention/decrement-indent-level
                   :predicate (lambda () (s-contains-p "}" (thing-at-point 'line t)))
                   :check-on-prev-line)))))

