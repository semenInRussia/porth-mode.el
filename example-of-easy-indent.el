
(indention/define-for-major-mode java java
                                 :one-indent "    "
                                 :rules
                                 (list
                                  (indention/make-rule
                                   :on-chars "{"
                                   :add-indent
                                   :check-on-prev-line
                                   :if-true-check-next-rules)
                                  (indention/make-rule
                                   :on-chars "}"
                                   :deindent
                                   :if-true-check-next-rules)))

