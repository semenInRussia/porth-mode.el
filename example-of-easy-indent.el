
(indention/define-for-major-mode java java
                                 :one-indent "    "
                                 :rules
                                 (list
                                  (ind/make-rule
                                   :on-chars "{"
                                   :add-indent
                                   :check-on-prev-line
                                   )
                                  (ind/make-rule
                                   :on-chars "}"
                                   :deindent
                                   )))

