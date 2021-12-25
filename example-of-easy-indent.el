
(indention/define-for-major-mode
  js js
  :one-indent "    "
  :rules
  (list
   (indention/make-rule
    :on-chars "{"
    :add-indent
    :check-on-prev-line)
   (indention/make-rule
    :on-chars "}"
    :deindent)))

