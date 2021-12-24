(indention/define-for-major-mode
  js js
  :one-indent "  "
  :rules
  (list
   (indention/make-rule
    :indent-func 'indention/increment-indent-level
    :on-chars "{"
    :check-on-prev-line)
   (indention/make-rule
    :indent-func 'indention/decrement-indent-level
    :on-chars "}")))



