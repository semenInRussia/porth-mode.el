Add a lot of feature, and fix some bugs

- **REFACTOR** Rename in some places `indention` to `ind`
- **REFACTOR** Extract logic to folowed files:
  * indention-simple.el (simple functions for Emacs)
  * indention-make-rule.el (`ind/make-rule` function)

- **REFACTOR** Make agile handle of flags in `ind/make-rule`

- **REFACTOR** Rename `increment-indent-level` to `increment-real-indent-level`
- **FEATURE** Add function: `change-real-indent-level`
- **SECRET** `smart-mode` arg
