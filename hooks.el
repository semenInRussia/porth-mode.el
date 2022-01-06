;;; hooks --- Operations on emacs hooks. -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1
;; Packages-Requires: ((dash "2.18.0")
;;                     (s    "1.12.0"))
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'dash)

(defmacro hooks/union-to (destination-hook &rest hooks)
    "Add hook to run DESTINATION-HOOK to HOOKS."
    `(--each (list ,@hooks)
        (add-hook it (lambda () (run-hooks ,destination-hook))))
    )


(defmacro hooks/union-from-namespace-to (namespace
                                         dest-hook
                                         &rest hooks)
    "Add hook to run DEST-HOOK to HOOKS from NAMESPACE.
Example:
    (hooks/union-from-namespace-to calc
                                   embeded-hook
                                   embeded-mode-hook
                                   embedded-new-buffer-hook)
    --→ (hooks/union-to 'calc-embedded-hook
                        'calc-embeded-mode-hook
                        'calc-embedded-new-buffer-hook)"
    (setq dest-hook (from-namespace-for-symbols namespace dest-hook))
    (->> hooks
         (--map (from-namespace-for-symbols namespace it))
         (-map (lambda (el) `(quote ,el)))
         (setq hooks))
    `(hooks/union-to ',dest-hook ,@hooks)
    )


(defmacro hooks/add-to-hook-from-namespace (namespace hook function)
    "Add to the value of HOOK from NAMESPACE the function FUNCTION.
FUNCTION is not added if already present."
    (setq hook (from-namespace-for-symbols namespace hook))
    `(add-hook ',hook ,function)
    )


(defmacro hooks/add-to-hook-from-namespace (namespace hook function)
    "Add to the value of HOOK from NAMESPACE the function FUNCTION.
FUNCTION is not added if already present."
    (setq hook (from-namespace-for-symbols namespace hook))
    `(add-hook ',hook ,function)
    )


(defmacro hooks/from-namespace-add-hook (namespace hook function)
    "Add to the value of HOOK from NAMESPACE the function FUNCTION.
FUNCTION is not added if already present."
    (setq hook (from-namespace-for-symbols namespace hook))
    (setq function (from-namespace-for-symbols namespace function))
    `(add-hook ',hook ',function)
    )


(defmacro hooks/from-namespace-remove-hook (namespace hook function)
    "Remove to the value of HOOK from NAMESPACE the function FUNCTION.
FUNCTION is not removed if already present."
    (setq hook (from-namespace-for-symbols namespace hook))
    (setq function (from-namespace-for-symbols namespace function))
    `(remove-hook ',hook ',function)
    )


(defmacro hooks/remove-from-hook-from-namespace (namespace hook function)
    "Add to the value of HOOK from NAMESPACE the function FUNCTION.
FUNCTION is not added if already present."
    (setq hook (from-namespace-for-symbols namespace hook))
    `(remove-hook ',hook ,function)
    )


(defmacro hooks/run-from-namespace (namespace &rest hooks)
    "Run all HOOKS from NAMESPACE."
    (->> hooks
         (--map (from-namespace-for-symbols namespace it))
         (-map (lambda (el) `(quote ,el)))
         (setq hooks))

    `(run-hooks ,@hooks)
    )

(defmacro hooks/from-namespace (namespace)
    "Return hooks of NAMESPACE.
For example:
python => python-hook;
clang => clang-hook;"
    `(from-namespace ,namespace hook)
    )


(provide 'hooks)
;;; hooks.el ends here
