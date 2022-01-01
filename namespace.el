;;; namespace --- Operations on namespaces. -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1
;; Packages-Requires: ((dash "2.18.0")
;;                     (s     "1.12.0"))

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


(defmacro defun-in-namespace (namespace func-name
                              arglist
                              docstring
                              &rest body)
    "Define function in `NAMESPACE` with name `FUNC-NAME`.
With respective `ARGLIST`, `DOCSTRING` and `BODY`."
    `(defun ,(from-namespace-for-symbols namespace func-name) ,arglist
         ,docstring
         ,@body)
    )


(defmacro defcustom-from-namespace (namespace
                                    var-name
                                    standard
                                    docstring &rest args)
    "Define custom variable from `NAMESPACE`.
`VAR-NAME` is name of variable.  `STANDARD` is default value of variable.
`DOCSTRING` is documentation's string for variable.  `ARGS` is additional
arguments for `defcustom`."
    `(defcustom ,(from-namespace-for-symbols namespace var-name)
       ,standard
       ,docstring
       ,@args
       )
    )


(defmacro from-namespace-funcall (namespace func-name &rest args)
    "Call function with FUNC-NAME from NAMESPACE with ARGS."
    `(,(from-namespace-for-symbols namespace func-name) ,@args)
    )


(defmacro from-namespace-var (namespace var-name)
    "Get something symbol with `VAR-NAME` from `NAMESPACE`."
    `(eval ,(from-namespace-for-symbols namespace var-name))
    )


(defmacro from-namespace-setq (namespace var-name value)
    "Setq var with VAR-NAME from NAMESPACE to VALUE."
    `(set (from-namespace ,namespace ,var-name) ,value)
    )


(defmacro from-namespace (namespace something-name)
    "Get something symbol with `SOMETHING-NAME` from `NAMESPACE`."
    (message "nams is %s" (symbol-name namespace))
    `(intern ,(s-concat (symbol-name namespace)
                        "-"
                        (symbol-name something-name)))
    )


(defun from-namespace-for-symbols (namespace-symb something-name-symb)
    "Get symbol with SOMETHING-NAME-SYMB from NAMESPACE-SYMB."
    (intern (s-concat (symbol-name namespace-symb)
                      "-"
                      (symbol-name something-name-symb)))
    )

(provide 'namespace)
;;; namespace.el ends here
