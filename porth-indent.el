;;; porth-indent --- Defnition of auto-indent code for Porth -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

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

(require 'easy-define-indent)

(defcustom porth-keywords-add-indent
  '("const" "proc" "memory" "if" "assert" "while" "do" "in" "else")
  "Keywords of Porth, which add indent to next line."
  :group 'porth
  :type '(repeat string))


(defcustom porth-keywords-deindent '("end" "else")
  "Keywords of Porth, which deindent next line."
  :group 'porth
  :type '(repeat string))


(defcustom porth-keywords-change-indent
  (-concat porth-keywords-add-indent porth-keywords-deindent)
  "All keywords of `porth-mode', which chage indent.")

(indention/define-for-major-mode
 porth
 porth
 :one-indent "  "
 :rules
 (list
  (indention/make-rule :add-indent
                       :check-on-prev-line
                       :predicate
                       
                       (indention/make-rule
                        :on-keywords porth-keywords-add-indent
                        :add-indent
                        :check-on-prev-line
                        :if-true-check-next-rules)
                       (indention/make-rule
                        :on-keywords porth-keywords-deindent
                        :deindent
                        ))
  )

 (provide 'porth-indent)
;;; porth-indent.el ends here
