;;; functools --- Functions from functional programmming -*- lexical-binding: t; -*-

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

(require 'cl)

(defun functools/compose (&rest funs)
    "Return function composed of FUNS."
    (lexical-let ((lex-funs funs))
                 (lambda (&rest args)
                     (reduce 'funcall (butlast lex-funs)
                             :from-end t
                             :initial-value
                             (apply (car (last lex-funs)) args)))))

(provide 'functools)
;;; functools.el ends here
