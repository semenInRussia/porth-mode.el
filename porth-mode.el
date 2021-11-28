;;; porth-mode.el --- A Porth editing mode

;; Copyright © 2021
;;             Free Software Foundation, Inc

;; Author: semenInRussia <hrams205@gmail.com>
;; Keywords: faces files Porth
;; Packages-Requires: ((dash "2.18.0")
;;                     (s     "1.12.0"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A major mode for editing Porth (the stack orintied programming
;; language, see URL `https://gitlab.com/tsoding/porth/') in Emacs.
;;
;; Some of its major features include:
;;
;;  - syntax highlighting (font lock),
;;  - on-the-fly documentation,
;;


;;; Code:

(defgroup porth nil
    "Mode of editing in Emacs!."
    :group 'tools
    )


(defconst porth-version "0.0.1"
  "The release version of `porth-mode'.")


(defconst porth-keywords
  '("if"
    "else"
    "while"
    "do"
    "include"
    "memory"
    "proc"
    "const"
    "end"
    "offset"
    "reset"
    "assert"
    "in")
  "Keywords of Porth.")


(defcustom porth-keyword-regexp
  (regexp-opt porth-keywords 'symbols)
  "Regexp for keyword in Porth."
  :group 'porth-mode
  :type 'string)


(defcustom porth-intrinsics
  '(("dup"
     "a -- a a"
     "duplicate an element on top of the stack.")
    ("swap"
     "a b -- b a"
     "swap 2 elements on the top of the stack.")
    ("drop"
     "a b -- a"
     "drops the top element of the stack.")
    ("print"
     "a b -- a"
     "print the element on top of the stack in a free form to stdout and remove it from the stack.")
    ("over"
     "a b -- a b a"
     "copy the element below the top of the stack")
    ("rot"
     "a b c -- b c a"
     "rotate the top three stack elements.")
    ("= "
     "[a: int] [b: int] -- [a == b : bool]"
     "checks if two elements on top of the stack are equal.")
    ("!="
     "[a: int] [b: int] -- [a != b : bool]"
     "checks if two elements on top of the stack are not equal.")
    ("> "
     "[a: int] [b: int] -- [a > b  : bool]"
     "applies the greater comparison on top two elements.")
    ("< "
     "[a: int] [b: int] -- [a < b  : bool]"
     "applies the less comparison on top two elements.")
    (">="
     "[a: int] [b: int] -- [a >= b : bool]"
     "applies the greater or equal comparison on top two elements")
    ("<="
     "[a: int] [b: int] -- [a <= b : bool]"
     "applies the greater or equal comparison on top two elements.")
    ("+"
     "[a: int] [b: int] -- [a + b: int]"
     "sums up two elements on the top of the stack.")
    ("-"
     "[a: int] [b: int] -- [a - b: int]"
     "subtracts two elements on the top of the stack")
    ("*"
     "[a: int] [b: int] -- [a * b: int]"
     "multiples two elements on top of the stack")
    ("divmod"
     "[a: int] [b: int] -- [a / b: int] [a % b: int]"
     "perform [Euclidean division](https://en.wikipedia.org/wiki/Euclidean_division) between two elements on top of the stack.")
    ("max"
     "[a: int] [b: int] -- [max(a, b): int]"
     "compute maximum between two numbers")
    ("shr"
     "[a: int] [b: int] -- [a >> b: int]"
     "right **unsigned** bit shift.")
    ("shl"
     "[a: int] [b: int] -- [a << b: int]"
     "light bit shift.")
    ("and"
     "[a: int] [b: int] -- [a & b: int]"
     "bit `and`.")
    ("not"
     "[a: int] -- [~a: int]"
     "bit `not`.")
    ("mem"
     "-- [mem: ptr]"
     "pushes the address of the beginning of the memory where you can read and write onto the stack.")
    ("!8"
     "[byte: int] [place: ptr] -- "
     "store a given byte at the address on the stack.")
    ("@8"
     "[place: ptr] -- [byte: int]"
     "load a byte from the address on the stack.")
    ("!16"
     "[byte: int] [place: ptr] --"
     "store an 2-byte word at the address on the stack.")
    ("@16"
     "[place: ptr] -- [byte: int]"
     "load an 2-byte word from the address on the stack.")
    ("!32"
     "[byte: int] [place: ptr] --"
     "store an 4-byte word at the address on the stack.")
    ("@32"
     "[place: ptr] -- [byte: int]"
     "load an 4-byte word from the address on the stack.")
    ("!64"
     "[byte: int] [place: ptr] --"
     "store an 8-byte word at the address on the stack.")
    ("@64"
     "[place: ptr] -- [byte: int]"
     "load an 8-byte word from the address on the stack.")
    ("cast(int)"
     "[a: any] -- [a: int]"
     "cast the element on top of the stack to `int`")
    ("cast(bool)"
     "[a: any] -- [a: bool]"
     "cast the element on top of the stack to `bool`")
    ("cast(ptr)"
     "[a: any] -- [a: ptr]"
     "cast the element on top of the stack to `ptr`")
    ("syscall0"
     ""
     "perform a syscall with 0 arguments where 0 is in range")
    ("syscall1"
     ""
     "perform a syscall with 1 arguments where 1 is in range")
    ("syscall2"
     ""
     "perform a syscall with 2 arguments where 2 is in range")
    ("syscall3"
     ""
     "perform a syscall with 3 arguments where 3 is in range")
    ("syscall4"
     ""
     "perform a syscall with 4 arguments where 4 is in range")
    ("syscall5"
     ""
     "perform a syscall with 5 arguments where 5 is in range")
    ("here"
     "a -- a [len: int] [str: ptr]"
     "pushes a string "<file-path>:<row>:<col>"")
    ("argc"
     "-- [argc: int]"
     "")
    ("argv"
     "-- [argv: ptr]"
     ""))
  "Intrinsics (built-in words) of Porth."
  :group 'porth
  :type '('('string 'string 'string))
  )


(defcustom porth-intrinsics-names
  (-map '-first-item porth-intrinsics)
  "Names of intrinsics (built-in words) of Porth."
  :group 'porth
  :type '('string))


(defcustom porth-intrinsic-regexp
  (regexp-opt porth-intrinsics-names t)
  "Regexp of intrinsic (built-in word) in Porth."
  :group 'porth
  :type 'string)


(defconst porth-highlights
  `((,(regexp-opt porth-keywords 'symbols) . font-lock-keyword-face)
    (,(regexp-opt porth-intrinsics-names 'symbols) . font-lock-builtin-face)))


(defun porth-nav-forward-intrinsic (arg)
    "Move forward across one intrinsic.
With `ARG`, do it that many times.  Negative arg -N means move backward across N
balanced expressions.  This command assumes point is not in a
string or comment."
    (interactive "P")
    (or arg (setq arg 1)) ; set arg to 1, by default
    (search-forward-regexp porth-intrinsic-regexp nil t arg)
    )


(define-derived-mode porth-mode prog-mode
    "Porth"
    "Major mode for editing Porth's programs."
    :group 'porth-mode

    (setq-local comment-start "//")
    (setq-local comment-padding 1)
    (setq-local comment-end "")
    (setq-local forward-sexp-function 'porth-nav-forward-intrinsic)

    (setq-local eldoc-documentation-function 'porth-eldoc)

    (setq font-lock-defaults '(porth-highlights))

    (setq-local indent-tabs-mode nil)
    (setq-local tab-width 8)
    (setq-local comment-auto-fill-only-comments t)
    (when (boundp 'electric-indent-inhibit)
        (setq electric-indent-inhibit t))
    )

(add-to-list 'auto-mode-alist '("\\.porth\\'" . porth-mode))

(require 'porth-mode-doc)

(provide 'porth-mode)
;;; porth-mode.el ends here
