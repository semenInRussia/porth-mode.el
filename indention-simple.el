;;; indention-simple --- Simple functions for Emacs -*- lexical-binding: t; -*-

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

(require 'dash)
(require 's)


(defcustom ind-simple nil
  "Simple functions for Emacs.")


(defcustom simple-text-symbol-regexp "[^\t \n]"
  "Regexp for indicate text symbols."
  :group 'ind-simple
  :type 'string)


(defun simple-compose-with-prev-line (f)
    "Return func, which run `simple-previous-line' and function F.
If impossible go to previous line, then return nil."
    (lambda ()
        (when (eq 0 (simple-previous-line))
                                        ; when successively go to previous line
            (funcall f)))
    )


(defun simple-previous-line (&optional n)
    "Move on previous line N times, return amount of moved lines."
    (interactive)
    (setq n (or n 1))
    (forward-line (- n))
    )


(defun simple-duplicate-indention-of-prev-line ()
    "Duplicate for current line indention of previous line."
    (interactive)
    (simple-clear-indention)
    (save-excursion (simple-to-backward-not-empty-line)
                    (simple-mark-indention)
                    (copy-region-as-kill (region-beginning) (region-end)))
    (yank)
    )


(defun simple-to-backward-not-empty-line ()
    "Navigate to end of backward line not empty (has 1+ not whitespace symbol).
If empty line not found, then return nil, if ok, then return t."
    (interactive)
    (forward-line -1)
    (end-of-line)
    (prog1
        (search-backward-regexp "[^ \n\t]" nil t)
        (end-of-line))
    )


(defun simple-empty-current-line-p ()
    "Is current line empty (\"   \", \" \", \"\")?."
    (s-blank-p (s-trim (simple-current-line)))
    )


(defun simple-mark-indention ()
    "Mark as selected region indention of current line."
    (interactive)
    (if (simple-empty-current-line-p)
                                        ; Then...
        (simple-mark-line)
                                        ; Else...
        (beginning-of-line)
        (let ((indent-end (1- (simple-point-at-forward-regexp
                               simple-text-symbol-regexp))))
            (simple-mark-region (point) indent-end)))
    )


(defun simple-clear-indention ()
    "Clear region indention of current line."
    (interactive)
    (simple-mark-indention)
    (kill-region (region-beginning) (region-end))
    )


(defun simple-point-at-forward-regexp (regexp &optional bound)
    "Return `point' at forward REGEXP in buffer.
If regexp not found, then get nil, otherwise get `point'.  Max `point' is
BOUND."
    (save-excursion (search-forward-regexp regexp bound t))
    )


(defun simple-mark-line ()
    "Mark whole current line, push position before run function."
    (simple-mark-region (point-at-bol) (point-at-eol))
    )


(defun simple-mark-region (beg end)
    "Mark region from BEG to END, push position before run function."
    (push-mark)
    (push-mark beg nil t)
    (goto-char end)
    )


(defun simple-if-empty-clear ()
    "If current line is empty, then clear line and navigate to next line."
    (interactive)
    (when (simple-empty-current-line-p)
        (kill-region (point-at-bol) (point-at-eol))
        (forward-line))
    )


(defun simple-current-line ()
    "Get content of current string."
    (thing-at-point 'line t))



(provide 'indention-simple)
;;; simple.el ends here
