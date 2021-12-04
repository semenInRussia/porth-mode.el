;;; easy-define-indent --- Mark create indent region function as easy task.-*- lexical-binding: t; -*-

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

(require 's)
(require 'dash)

(defgroup indention nil
    "Package for easy define indention functions and vars.")


(defconst indention/default-one-indent "    "
  "Default indention when define for major mode.")


(cl-defmacro indention/define-for-major-mode
    (major-mode
     &optional namespace
     &key
       rules
       (one-indent indention/default-one-indent)
       (copy-indention-of-previous-line t)
       (clear-old-indention nil)
       )
    "Create all variables and functions for indent code in `MAJOR-MODE`.
All vars and functions will save in `NAMESPACE`.  `RULES` is list of
 groups from rules which should create with `indention/make-rule`."
    (let ((major-mode-name (symbol-name major-mode)))
        `(progn
             (defcustom-from-namespace ,namespace indention-rules
                 (-map 'eval ,rules)
                 ,(s-lex-format "Rules of indention for ${major-mode-name}.")
                 :group ',major-mode
                 :type '(repeat (list function function integer)))

             (defcustom-from-namespace ,namespace each-line-after-indent-hook
                 nil
                 ,(s-lex-format
                   "Hooks which run after each indent line.")
                 :group ',major-mode
                 :type '(repeat function))

             (defcustom-from-namespace ,namespace each-line-before-indent-hook
                 nil
                 ,(s-lex-format
                   "Hooks which run before each indent line.")
                 :group ',major-mode
                 :type '(repeat function))

             (defcustom-from-namespace ,namespace one-indent
                 ,one-indent
                 "One level of indention for ${major-mode-name}."
                 :group ',major-mode
                 :type 'string)

             (defun-in-namespace
                 ,namespace add-indent-to-current-line ()
                 "Raise indention for current line."
                 (interactive)
                 (beginning-of-line)
                 (insert (eval ,(from-namespace-for-symbols namespace
                                                            'one-indent))))

             (defun-in-namespace
                 ,namespace indent-line (line-num)
                 ,(s-lex-format
                   "Indent line with `LINE-NUM` for ${major-mode-name}.")
                 (interactive (list (line-number-at-pos (point))))
                 (goto-line line-num)
                 (indention/indent-line-with-sorted-rules
                  (eval (from-namespace ,namespace indention-rules))
                  :each-line-before-indent-hook
                  (from-namespace ,namespace each-line-before-indent-hook)
                  :each-line-after-indent-hook
                  (from-namespace ,namespace each-line-after-indent-hook))
                 )

             (defun-in-namespace
                 ,namespace indent-lines (beg end)
                 ,(s-lex-format
                   "Indent lines for ${major-mode-name} from `BEG` to `END`.
`BEG` and `END` are numbers of lines.")
                 (interactive (list
                               (line-number-at-pos (region-beginning))
                               (line-number-at-pos (region-end))))
                 (unless (= (- end beg) 0)
                     (funcall-from-namespace ,namespace indent-line beg)
                     (funcall-from-namespace ,namespace indent-lines
                                             (1+ beg)
                                             end)
                     )
                 )


             (defun-in-namespace
                 ,namespace indent-region (beg end)
                 ,(s-lex-format
                   "Indent region from `BEG` to `END` for ${major-mode-name}")
                 (interactive "r")
                 (funcall-from-namespace ,namespace indent-lines
                                         (line-number-at-pos beg)
                                         (line-number-at-pos end))
                 )
             )))


(defun indention/make-rule (indent-current-line indent-line-p)
    "Create indention rule.
`INDENT-CURRENT-LINE` is function which call when `INDENT-LINE-P` returns
non-nil value.  If some rules' `INDENT-LINE-P` return non-nil value, then call
`INDENT-CURRENT-LINE` of rule with greatest `PRIORITY`."
    (list indent-current-line indent-line-p))


(cl-defun indention/indent-line-with-sorted-rules
    (sorted-rules &key
                    (each-line-before-indent-hook nil)
                    (each-line-after-indent-hook nil))
    "Indent or don't indent current line depending on `SORTED-RULES`.
If `COPY-INDENTION-OF-PREV-LINE` is true, then for each line copy indent of
previous line.  If `CLEAR-OLD-INDENTION` is true, then delete indention of
current line when check or indent current line."
    (run-hooks each-line-before-indent-hook)
    (cl-loop for rule in sorted-rules do
         (when (indention/rule-indent-current-line-p rule)
             (indention/rule-call-indent-function rule
                                                  )))
    (run-hooks each-line-after-indent-hook)
    )


(defun indention/rule-indent-current-line-p (rule)
    "Check: this `RULE` must indent current line."
    (save-excursion (funcall (-second-item rule)))
    )


(defun indention/rule-call-indent-function (rule)
    "Call function for indent current line of `RULE`."
    (save-excursion
        (beginning-of-line)
        (funcall (-first-item rule)))
    )


(defun indention/rule-priority (rule)
    "Get priority of `RULE`."
    (-third-item rule)
    )


(defun indention/duplicate-indention-of-prev-line ()
    "Duplicate for current line indention of previous line."
    (interactive)
    (indention/clear-indention)

    (save-excursion
        (forward-line -1)
        (indention/to-backward-not-empty-line)
        (indention/mark-indention)
        (copy-region-as-kill (region-beginning) (region-end)))

    (beginning-of-line)
    (yank)
    )


(defun indention/to-backward-not-empty-line ()
    "Navigate to backward line not empty (has 1+ not whitespace symbol)."
    (forward-line -1)
    (search-backward-regexp "^.*\\S+.*$" nil nil)
    )


(defun indention/empty-current-line-p ()
    "Is current line empty (\"   \", \" \", \"\")?."
    (s-blank-p (s-trim (thing-at-point 'line t)))
    )


(defun indention/clear-indention ()
    "Clear region indention of current line."
    (interactive)
    (indention/mark-indention)
    (kill-region (region-beginning) (region-end))
    )


(defun indention/mark-indention ()
    "Mark as selected region indention of current line."
    (interactive)
    (let* ((point-at-next-line (save-excursion (if (eq (forward-line) 1)
                                                   (end-of-line)
                                        ; When Still in current line ^
                                                   (beginning-of-line)
                                        ; When In forward line ^
                                                   )
                                               (point)))
           (point-at-not-space-char (save-excursion
                                        (beginning-of-line)
                                        (search-forward-regexp
                                         "[^ ]"
                                         point-at-next-line
                                         (1+ (point-at-bol)))
                                        (point))))
        (goto-char (point-at-bol))
        (set-mark (point))
        (goto-char (1- point-at-not-space-char)))
    )


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


(defmacro funcall-from-namespace (namespace func-name &rest args)
    "Call function with `FUNC-NAME` from `NAMESPACE` with `ARGS`."
    `(,(from-namespace-for-symbols namespace func-name) ,@args)
    )


(defmacro var-from-namespace (namespace var-name)
    "Get something symbol with `VAR-NAME` from `NAMESPACE`."
    `(eval ,(from-namespace-for-symbols namespace var-name))
    )


(defmacro from-namespace (namespace something-name)
    "Get something symbol with `SOMETHING-NAME` from `NAMESPACE`."
    `(intern ,(s-concat (symbol-name namespace)
                        "-"
                        (symbol-name something-name)))
    )


(defun from-namespace-for-symbols (namespace-symb something-name-symb)
    "Get something symbol with `SOMETHING-NAME` from `NAMESPACE`."
    (intern (s-concat (symbol-name namespace-symb)
                      "-"
                      (symbol-name something-name-symb)))
    )


(provide 'easy-define-indent)
;;; easy-define-indent.el ends here
