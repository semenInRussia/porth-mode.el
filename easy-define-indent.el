;;; easy-define-indent --- Mark create indent region function as easy task. -*- lexical-binding: t; -*-

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
(require 'dash)
(require 'functools)
(require 's)
(require 'hooks)

(require 'indention-plist)
(require 'namespace)


(defgroup indention nil
    "Package for easy define indention functions and vars.")


(defcustom indention/default-one-indent "    "
  "Default indention when define for major mode."
  :group 'indention
  :type 'string)


(defvar indention/increment-indent-level-function nil
  "This is function, which raise indent level of current line.
Please, don't touch, this is change automatically.")


(defvar indention/decrement-indent-level-function nil
  "This is function, which deindent current line.
Please, don't touch, this is change automatically.")


(defcustom indention/text-symbol-regexp "[^\t \n]"
  "Regexp for indicate text symbols."
  :group 'indention
  :type 'string)


(cl-defmacro indention/define-for-major-mode
    (major-mode
     &optional namespace
     &key
       rules
       (one-indent indention/default-one-indent)
       (copy-indention-of-previous-line t)
       (clear-old-indention nil)
       (clear-empty-lines t)
       )
    "Create all variables and functions for indent code in `MAJOR-MODE`.
All vars and functions will save in `NAMESPACE`.  `RULES` is list of
rules which you can create with `indention/make-rule`."
    (let ((major-mode-name (symbol-name major-mode))
          (one-indent-of-mode `(eval (from-namespace ,namespace
                                                     one-indent))))
        `(progn
             (defcustom-from-namespace ,namespace indention-rules
                 nil
                 ,(s-lex-format "Rules of indention for ${major-mode-name}.")
                 :group ',major-mode
                 :type '(repeat (list function function)))

             (from-namespace-setq ,namespace indention-rules ,rules)

             (defcustom-from-namespace ,namespace each-line-before-indent-hook
                 nil
                 "Hooks which run before each indent line."
                 :group ',major-mode
                 :type '(repeat function))

             (defcustom-from-namespace ,namespace each-line-after-indent-hook
                 nil
                 "Hooks which run after each indent line."
                 :group ',major-mode
                 :type '(repeat function))

             (defcustom-from-namespace ,namespace before-run-indent-func-hook
                 nil
                 "Hooks which run before runing indent func."
                 :group ',major-mode
                 :type '(repeat function))

             (defcustom-from-namespace ,namespace after-run-indent-func-hook
                 nil
                 "Hooks which run after runing indent function."
                 :group ',major-mode
                 :type '(repeat function))

             (defcustom-from-namespace ,namespace before-indent-line-hook
                 nil
                 ,(s-lex-format
                   "This hooks run before `${major-mode-name}-indent-line'.")
                 :group ',major-mode
                 :type '(repeat function))

             (defcustom-from-namespace ,namespace after-indent-line-hook
                 nil
                 ,(s-lex-format
                   "This hooks run after `${major-mode-name}-indent-line'.")
                 :group ',major-mode
                 :type '(repeat function))

             (defcustom-from-namespace ,namespace before-indent-region-hook
                 nil
                 ,(s-lex-format
                   "This hooks run before `${major-mode-name}-indent-region'.")
                 :group ',major-mode
                 :type '(repeat function))

             (defcustom-from-namespace ,namespace after-indent-region-hook
                 nil
                 ,(s-lex-format
                   "This hooks run after `${major-mode-name}-indent-region'.")
                 :group ',major-mode
                 :type '(repeat function))

             (defcustom-from-namespace ,namespace before-indent-lines-hook
                 nil
                 ,(s-lex-format
                   "This hooks run before `${major-mode-name}-indent-lines'.")
                 :group ',major-mode
                 :type '(repeat function))

             (defcustom-from-namespace ,namespace after-indent-lines-hook
                 nil
                 ,(s-lex-format
                   "This hooks run after `${major-mode-name}-indent-lines'.")
                 :group ',major-mode
                 :type '(repeat function))

             (defcustom-from-namespace ,namespace before-indent-some-lines-hook
                 nil
                 ,(s-lex-format
                   "This hooks run before
`${major-mode-name}-indent-some-lines'.")
                 :group ',major-mode
                 :type '(repeat function))

             (defcustom-from-namespace ,namespace after-indent-some-lines-hook
                 nil
                 ,(s-lex-format
                   "This hooks run after
`${major-mode-name}-indent-some-lines`.")
                 :group ',major-mode
                 :type '(repeat function))

             (defcustom-from-namespace
                 ,namespace indent-some-lines-before-each-line-hook
                 nil
                 "This hook run before each indent line,
when indent some lines."
                 :group ',major-mode
                 :type '(repeat function))

             (defcustom-from-namespace
                 ,namespace indent-some-lines-after-each-line-hook
                 nil
                 "This hook run after each indent line, when indent some lines."
                 :group ',major-mode
                 :type '(repeat function))

             (hooks/union-from-namespace-to ,namespace
                                            before-indent-some-lines-hook
                                            ;; ---
                                            before-indent-lines-hook
                                            before-indent-region-hook)

             (hooks/union-from-namespace-to ,namespace
                                            after-indent-some-lines-hook
                                            ;; ---
                                            after-indent-lines-hook
                                            after-indent-region-hook)

             (hooks/union-from-namespace-to ,namespace
                                            before-run-indent-func-hook
                                            ;; ---
                                            before-indent-region-hook
                                            before-indent-lines-hook
                                            before-indent-line-hook)

             (hooks/union-from-namespace-to ,namespace
                                            after-run-indent-func-hook
                                            ;; ---
                                            after-indent-region-hook
                                            after-indent-lines-hook
                                            after-indent-line-hook)

             (hooks/add-to-hook-from-namespace
              ,namespace
              before-run-indent-func-hook
              (lambda ()
                  (setq indention/increment-indent-level-function
                        (from-namespace ,namespace increment-indent-level))
                  (setq indention/decrement-indent-level-function
                        (from-namespace ,namespace decrement-indent-level))))

             (hooks/add-to-hook-from-namespace
              ,namespace
              after-run-indent-func-hook
              'indention/to-defaults-change-indent-function)

             (defun-in-namespace
                 ,namespace add-indent-some-lines-before-each-line-hook ()
                 ,(s-lex-format
                   "Add to `${major-mode-name}-before-each-line-indent-hook'
`${major-mode-name}-indent-some-lines-before-each-line-hook'.")
                 (hooks/from-namespace-add-hook
                  ,namespace
                  before-each-line-indent-hook
                  run-indent-some-lines-before-each-line-hook)

                 (hooks/from-namespace-add-hook
                  ,namespace
                  after-each-line-indent-hook
                  run-indent-some-lines-after-each-line-hook)
                 )

             (defun-in-namespace
                 ,namespace
                 remove-indent-some-lines-before-each-line-hook ()
                 ,(s-lex-format
                   "Remove from
 `${major-mode-name}-before-each-line-indent-hook'
`${major-mode-name}-indent-some-lines-before-each-line-hook'.")
                 (hooks/from-namespace-remove-hook
                  ,namespace
                  before-each-line-indent-hook
                  run-indent-some-lines-before-each-line-hook)

                 (hooks/from-namespace-remove-hook
                  ,namespace
                  after-each-line-indent-hook
                  run-indent-some-lines-after-each-line-hook))

             (defun-in-namespace
                 ,namespace run-indent-some-lines-before-each-line-hook ()
                 "Run `indent-some-lines-before-each-line-hook'."
                 (hooks/run-from-namespace
                  ,namespace indent-some-lines-after-each-line-hook)
                 )

             (defun-in-namespace
                 ,namespace run-indent-some-lines-after-each-line-hook ()
                 "Run `indent-some-lines-before-each-line-hook'."
                 (hooks/run-from-namespace
                  ,namespace indent-some-lines-before-each-line-hook)
                 )

             (hooks/from-namespace-add-hook
              ,namespace
              before-indent-some-lines-hook
              add-indent-some-lines-before-each-line-hook)

             (hooks/from-namespace-add-hook
              ,namespace
              before-indent-some-lines-hook
              remove-indent-some-lines-before-each-line-hook)

             ,(if copy-indention-of-previous-line
                  `(hooks/add-to-hook-from-namespace
                    ,namespace
                    before-each-line-hook
                    'indention/duplicate-indention-of-prev-line))

             ,(if clear-old-indention
                  `(hooks/add-to-hook-from-namespace
                    ,namespace
                    before-each-line-hook
                    'indention/clear-indention))

             ,(if clear-empty-lines
                  `(hooks/add-to-hook-from-namespace
                    ,namespace
                    before-each-line-hook
                    'indention/if-empty-clear))

             (defcustom-from-namespace ,namespace one-indent
                 ,one-indent
                 ,(s-lex-format
                   "One level of indention for ${major-mode-name}.")
                 :group ',major-mode
                 :type 'string)

             (defun-in-namespace
                 ,namespace increment-indent-level ()
                 "Raise indention for current line."
                 (interactive)
                 (beginning-of-line)
                 (insert ,one-indent-of-mode))

             (defun-in-namespace
                 ,namespace decrement-indent-level ()
                 "Deindent current line."
                 (interactive)
                 (when (s-prefix-p ,one-indent-of-mode
                                   (indention/current-line))
                     (beginning-of-line)
                     (delete-forward-char (length ,one-indent-of-mode)))
                 )

             (defun-in-namespace
                 ,namespace indent-line (&optional line-num)
                 ,(s-lex-format
                   "Indent line with LINE-NUM, for `${major-mode-name}`.")
                 (interactive)
                 (when line-num
                     (goto-line line-num))
                 (hooks/run-from-namespace ,namespace before-indent-line-hook)
                 (from-namespace-funcall ,namespace
                                         indent-line-without-run-cmd-hooks)
                 (hooks/run-from-namespace ,namespace after-indent-line-hook)
                 )

             (defun-in-namespace
                 ,namespace indent-line-without-run-cmd-hooks (&optional
                                                               line-num)
                 "Indent line with `LINE-NUM`, but don't run command hooks."
                 (interactive)
                 (when line-num (goto-line line-num))
                 (indention/indent-line-with-sorted-rules
                  (eval (from-namespace ,namespace indention-rules))
                  :each-line-before-indent-hook
                  (from-namespace ,namespace before-each-line-hook)
                  :each-line-after-indent-hook
                  (from-namespace ,namespace after-each-line-hook))
                 )

             (defun-in-namespace
                 ,namespace indent-lines (beg end)
                 ,(s-lex-format
                   "Indent lines for ${major-mode-name} from `BEG` to `END`.
`BEG` and `END` are numbers of lines.")
                 (interactive (list
                               (line-number-at-pos (region-beginning))
                               (line-number-at-pos (region-end))))
                 (hooks/run-from-namespace ,namespace before-indent-lines-hook)
                 (from-namespace-funcall ,namespace
                                         indent-lines-without-run-cmd-hooks
                                         beg
                                         end)
                 (hooks/run-from-namespace ,namespace after-indent-lines-hook)
                 )

             (defun-in-namespace
                 ,namespace indent-lines-without-run-cmd-hooks (beg end)
                 "Indent lines without run cmd hooks from `BEG` to `END`.
`BEG` and `END` are numbers of lines."
                 (interactive (list
                               (line-number-at-pos (region-beginning))
                               (line-number-at-pos (region-end))))
                 (unless (= (- end beg) 0)
                     (from-namespace-funcall ,namespace
                                             indent-line-without-run-cmd-hooks
                                             beg)
                     (from-namespace-funcall ,namespace
                                             indent-lines-without-run-cmd-hooks
                                             (1+ beg)
                                             end))
                 )

             (defun-in-namespace
                 ,namespace indent-region (beg end)
                 ,(s-lex-format
                   "Indent region from `BEG` to `END` for
${major-mode-name}")
                 (interactive "r")
                 (hooks/run-from-namespace ,namespace
                                           before-indent-region-hook)
                 (from-namespace-funcall ,namespace
                                         indent-region-without-run-cmd-hooks
                                         beg end)
                 (hooks/run-from-namespace ,namespace
                                           after-indent-region-hook)
                 )

             (defun-in-namespace
                 ,namespace indent-region-without-run-cmd-hooks (beg end)
                 "Indent region from `BEG` to `END` without run cmd hooks."
                 (interactive "r")
                 (from-namespace-funcall ,namespace indent-lines
                                         (line-number-at-pos beg)
                                         (line-number-at-pos end))
                 )
             )))


(defun indention/to-defaults-change-indent-function ()
    "Set to `nil` `indention/increment-indent-level-function' and decremnt."
    (setq indention/increment-indent-level-function nil)
    (setq indention/decrement-indent-level-function nil)
    )


(defun indention/increment-indent-level (&optional n)
    "Increment indent level on `N`.
`N` defaults to 1."
    (interactive)
    (setq n (or n 1))
    (dotimes (_ n) (funcall indention/increment-indent-level-function))
    )


(defun indention/decrement-indent-level (&optional n)
    "Decrement indent level on `N` (deindent current line).
`N` defaults to 1."
    (interactive)
    (setq n (or n 1))
    (dotimes (_ n) (funcall indention/decrement-indent-level-function))
    )


(cl-defun indention/make-rule (&optional &rest args
                               &key
                                 indent-func
                                 predicate
                                 &allow-other-keys)
    "Create indention rule.
`INDENT-FUNC` is function which call when `PREDICATE` returns
non-nil value.  Possible `ARGS`:
* :check-on-prev-line
Before run `PREDICATE`, move to previous line.
* :on-keywords <keywords>
Call INDENT-FUNC When line has one of keywords.
If keyword of keywords has space, then this keywords parsed as keyword 1 and
keyword2 splitted (1+) spaces.
* :on-chars <chars>
Call INDENT-FUNC When line has one of chars.
* :add-indent
INDENT-FUNC is just add `one-indent' to current line.
* :deindent
INDENT-FUNC is deindent current line"
    (let ((keywords (ind-plist/get :on-keywords args))
          (chars (ind-plist/get :on-chars args))
          (add-indent (ind-plist/has :add-indent args))
          (deindent (ind-plist/has :deindent args))
          (check-on-prev-line (ind-plist/has :check-on-prev-line args)))
        (cond
          (keywords
           (setq predicate (indention/make-line-has-keywords-p keywords)))
          (chars
           (setq predicate (indention/make-line-has-chars-p chars))
           ))

        (when check-on-prev-line
            (setq predicate (indention/compose-with-prev-line predicate)))

        (cond
          (add-indent (setq indent-func 'indention/increment-indent-level))
          (deindent (setq indent-func 'indention/decrement-indent-level)))
        (list indent-func predicate)))


(defun indention/make-line-has-keywords-p (keywords)
    "Make func, which if line has one of KEYWORDS, return t."
    (declare (pure t) (side-effect-free t))
    (lambda () (indention/line-has-keywords-p keywords))
    )


(defun indention/line-has-keywords-p (keywords)
    "If S, has one of KEYWORDS, return t.
If keyword of keywords has space, then this keywords parsed as keyword 1 and
keyword2 splitted (1+) spaces."
    (-any 'indention/line-has-this-keyword-p keywords))


(defun indention/line-has-this-keyword-p (keyword)
    "If current line has KEYWORD, then return t.
If keyword has space(s), then this is parse as some words separated (1+)
spaces."
    (let ((keyword-regexp
           (->> keyword
                (s-split-words)
                (indention/regexp-words-separated-spaces)
                (indention/spaces-around-regexp))))
        (message "%s" keyword-regexp)
        (s-matches-p keyword-regexp
                     (indention/current-line)))
    )


(defun indention/regexp-words-separated-spaces (words)
    "Get regexp, which match all WORDS separated spaces."
    (s-join " +" words)
    )


(defun indention/spaces-around-regexp (regexp)
    "Add spaces around REGEXP."
    (s-concat "^" regexp "$"
              "\\|"
              "^" regexp " "
              "\\|"
              " " regexp "$"
              "\\|"
              " " regexp " ")
    )


(defun indention/make-line-has-chars-p (chars)
    "Make func, which if line has one of CHARS, return t."
    (declare (pure t) (side-effect-free t))
    (lambda () (indention/line-has-chars-p chars))
    )


(defun indention/line-has-chars-p (chars)
    "If S, has one of CHARS, return t."
    (let ((line (indention/current-line)))
        (--any (s-contains-p (char-to-string it) line)
               (string-to-list chars))))


(defun indention/current-line ()
    "Get content of current string."
    (thing-at-point 'line t))


(defun indention/compose-with-prev-line (f)
    "Return func, which run `indention/previous-line' and function F.
If impossible go to previous line, then return nil."
    (lambda ()
        (when (eq 0 (indention/previous-line))
                                        ; when successively go to previous line
            (funcall f)))
    )


(defun indention/previous-line (&optional n)
    "Move on previous line N times.
Return amount of moved lines."
    (interactive)
    (setq n (or n 1))
    (forward-line (- n))
    )


(cl-defun indention/indent-line-with-sorted-rules
    (sorted-rules
     &key
       (each-line-before-indent-hook nil)
       (each-line-after-indent-hook nil))
    "Indent or don't indent current line depending on `SORTED-RULES`.
Before each indent of line call `EACH-LINE-BEFORE-INDENT-HOOK`, after
`EACH-LINE-AFTER-INDENT-HOOK`"
    (run-hooks each-line-before-indent-hook)
    (cl-loop for rule in sorted-rules do
         (when (indention/rule-indent-current-line-p rule)
             (indention/rule-call-indent-function rule)))
    (run-hooks each-line-after-indent-hook)
    )


(defun indention/rule-indent-current-line-p (rule)
    "Check this `RULE` must indent current line."
    (save-excursion
        (funcall (-second-item rule)))
    )


(defun indention/rule-call-indent-function (rule)
    "Call function for indent current line of `RULE`."
    (save-excursion
        (beginning-of-line)
        (funcall (-first-item rule)))
    )


(defun indention/duplicate-indention-of-prev-line ()
    "Duplicate for current line indention of previous line."
    (interactive)
    (indention/clear-indention)
    (save-excursion
        (indention/to-backward-not-empty-line)
        (indention/mark-indention)
        (copy-region-as-kill (region-beginning) (region-end)))
    (yank)
    )


(defun indention/to-backward-not-empty-line ()
    "Navigate to end of backward line not empty (has 1+ not whitespace symbol).
If empty line not found, then return nil, if ok, then return t."
    (interactive)
    (forward-line -1)
    (end-of-line)
    (prog1
        (search-backward-regexp "[^ \n\t]" nil t)
        (end-of-line))
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
    (if (indention/empty-current-line-p)
                                        ; Then...
        (indention/mark-line)
                                        ; Else...
        (beginning-of-line)
        (let ((indent-end (1- (indention/point-at-forward-regexp
                               indention/text-symbol-regexp))))
            (indention/mark-region (point) indent-end)))
    )


(defun indention/point-at-forward-regexp (regexp &optional bound)
    "Return `point' at forward REGEXP in buffer.
If regexp not found, then get nil, otherwise get `point'.  Max `point' is
BOUND."
    (save-excursion (search-forward-regexp regexp bound t))
    )

(defun indention/mark-line ()
    "Mark whole current line, push position before run function."
    (indention/mark-region (point-at-bol) (point-at-eol))
    )


(defun indention/mark-region (beg end)
    "Mark region from BEG to END, push position before run function."
    (push-mark)
    (push-mark beg nil t)
    (goto-char end)
    )


(defun indention/if-empty-clear ()
    "If current line is empty, then clear line and navigate to next line."
    (interactive)
    (when (indention/empty-current-line-p)
        (kill-region (point-at-bol) (point-at-eol))
        (forward-line))
    )


(provide 'easy-define-indent)
;;; easy-define-indent.el ends here
