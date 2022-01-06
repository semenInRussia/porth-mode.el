;;; indention-make-rule --- Make rule function -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

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
(require 'cl)
(require 'dash)
(require 's)

(require 'functools)
(require 'indention-simple)


(defcustom ind/make-rule-basic-keywords '(:on-chars
                                          :check-on-prev-line
                                          :on-keywords
                                          :add-indent
                                          :deindent
                                          :if-true-dont-check-next-rules
                                          )
  "Keywords `ind/make-rule' which has handler `ind/:<word>/make-rule-handler'."
  :group 'indention
  :type '(repeat symbol))

;; (setq ind/make-rule-basic-keywords )


(defcustom ind/make-rule-special-keywords-handlers
  (list
   (list (-const t) 'ind/not-found-keyword-handler))
  "This is list of pairs from predicate or keyword and handlers.
Predicate take keyword, rule, values and return t or nil, when nil then,
handler will executing, handler take keyword and return modified,
rule.  If instead of predicate passed keyword, then predicate is function which
return true when keywords is equal.
This functions will call with rule and values, when call `ind/make-rule'.
Default handler is `ind/:<keyword-name>/make-rule-handler`."
  :group 'indention
  :type '(repeat '(predicate function)))


(defun ind/make-rule (&rest args)
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
INDENT-FUNC is deindent current line
* :if-true-dont-check-next-rules
If PREDICATE is true, then dont run INDENT-FUNC and dont run next indention
rules.  By default check next rules always."
    (--reduce-from (ind/handle-keyword (car it) acc (cdr it))
                   (ind/make-empty-rule)
                   (-partition-before-pred 'keywordp args))
    )


(defun ind/handle-keyword (keyword rule values)
    "Handle KEYWORD, then modify RULE with VALUES."
    (let ((handle-funtion (ind/make-rule-get-handler-of keyword)))
        (funcall handle-funtion rule values))
    )


(defun ind/make-rule-get-handler-of (keyword)
    "Get handler of KEYWORD in `ind/make-rule'."
    (if (-contains-p ind/make-rule-basic-keywords keyword)
        (ind/make-rule-get-basic-handler-of keyword)
        (ind/make-rule-get-special-handler-of keyword))
    )


(defun ind/make-rule-get-basic-handler-of (keyword)
    "Get basic handler of KEYWORD for `ind/make-rule'."
    (intern (s-lex-format "ind/${keyword}/make-rule-handler"))
    )


(defun ind/make-rule-get-special-handler-of (keyword)
    "Get special handler of KEYWORD for `ind/make-rule'."
    (let ((special-handler (-second-item
                            (--find
                             (ind/make-rule-special-handler-of-keyword-p keyword
                                                                         it)
                             ind/make-rule-special-keywords-handlers))))
        (lambda (rule values)
            (funcall special-handler keyword rule values)))
    )


(defun ind/make-rule-special-handler-of-keyword-p (keyword pred-and-handler)
    "Get t, when PRED-AND-HANDLER is special handler of KEYWORD.
PRED-AND-HANDLER is list from predicate, which take keyword and return
t when is suitable for keyword handler, and function which take rule and
values and return new rule.  Instead of predicate, also able be keyword,
when this keyword is equal other keyword, return t, by example with basic
predicate."
    (let ((pred-or-keyword (-first-item pred-and-handler))
          (handler (-second-item pred-and-handler)))
        (when (keywordp pred-or-keyword)
            (setq pred-or-keyword
                  (apply-partially 'eq pred-or-keyword)))
        (funcall pred-or-keyword keyword)
        )
    )


(defun ind/make-empty-rule ()
    "Return indention rule by defaults."
    '(nil nil t) ; By default check next rules (third t)
    )


(defun ind/not-found-keyword-handler (keyword rule values)
    "Message that, handler for KEYWORD not found, and return RULE.
Debug VALUES, passed to non exists handler."
    (message "Handler of keyword `%s` non exists, this is very bad!" keyword)
    (message "If you are has basic handler, just rename its to this template
`ind/:<keyword>/make-rule-handler`")
    (message "If your handler need to special rules, see to
`ind/make-rule-special-keywords-handlers'")
    (message "RULE for handler of `%s`: \n%s" keyword rule)
    (message "VALUES passed to non-exists handlers:\n%s" values)
    rule)


(defun ind/:on-chars/make-rule-handler (rule values)
    "Handler for `:on-chars' in `ind/make-rule'.
RULE is old rule, VALUES is list, in which first item is string of chars.
Return new rule."
    (let* ((chars (-first-item values))
           (new-predicate (ind/make-line-has-chars-p chars)))
        (ind/rule-set-predicate new-predicate rule))
    )


(defun ind/:on-keywords/make-rule-handler (rule keywords)
    "Handler for `:on-keywords' in `ind/make-rule'.
RULE is old rule, KEYWORDS is list of keywords"
    (let ((new-predicate (ind/make-line-has-keywords-p keywords)))
        (ind/rule-set-predicate new-predicate rule))
    )


(defun ind/:check-on-prev-line/make-rule-handler (rule &rest _)
    "Handler for `:check-on-prev-line' in `ind/make-rule'.
RULE is old rule, VALUES is list, in which first item is string of chars.
Return new rule."
    (ind/rule-apply-to-predicate 'simple-compose-with-prev-line
                                 rule)
    )


(defun ind/:add-indent/make-rule-handler (rule &rest _)
    "Handler for `:add-indent' in `ind/make-rule'.
Return new modified RULE."
    (ind/rule-set-indent-func 'indention/increment-indent-level rule)
    )


(defun ind/:if-true-dont-check-next-rules/make-rule-handler (rule &rest _)
    "Handler for `:if-true-check-next-rules' in `ind/make-rule'.
Return new modified RULE."
    (ind/rule-set-if-true-check-next-rules-p nil rule)
    )


(defun ind/:deindent/make-rule-handler (rule &rest _)
    "Handler for `:add-indent' in `ind/make-rule'.
Return new modified RULE."
    (ind/rule-set-indent-func 'indention/decrement-indent-level
                              rule)
    )


(defun ind/make-line-has-keywords-p (keywords)
    "Make func, which if line has one of KEYWORDS, return t."
    (declare (pure t) (side-effect-free t))
    (lambda () (ind/line-has-keywords-p keywords))
    )


(defun ind/line-has-keywords-p (keywords)
    "If S, has one of KEYWORDS, return t.
If keyword of keywords has space, then this keywords parsed as keyword 1 and
keyword2 splitted (1+) spaces."
    (-any 'ind/line-has-this-keyword-p keywords))


(defun ind/line-has-this-keyword-p (keyword)
    "If current line has KEYWORD, then return t.
If keyword has space(s), then this is parse as some words separated (1+)
spaces."
    (let ((keyword-regexp (->> keyword
                               (s-split-words)
                               (ind/regexp-words-separated-spaces)
                               (ind/spaces-around-regexp))))
        (s-matches-p keyword-regexp (simple-current-line)))
    )


(defun ind/regexp-words-separated-spaces (words)
    "Get regexp, which match all WORDS separated spaces."
    (s-join " +" words)
    )


(defun ind/spaces-around-regexp (regexp)
    "Add spaces around REGEXP."
    (s-concat "^" regexp "$"
              "\\|"
              "^" regexp " "
              "\\|"
              " " regexp "$"
              "\\|"
              " " regexp " ")
    )


(defun ind/make-line-has-chars-p (chars)
    "Make func, which if line has one of CHARS, return t."
    (declare (pure t) (side-effect-free t))
    (lambda () (ind/line-has-chars-p chars))
    )


(defun ind/line-has-chars-p (chars)
    "If S, has one of CHARS, return t."
    (let ((line (simple-current-line)))
        (--any (s-contains-p (char-to-string it) line)
               (string-to-list chars))))


(defun ind/rule-predicate (rule)
    "Get predicate of indention RULE."
    (-second-item rule)
    )


(defun ind/rule-indent-current-line-p (rule)
    "Check this RULE must indent current line."
    (save-excursion (funcall (ind/rule-predicate rule)))
    )


(defun ind/rule-set-indent-func (new-indent-func rule)
    "Set indent function of RULE to NEW-INDENT-FUNC."
    (-replace-at 0 new-indent-func rule)
    )


(defun ind/rule-call-indent-function (rule)
    "Call function for indent current line of `RULE`."
    (save-excursion
        (beginning-of-line)
        (funcall (-first-item rule)))
    )


(defun ind/rule-if-true-check-next-rules-p (rule)
    "Return t, if RULE need to check next rules, when RULE's predicate is t."
    (-third-item rule)
    )


(defun ind/rule-set-if-true-check-next-rules-p (x rule)
    "Set `if-true-check-next-rules-p' of RULE to X."
    (-replace-at 2 x rule)
    )


(defun ind/rule-check-next-rules-p (rule)
    "Return t, if RULE need to check next ind rules."
    (or (not (ind/rule-indent-current-line-p rule))
        (ind/rule-if-true-check-next-rules-p rule)))


(defun ind/rule-set-predicate (new-predicate rule)
    "Set predicate of RULE to NEW-PREDICATE."
    (-replace-at 1 new-predicate rule)
    )


(defun ind/rule-apply-to-predicate (f rule)
    "Apply F to predicate of RULE, and return updated RULE."
    (let* ((old-pred (ind/rule-predicate rule))
           (new-pred (funcall f old-pred)))
        (ind/rule-set-predicate new-pred rule))
    )


(cl-defun ind/indent-line-with-sorted-rules
    (sorted-rules
     &key
       (each-line-before-indent-hook nil)
       (each-line-after-indent-hook nil))
    "Indent or don't indent current line depending on `SORTED-RULES`.
Before each indent of line call EACH-LINE-BEFORE-INDENT-HOOK, after
EACH-LINE-AFTER-INDENT-HOOK"
    (prog2
        (run-hooks each-line-before-indent-hook)
        (->> sorted-rules
             (functools/take-including-while 'ind/rule-check-next-rules-p)
             (-filter 'ind/rule-indent-current-line-p)
             (-map 'ind/rule-call-indent-function))
        (run-hooks each-line-after-indent-hook))
    )



(provide 'indention-make-rule)
;;; indention-make-rule.el ends here
