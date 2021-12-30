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
    `(--map (add-hook it (lambda () (run-hooks ,destination-hook)))
            (list ,@hooks)))


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
    (setq hooks (->> hooks
                     (--map (from-namespace-for-symbols namespace it))
                     (-map (lambda (el) `(quote ,el)))))
    `(hooks/union-to ',dest-hook ,@hooks)
    )


;;; hooks.el ends here
