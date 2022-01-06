;;; porth-mode-doc --- View documentation for Porth language in `porth-mode`

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

(defcustom porth-eldoc-obarray (make-vector 200 0)
  "Eldoc obarray for Porth.
It's contains pair of `intern` and strings (docs)."
  :group 'porth
  :type 'vector)


(defun porth-eldoc-obarray-init ()
    "Initialize `porth-eldoc-obarray`."
    (setq porth-eldoc-obarray (make-vector 200 0))

    (--map (set (intern (-first-item it) porth-eldoc-obarray)
                (s-concat (-second-item it) " | " (-third-item it)))
           porth-intrinsics)
    )


(defun porth-eldoc ()
    "Eldoc for Porth function."
    (symbol-value
     (intern-soft (thing-at-point 'symbol)
                  porth-eldoc-obarray)))

(porth-eldoc-obarray-init)

(provide 'porth-mode-doc)
;;; porth-mode-doc.el ends here
