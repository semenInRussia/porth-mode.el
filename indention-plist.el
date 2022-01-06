;;; indention-plist --- Operations on `plist`. -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1
;; Packages-Requires: ((dash "2.18.0")
;;                     (s    "1.12.0"))

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

(defun ind-plist/has (flag plist)
    "If PLIST has FLAG, get postion of flag, otherwise get nil."
    (-elem-index flag plist))


(defun ind-plist/get (flag plist)
    "Get value in PLIST after FLAG.
Example:
    (:x 1 :y 2) :y → 2
    (:x 1 :y 2) :x → 1"
    (-when-let (flag-pos (ind-plist/has flag plist))
        (ind-plist/safe-nth (1+ flag-pos) plist))
    )


(defun ind-plist/safe-nth (n list)
    "As nth, but if error then nil.
N is index of element in LIST."
    (ignore-errors (nth n list))
    )

(provide 'indention-plist)
;;; indention-plist.el ends here
