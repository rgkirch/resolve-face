;;; resolve-face.el --- Utility to fully resolve face attributes -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Richie Kirchofer

;; Author: Richie Kirchofer
;; Keywords: faces, convenience
;; Version: 0.1
;; Package-Requires: ((emacs "24.3"))

;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package provides a single utility function, `resolve-face`,
;; designed to fully resolve any Emacs face specification into a
;; final, flat property list (plist) of its visual attributes.
;;
;; It correctly handles named faces, anonymous faces, single
;; inheritance, and multiple inheritance.

;;; Code:

(defun resolve-face (face)
  "Return a resolved plist of attributes for FACE.

FACE can be a named face symbol (e.g., 'font-lock-warning-face)
or an anonymous face plist (e.g., '(:inherit error :weight bold)).
Correctly handles single and multiple (:inherit '(face-a face-b)) inheritance."
  (cond
   ;; Case 1: FACE is a named symbol.
   ((symbolp face)
    (cl-loop for (attr) in custom-face-attributes
             for value = (face-attribute face attr nil t)
             when (and (not (eq attr :inherit))
                       value
                       (not (eq value 'unspecified)))
             collect attr and collect value))

   ;; Case 2: FACE is a list (anonymous face).
   ((consp face)
    (let* ((parents (plist-get face :inherit))
           (parent-attrs
            (cond
             ;; No inheritance
             ((null parents) '())
             ;; Single inheritance
             ((symbolp parents) (resolve-face parents))
             ;; Multiple inheritance (a list of faces)
             ((consp parents)
              (let ((merged-attrs '()))
                ;; Iterate through parents from right-to-left.
                ;; This ensures faces on the left of the list take precedence.
                (dolist (p (reverse parents))
                  (setq merged-attrs (append (resolve-face p) merged-attrs)))
                merged-attrs))))
           ;; Get the anonymous face's own specific attributes
           (own-attrs
            (cl-loop for (prop val) on face by #'cddr
                     unless (eq prop :inherit)
                     collect prop and collect val)))
      ;; Merge, with own-attrs taking highest precedence.
      (append own-attrs parent-attrs)))

   ;; Case 3: Invalid input.
   (t
    (error "Invalid face specifier: %S" face))))


(provide 'resolve-face)

;;; resolve-face.el ends here
