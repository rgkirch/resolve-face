;;; resolve-face-test.el --- ERT tests for resolve-face.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Richie Kirchofer

;; Author: Richie Kirchofer
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: faces
;; URL: https://github.com/rgkirch/resolve-face

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
;; This file contains ERT (Emacs Lisp Regression Testing) tests for the
;; `resolve-face` utility and its helper functions defined in `resolve-face.el`.

;;; Code:

(require 'ert)
(require 'resolve-face)
(require 'cl-lib)

(defface resolve-face-test-face-1
  nil
  "Face for testing."
  :group 'resolve-face-test)

(defface resolve-face-test-face-2
  nil
  "Face for testing."
  :group 'resolve-face-test)

(defface resolve-face-test-face-3
  nil
  "Face for testing."
  :group 'resolve-face-test)

(defun resolve-face-test-face-specs-set (&rest specs)
  "Set SPECS on faces."
  (cl-loop for (face spec . rest) on specs by #'cddr
           collect
           (face-spec-set face
                          spec
                          'face-defface-spec)))

(ert-deftest resolve-face-test-negative-two ()
  "Relative is applied to inherited absolute."
  (should (equal '((:foreground . "blue"))
                 (resolve-face-attributes '(:foreground "blue") nil t))))

(ert-deftest resolve-face-test-negative-one ()
  "Relative is applied to inherited absolute."
  (resolve-face-test-face-specs-set 'resolve-face-test-face-2
                                    '((t (:height 100)))
                                    'resolve-face-test-face-1
                                    '((t (:height 1.5 :inherit resolve-face-test-face-2))))
  (should (equal '((:height . 150))
                 (resolve-face-attributes 'resolve-face-test-face-1 nil t))))

(ert-deftest resolve-face-test-zero ()
  "Sanity."
  (resolve-face-test-face-specs-set 'resolve-face-test-face-1
                                    nil)
  (should (null (resolve-face-attributes 'resolve-face-test-face-1))))

(ert-deftest resolve-face-test-one ()
  "Walk up to the grandparent to get it but don't let the great grand parent override it."
  (resolve-face-test-face-specs-set 'test-face-4
                  '((t (:foreground "#ffffff")))
                  'resolve-face-test-face-3
                  '((t (:foreground "red" :inherit test-face-4)))
                  'resolve-face-test-face-2
                  '((t (:inherit resolve-face-test-face-3)))
                  'resolve-face-test-face-1
                  '((t (:inherit resolve-face-test-face-2))))
  (should (equal '((:foreground . "red"))
                 (resolve-face-attributes 'resolve-face-test-face-1 nil t))))

(ert-deftest resolve-face-test-two ()
  "More inheritance."
  (resolve-face-test-face-specs-set 'test-face-c
                  '((t ()))
                  'test-face-b
                  '((t (:foreground "#222222")))
                  'test-face-a
                  '((t ()))
                  'resolve-face-test-face-1
                  '((t (:inherit (test-face-a test-face-b test-face-c)))))
  (should (equal '((:foreground . "#222222"))
                 (resolve-face-attributes 'resolve-face-test-face-1 nil t))))

(ert-deftest resolve-face-test-three ()
  "in a list"
  (resolve-face-test-face-specs-set 'resolve-face-test-face-1
                  '((t (:foreground "#111111" :background "#222222"))))
  (should (equal '((:foreground . "#111111")
                   (:background . "#222222"))
                 (resolve-face-attributes 'resolve-face-test-face-1 nil t))))

(ert-deftest resolve-face-test-four ()
  "not in a list"
  (resolve-face-test-face-specs-set 'resolve-face-test-face-1
                  '((t :foreground "#111111" :background "#222222")))
  (should (equal '((:foreground . "#111111")
                   (:background . "#222222"))
                 (resolve-face-attributes 'resolve-face-test-face-1 nil t))))

 (provide 'resolve-face-test)

;;; resolve-face-test.el ends here
