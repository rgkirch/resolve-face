;;; resolve-face-test.el --- Tests for resolve-face.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Richie Kirchofer

;; Author: Richie Kirchofer
;; Keywords: faces, convenience, tests

;;; Commentary:
;;
;; This file contains tests for the `resolve-face` function using the
;; built-in `ert` (Emacs Lisp Regression Testing) framework.
;;
;; To run these tests, evaluate the buffer and then run `M-x ert-run-tests-interactively`.

;;; Code:

(require 'resolve-face)
(require 'ert)

;; Define some test faces to create a predictable environment
(defface test-face-parent
  '((t :foreground "red" :weight bold))
  "A parent face for testing.")

(defface test-face-grandparent
  '((t :background "blue" :slant italic))
  "A grandparent face for testing.")

(defface test-face-child
  '((t :inherit test-face-parent :foreground "green"))
  "A child face that inherits and overrides.")

(ert-deftest resolve-face-named-face-test ()
  "Test resolving a simple named face."
  (should (plist-member (resolve-face 'test-face-parent) :foreground))
  (should (equal (plist-get (resolve-face 'test-face-parent) :foreground) "red"))
  (should (equal (plist-get (resolve-face 'test-face-parent) :weight) 'bold)))

(ert-deftest resolve-face-named-inheritance-test ()
  "Test resolving a named face with inheritance."
  (let ((resolved (resolve-face 'test-face-child)))
    ;; It should have its own foreground color.
    (should (equal (plist-get resolved :foreground) "green"))
    ;; It should inherit the weight from its parent.
    (should (equal (plist-get resolved :weight) 'bold))
    ;; It should NOT contain the :inherit attribute.
    (should-not (plist-member resolved :inherit))))

(ert-deftest resolve-face-anonymous-single-inheritance-test ()
  "Test an anonymous face with single inheritance."
  (let ((resolved (resolve-face '(:inherit test-face-parent :slant italic))))
    ;; It should have its own slant.
    (should (equal (plist-get resolved :slant) 'italic))
    ;; It should inherit foreground and weight from parent.
    (should (equal (plist-get resolved :foreground) "red"))
    (should (equal (plist-get resolved :weight) 'bold))))

(ert-deftest resolve-face-anonymous-multiple-inheritance-test ()
  "Test an anonymous face with multiple inheritance."
  (let ((resolved (resolve-face '(:inherit (test-face-parent test-face-grandparent) :foreground "yellow"))))
    ;; Its own attribute should take highest precedence.
    (should (equal (plist-get resolved :foreground) "yellow"))
    ;; The leftmost parent's attribute (:weight) should take precedence.
    (should (equal (plist-get resolved :weight) 'bold))
    ;; It should inherit attributes from the rightmost parent if not otherwise specified.
    (should (equal (plist-get resolved :background) "blue"))
    (should (equal (plist-get resolved :slant) 'italic))))

(ert-deftest resolve-face-no-inheritance-test ()
  "Test an anonymous face with no inheritance."
  (let ((resolved (resolve-face '(:background "cyan"))))
    (should (equal (plist-get resolved :background) "cyan"))
    (should-not (plist-member resolved :foreground))))

(provide 'resolve-face-test)

;;; resolve-face-test.el ends here
