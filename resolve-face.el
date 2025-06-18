;;; resolve-face.el --- Utility to fully resolve face attributes -*- lexical-binding: t; -*-

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

;;; Code:

(require 'cl-lib) ; cl-loop

;; This is a copy of `face-attribute' that has been updated to allow `face' to
;; be an anonymous face. The first place `face' is referenced is in initializing
;; `value' in the let binding. The call to `facep' checks if it is a symbol
;; referring to a face. If `facep' returns `true' then the code path is
;; unchanged from the original function `face-attribute'. If `facep' returns
;; false then `value' is initialized using the value in `face' which is assumed
;; to be a plist. If `attribute' is not found in the plist then `'unspecified'
;; is used as a default. `'unspecified' is considered "relative" along with
;; floating point numbers and will get replaced by an inherited value if
;; `inherit' is not nil. If `inherit' is `t' and no inherited value exists to
;; override `attribute' then `attribute' stays `'unspecified'. If `inherit' is a
;; face then that face is used as a last resort after looking for `attribute' in
;; the inherited face attributes. You can pass `'default' as `inherit' to
;; definitely resolve `attribute' to something other than `'unspecified'. The
;; second time `face' is referenced is when the function looks up what other
;; faces this face inherits from. If it's an anonymous face (i.e. `facep'
;; returns `nil') then we will instead assume `face' to be a plist and look up
;; the `:inherit' key from the plist. The rest of the code is unmodified from
;; the original.
(defun face-attribute+ (face attribute &optional frame inherit)
  "This function is like `face-attribute' except that FACE can be
 anonymous. If `facep' is true for FACE then this function
 behaves identically to `face-attribute'. Otherwise, where
 `face-attribute' would fail, it is updated to do the sensible thing."
  (let ((value (if (facep face)
                   (internal-get-lisp-face-attribute face attribute frame)
                 (or (plist-get face attribute) 'unspecified))))
    (when (and inherit (face-attribute-relative-p attribute value))
      ;; VALUE is relative, so merge with inherited faces
      (let ((inh-from (if (facep face)
                          (face-attribute face :inherit frame)
                        (plist-get face :inherit))))
        (unless (or (null inh-from) (eq inh-from 'unspecified))
          (condition-case nil
              (setq value
                    (face-attribute-merged-with attribute value inh-from frame))
            ;; The `inherit' attribute may point to non existent faces.
            (error nil)))))
    (when (and inherit
               (not (eq inherit t))
               (face-attribute-relative-p attribute value))
      ;; We should merge with INHERIT as well
      (setq value (face-attribute-merged-with attribute value inherit frame)))
    value))

(defun resolve-face-attributes (face &optional frame inherit)
  "Return an alist stating the attributes of FACEE. Each element
of the result has the form (ATTR-NAME . ATTR-VALUE).

If the optional argument FRAME is given, report on face FACE in that
frame. If FRAME is t, report on the defaults for face FACE (for new
frames). If FRAME is omitted or nil, use the selected frame.

If INHERIT is nil, only attributes directly defined by FACE are
considered. If INHERIT is non-nil, FACE's definition of ATTRIBUTE is
merged with the faces specified by its `:inherit' attribute. If INHERIT
is a face or a list of faces, then the result is further merged with
that face (or faces), until it becomes specified and absolute."
  (cl-loop for (k . _) in face-attribute-name-alist
           unless (eq k :inherit)
           for v = (face-attribute+ face k frame inherit)
           unless (eq v 'unspecified)
           collect
           (cons k v)))


(defun resolve-face-attributes-as-alist-of-symbols (face &optional frame inherit)
  (cl-loop for (k . _) in face-attribute-name-alist
           unless (eq k :inherit)
           for v = (face-attribute+ face k frame inherit)
           unless (eq v 'unspecified)
           collect (cons (intern (substring (symbol-name k) 1)) v)))


(defun resolve-face-attributes-as-plist-of-keywords (face &optional frame inherit)
  (cl-loop for (k . _) in face-attribute-name-alist
           unless (eq k :inherit)
           for v = (face-attribute+ face k frame inherit)
           unless (eq v 'unspecified)
           append (list k v)))

(provide 'resolve-face)

;;; resolve-face.el ends here
