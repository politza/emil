;; -*- lexical-binding: t -*-

;; Copyright (C) 2024  Andreas Politz

;; Author: Andreas Politz

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

(require 'Struct)
(require 'Struct/Impl)
(require 'Trait)

(Struct:define Emil:Support:StructImpl
  (name :type symbol)
  (properties :type list)
  (functions :type (List (Struct:Function))))

(Struct:define Emil:Support:TraitDef
  (name :type symbol)
  (supertraits :type (List symbol))
  (properties :type list)
  (functions :type (List (Struct:Function))))

(Struct:define Emil:Support:TraitImpl
  (name :type symbol)
  (type :type symbol)
  (properties :type list)
  (functions :type (List (Struct:Function))))

(defconst Emil:Support:structure-start-regexp
  (format "([ \t]*%s\\_>"
          (regexp-opt
           '("Struct:implement" "Trait:define" "Trait:implement")
           t)))

(defconst Emil:Support:keyword-regexp ":\\(?:\\w\\|\\s_\\)+")

(defconst Emil:Support:fn-regexp "^ *( *fn \\(\\(?:\\w\\|\\s_\\)+\\)")

(defun Emil:Support:parse (&optional position include-all-bodies?)
  (unless position (setq position (point)))
  (let ((symbols-with-pos-enabled t))
    (save-excursion
      (goto-char position)
      (ignore-error 'user-error
        (while (not (looking-at-p Emil:Support:structure-start-regexp))
          (up-list -1 t t)))
      (when (looking-at Emil:Support:structure-start-regexp)
        (let ((body-position (unless include-all-bodies? position))
              (kind (save-excursion
                      (goto-char (match-beginning 1))
                      (read-positioning-symbols (current-buffer))))
              (end (Emil:Support:structure-end)))
          (save-restriction
            (narrow-to-region (point) end)
            (condition-case-unless-debug nil
                (cl-case kind
                  (Struct:implement (Emil:Support:parse-struct-impl body-position))
                  (Trait:define (Emil:Support:parse-trait-def body-position))
                  (Trait:implement (Emil:Support:parse-trait-impl body-position)))
              (error nil))))))))

(defun Emil:Support:current-function (&optional position)
  (save-excursion
    (when position (goto-char position))
    (skip-syntax-backward " ")
    (ignore-error 'user-error
      (while (not (looking-at-p Emil:Support:fn-regexp))
        (up-list -1 t t)
        (skip-syntax-backward " ")))
    (when (looking-at Emil:Support:fn-regexp)
      (goto-char (match-beginning 1))
      (read-positioning-symbols (current-buffer)))))

(defun Emil:Support:structure-end ()
  (save-excursion
    (or (ignore-errors (forward-sexp)
                       (point))
        (let ((column (current-column)))
          (while (and (not (eobp))
                      (>= (current-column) column))
            (forward-line 1))
          (point)))))

(defun Emil:Support:parse-struct-impl (&optional include-only-body-at)
  (let ((name nil)
        (properties nil)
        (functions nil))
    (down-list 1)
    (forward-sexp 2)
    (backward-sexp)
    (when (setq name (read-positioning-symbols (current-buffer)))
      (setq properties (Emil:Support:parse-properties))
      (setq functions (Emil:Support:map-functions
                       (lambda (fn-name arguments body)
                         (Struct:read-function
                          name
                          `(fn ,fn-name ,arguments ,@body)
                          (plist-get properties :disable-syntax)))
                       include-only-body-at))
      (Emil:Support:StructImpl* name functions properties))))

(defun Emil:Support:parse-trait-def (&optional include-only-body-at)
  (let ((name nil)
        (supertraits nil)
        (properties nil)
        (functions nil))
    (down-list 1)
    (forward-sexp 2)
    (backward-sexp)
    (when (setq name (read-positioning-symbols (current-buffer)))
      (setq supertraits (ignore-errors (read-positioning-symbols (current-buffer))))
      (unless (listp supertraits)
        (setq supertraits nil))
      (setq supertraits (-filter #'symbolp supertraits))
      (setq properties (Emil:Support:parse-properties))
      (setq functions (Emil:Support:map-functions
                       (lambda (fn-name arguments body)
                         (Trait:read-function
                          name
                          `(fn ,fn-name ,arguments ,@body)
                          (plist-get properties :disable-syntax)))
                       include-only-body-at))
      (Emil:Support:TraitDef* name supertraits properties functions))))

(defun Emil:Support:parse-trait-impl (&optional include-only-body-at)
  (let ((name nil)
        (type nil)
        (properties nil)
        (functions nil))
    (down-list 1)
    (forward-sexp 2)
    (backward-sexp)
    (when (and (setq name (read-positioning-symbols (current-buffer)))
               (setq type (read-positioning-symbols (current-buffer))))
      (setq properties (Emil:Support:parse-properties))
      (setq functions (Emil:Support:map-functions
                       (lambda (fn-name arguments body)
                         (Trait:read-impl
                          name
                          type
                          `(fn ,fn-name ,arguments ,@body)
                          (plist-get properties :disable-syntax)))
                       include-only-body-at))
      (Emil:Support:TraitImpl* name type properties functions))))

(defun Emil:Support:parse-properties ()
  (let ((properties nil))
    (skip-syntax-forward " >")
    (while (looking-at-p Emil:Support:keyword-regexp)
      (push (read-positioning-symbols (current-buffer)) properties)
      (push (read-positioning-symbols (current-buffer)) properties))
    (nreverse properties)))

(defun Emil:Support:map-functions (fn &optional include-only-body-at)
  (let ((functions nil))
    (skip-syntax-backward " ")
    (while (re-search-forward Emil:Support:fn-regexp nil t)
      (let* ((name (save-excursion
                     (goto-char (match-beginning 1))
                     (read-positioning-symbols (current-buffer))))
             (start (match-beginning 0))
             (end (save-excursion
                    (goto-char start)
                    (Emil:Support:structure-end)))
             (arguments (ignore-errors
                          (read-positioning-symbols (current-buffer))))
             (body (when (or (null include-only-body-at)
                             (and (>= include-only-body-at start)
                                  (< include-only-body-at end)))
                     (save-restriction
                       (narrow-to-region (point) end)
                       (Emil:Support:parse-body)))))
        (goto-char end)
        (push (funcall fn name arguments body)
              functions)))
    (nreverse functions)))

(defun Emil:Support:parse-body ()
  (let* ((ppss (syntax-ppss))
         (closer ())
         (body nil)
         (syntax-error nil)
         (change-group (prepare-change-group)))
    (dolist (p (nth 9 ppss))
      (push (cdr (syntax-after p)) closer))
    (unwind-protect
        (progn
          (save-excursion
            (goto-char (point-max))
            (insert (apply #'string closer)))
          (skip-syntax-forward " >")
          (while (and (not (eobp))
                      (not syntax-error))
            (condition-case nil
                (push (read-positioning-symbols (current-buffer)) body)
              (invalid-read-syntax
               (setq syntax-error t)))
            (skip-syntax-forward " >"))
          (nreverse body))
      (cancel-change-group change-group))))

(defmacro Emil:Support:with-declarations (structure &rest body)
  (declare (indent 1) (debug (form body)))
  (let ((structure* (make-symbol "structure"))
        (body-fn (make-symbol "body-fn")))
    `(let ((,structure* ,structure)
           (,body-fn (lambda () ,@body)))
       (pcase-exhaustive ,structure*
         ((Struct Emil:Support:StructImpl name functions)
          (let ((Struct:declared-functions
                 (cons (cons name functions)
                       Struct:declared-functions)))
            (funcall ,body-fn)))
         ((Struct Emil:Support:TraitImpl)
          (funcall ,body-fn))
         ((Struct Emil:Support:TraitDef name supertraits functions)
          (let ((Trait:declared-traits
                 (cons (cons name (Trait:Declaration* name supertraits functions))
                       Trait:declared-traits)))
            (funcall ,body-fn)))))))

(provide 'Emil/Support)
