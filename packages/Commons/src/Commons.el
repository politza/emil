;;; Commons.el --- Common ELisp functions.             -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Andreas Politz

;; Author: Andreas Politz
;; Keywords: extensions
;; Version: 1.0.0beta1
;; Package-Requires: ((emacs "29.1") (dash "2.19.1"))

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

(require 'dash)
(eval-when-compile (require 'cl-macs))

(defun Commons:keyword-to-symbol (keyword)
  (unless (keywordp keyword)
    (error "Keyword expected: %s" keyword))
  (intern (substring (symbol-name keyword) 1)))

(defun Commons:symbol-to-keyword (symbol)
  (cond
   ((keywordp symbol)
    symbol)
   ((symbolp symbol)
    (intern (concat ":" (symbol-name symbol))))
   (t
    (signal 'wrong-type-argument (list 'symbol symbol)))))

(defun Commons:split-property-list (list &optional where)
  (pcase (or where :start)
    (:start
     (let ((property-list nil))
       (while (and (cdr list)
                   (keywordp (car list)))
         (push (pop list) property-list)
         (push (pop list) property-list))
       (list (nreverse property-list) list)))
    (:end
     (--split-with (not (keywordp it)) list))
    (argument (signal 'wrong-type-argument
                      (list '(member :start :end) argument)))))

(defun Commons:load-relative (feature)
  (let ((load-path (cons (file-name-directory
                          (or (Commons:evaluation-context-filename)
                              (error
                               "Unable to determine the current evaluation context")))
                         load-path)))
    (load (format "%s" feature) nil :no-message)))

(defun Commons:evaluation-context-filename ()
  (or load-file-name
      (and (boundp 'byte-compile-current-file)
           byte-compile-current-file)
      buffer-file-name))

(defun Commons:evaluation-context ()
  (cond
   ((and (boundp 'byte-compile-current-file)
         byte-compile-current-file)
    'compile)
   (load-file-name 'load)
   (t 'eval)))

(defmacro Commons:define-error (name message &optional parent)
  "Defines a new error type and a corresponding constructor.

Defines a new error signal by passing NAME, MESSAGE and PARENT to
`define-error' unevaluated.

Additionally defines a function named NAME, which accepts the
same arguments as the `error' function and signals an error of
the new type with the formatted string as error-data."
  (declare (indent 1))
  (cl-check-type name symbol)
  (cl-check-type message string)
  (cl-check-type parent (or null symbol))
  `(progn
     (when (and ',parent (not (get ',parent 'error-conditions)))
       (error "Parent should be a defined error: %s" ',parent))
     (define-error ',name ,message ',parent)
     (defun ,name (fmt &rest arguments)
       ,(format
         "Signals `%s' with ARGUMENTS applied to FMT as data."
         name)
       (signal ',name (list (apply #'format fmt arguments))))))

(defun Commons:constant-symbol? (symbol)
  "Returns non-nil, if SYMBOL is a constant symbol.

Constant symbols are `nil', `t' and all keywords. Signals a
`wrong-type-argument', if SYMBOL is not a symbol."
  (cl-check-type symbol symbol)
  (or (memq symbol '(nil t))
      (keywordp symbol)))

(provide 'Commons)
;;; Commons.el ends here
