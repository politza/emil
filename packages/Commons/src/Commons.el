;;; Commons.el --- Common ELisp functions.             -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Andreas Politz

;; Author: Andreas Politz <mail@andreas-politz.de>
;; Keywords: extensions
;; Version: 1.0.0beta1
;; Package-Requires: ((emacs "29.1") (dash "2.19.1"))

(eval-and-compile (require 'dash))

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

(defun Commons:split-property-list-start (list)
  (let ((property-list nil))
    (while (and (cdr list)
                (keywordp (car list)))
      (push (pop list) property-list)
      (push (pop list) property-list))
    (list (nreverse property-list) list)))

(defun Commons:split-property-list-end (list)
  (--split-with (not (keywordp it)) list))

(defun Commons:load-relative (feature)
  (let ((load-path (cons (file-name-directory
                          (or (Commons:evaluation-context-filename)
                              (error
                               "Unable to determine the current evaluation context")))
                         load-path)))
    (load (format "%s" feature) nil :no-message)))

(defun Commons:evaluation-context-filename ()
  (or (and (boundp 'byte-compile-current-file)
           byte-compile-current-file)
      load-file-name
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
     (define-error ',name ,message ',parent)
     (defun ,name (fmt &rest arguments)
       ,(format
         "Signals `%s' with ARGUMENTS applied to FMT as data."
         name)
       (declare (indent 1))
       (signal ',name (apply fmt arguments)))))

(defun Commons:constant-symbol? (symbol)
  "Returns non-nil, if SYMBOL is a constant symbol.

Constant symbols are `nil', `t' and all keywords. Signals a
`wrong-type-argument', if SYMBOL is not a symbol."
  (cl-check-type symbol symbol)
  (or (memq symbol '(nil t))
      (keywordp symbol)))

(provide 'Commons)
;;; Commons.el ends here
