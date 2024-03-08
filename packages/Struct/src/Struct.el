;;; Struct.el --- A struct data-type.             -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Andreas Politz

;; Author: Andreas Politz <mail@andreas-politz.de>
;; Keywords: extensions
;; Version: 1.0.0beta1
;; Package-Requires: ((emacs "29.1") (dash "2.19.1") (Commons "1.0.0beta1"))

(require 'Commons)
(require 'cl-macs)
(require 'dash)
(require 'Struct/Primitives)
(require 'Struct/Doc)
(require 'Struct/Pcase)

(require 'Struct/Syntax)

(defconst Struct:syntax-highlight-symbol 'Struct:syntax-highlight-symbol
  "Symbol to attach font-lock information to other symbols.")

(defvar Struct:enable-syntax-highlighting t
  "Whether to highlight struct types with face `font-lock-type-face'.")

(defmacro Struct:define (name &optional documentation &rest declarations)
  "Defines a new struct-type named NAMẸ.

DOCUMENTATION is an optional string describing the type.

STRUCT-META-PROPERTY is a keyword/value pair of which an arbitrary
number may be provided.  The supported properties are listed in the
documentation of `Struct:Type'.  All of which can be used, with the
exception of `:name', `:documentation' and `:properties'.

The remaining forms, if present, determine the properties of the
defined struct.

Each such form may be just the property's name, or a list consisting
of its name, an optional documentation string and an arbitrary number
of meta-properties.  See `Struct:Property' for a list of supported
meta-properties.  All of which mentioned there can be used.

Defining a struct also defines 2 functions, a macro and a new cl-type,
which are:

1. A type-constructor having the name of the struct. It accepts
keyword/value pairs for defining the properties of the struct-value.

2. A type-predicate having the name of the struct with a question-mark
appended.

3. Another type-constructor with the name of type with an asterisk
appended. It works like the first constructor, except that it is
defined as a macro and is therefore able to support shorthand- as well
as splice-syntax.

4. A cl-type having the name of the struct, which may be used with
`cl-check-type' and other cl-type features.

(fn NAME [DOCUMENTATION]? [STRUCT-META-PROPERTY]* [PROPERTY-NAME |
(PROPERTY-NAME [DOCUMENTATION]? [PROPERTY-META-PROPERTY]*)]*)"
  (declare (indent 1) (doc-string 2))

  (unless (symbolp name)
    (error "Expected a symbol: %s" name))
  (unless (or (stringp documentation)
              (null documentation))
    (push documentation declarations)
    (setq documentation nil))

  (-let* (((struct-declarations property-declarations)
           (Commons:split-property-list-start declarations))
          (struct-properties
           (append struct-declarations
                   (list :name name :documentation documentation
                         :properties
                         (-map #'Struct:-construct-property-entry
                               property-declarations))))
          (type (apply #'Struct:Type struct-properties))
          (macro-name (intern (concat (symbol-name name) "*")))
          (predicate-name (intern (concat (symbol-name name) "?"))))
    `(eval-and-compile
       (defun ,name (&rest properties)
         (Struct:construct ',name properties))

       (defmacro ,macro-name (&rest properties)
         (declare (no-font-lock-keyword t) (debug t))
         (list 'Struct:construct
               '',name (Struct:Syntax:expand-properties properties)))

       (defun ,predicate-name (object &optional ensure)
         (or (eq (car-safe object) ',name)
             (and ensure
                  (signal 'wrong-type-argument (list ',predicate-name object)))))

       (cl-deftype ,name ()
         (list 'satisfies (function ,predicate-name)))

       (put ',name 'function-documentation
         '(Struct:-doc-constructor (Struct:Type:get ',name :ensure)))
       (put ',macro-name 'function-documentation
         '(Struct:-doc-constructor (Struct:Type:get ',name :ensure)))
       (put ',predicate-name 'function-documentation
         '(Struct:-doc-predicate (Struct:Type:get ',name :ensure)))

       (Struct:Type:define ',name ',type)

       (when Struct:enable-syntax-highlighting
         (Struct:-add-syntax-highlighting ',name))

       ',name)))

(defun Struct:-construct-property-entry (declaration)
  (unless (consp declaration)
    (error "Property declaration should be a non-empty list: %s" declaration))
  (-let* ((((positional &as name documentation)
            properties)
           (Commons:split-property-list-end declaration))
          (positional-properties
           (cl-case (length positional)
             (0 nil)
             (1 (list :name name))
             (2 (list :name name :documentation documentation))
             (t (error "Invalid property declaration: %s" declaration)))))
    (cons (Commons:symbol-to-keyword name)
          (apply #'Struct:Property (append positional-properties
                                           properties)))))

(defun Struct:-add-syntax-highlighting (name)
  (let ((keywords `((,(format "\\_<%s\\*?\\_>"
                              (regexp-quote (symbol-name name)))
                     0 'font-lock-type-face))))
    (put name Struct:syntax-highlight-symbol keywords)
    (font-lock-add-keywords 'emacs-lisp-mode keywords)))

(defun Struct:-remove-syntax-highlighting (name)
  (when-let (keywords (get name Struct:syntax-highlight-symbol))
    (font-lock-remove-keywords 'emacs-lisp-mode keywords)
    (put name Struct:syntax-highlight-symbol nil)))

(defun Struct:undefine (name)
  "Undefine struct NAME.

This function undoes the side-effects of a corresponding
`Struct:define' invocation.

It does nothing, if NAME does not name a defined struct-type."
  (when (Struct:Type:get name)
    (Struct:Type:undefine name)
    (Struct:-remove-syntax-highlighting name)
    (put name 'cl-deftype-handler nil)
    (fmakunbound name)
    (fmakunbound (intern (concat (symbol-name name) "*")))
    (fmakunbound (intern (concat (symbol-name name) "?")))))

(provide 'Struct)
;;; Struct.el ends here
