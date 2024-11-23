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
(require 'Struct/Argument)
(require 'Commons)

(defconst Struct:Function:arrow-symbol '->)

(defconst Struct:Function:fn-symbol 'fn)

(defconst Struct:Function:namespace-separator ':)

(defconst Struct:Function:self-symbol 'self
  "The name of the dispatch argument of methods.")

(cl-deftype List (type)
  `(and list
        (satisfies ,(lambda (list)
                      (and (or (null list)
                               (cl-typep (car list) type))
                           (or (null (cdr list))
                               (cl-typep (cadr list) type))
                           (or (null (cddr list))
                               (cl-typep (caddr list) type)))))))

(Struct:define Struct:Function
  "Defines a declared function."
  (name
   "The name of the function."
   :type symbol)
  (qualified-name
   "The qualified name of the function.

This is the name under which the function is exported into the global
namespace."
   :type symbol)
  (arguments
   "The list of declared arguments of this function."
   :type list)
  (return-type
   "The return-type of this function."
   :type nil)
  (documentation
   "The documentation for this function."
   :type (or null string))
  (body
   "A list of forms defining this function."
   :type list :mutable t)
  (filename
   "The filename in which this function was declared."
   :default (Commons:evaluation-context-filename)
   :type (or null string)))

(defun Struct:Function:declared-in (struct filename)
  (let ((function-filename (Struct:get struct :filename)))
    (cond
     ((or (null function-filename)
          (null filename))
      (eq function-filename filename))
     (t
      (file-equal-p function-filename filename)))))

(defun Struct:Function:equivalent-arguments? (self other)
  (cl-check-type self Struct:Function)
  (cl-check-type other Struct:Function)
  (and (= (length (Struct:get self :arguments))
          (length (Struct:get other :arguments)))
       (-every? (-lambda ((argument . other))
                  (Struct:Argument:equivalent? argument other))
                (-zip-pair (Struct:get self :arguments)
                           (Struct:get other :arguments)))))

(defun Struct:Function:arity (self)
  "Calculates the number of accepted arguments of this function.

Returns a cons \(MIN . MAX\) denoting the minimally and maximally
accepted number of arguments. MAX may be `most-positive-fixnum', if a
`&rest' argument is present."
  (let* ((arguments (Struct:get self :arguments))
         (min (or (--find-index (Struct:get it :kind) arguments)
                  (length arguments)))
         (max (if (and arguments
                       (eq (Struct:get (car (last arguments)) :kind)
                           '&rest))
                  most-positive-fixnum
                (length arguments))))
    (cons min max)))

(defun Struct:Function:read (form &optional namespace)
  (declare (indent 0))
  (cl-check-type form cons)
  (cl-check-type namespace symbol)
  (-let (((fn name arguments documentation . body) form)
         (separator Struct:Function:namespace-separator))
    (unless (eq fn Struct:Function:fn-symbol)
      (error "Function declaration should start with %s: %s"
             Struct:Function:fn-symbol
             form))
    (unless (symbolp name)
      (error "Function name should be a symbol: %s" name))
    (unless (or (stringp documentation)
                (and (null documentation)
                     (<= (length form) 3)))
      (push documentation body)
      (setq documentation nil))
    (when (eq 'declare (car-safe (car body)))
      (error "Declare form not supported: %s" (car body)))

    (-let* (((arguments . return-type)
             (Struct:Function:read-arguments arguments))
            (qualified-name
             (if namespace
                 (intern (format "%s%s%s" namespace separator name))
               name)))
      (Struct:Function* name qualified-name arguments
                        return-type documentation body))))

(defun Struct:Function:read-arguments (form)
  "Reads a complete argument-list from FORM.

Returns a cons of (ARGUMENTS . RETURN_TYPE)."
  (cl-check-type form list)
  (let ((kind nil)
        (kinds nil)
        (arguments nil)
        (return-type nil))
    (while form
      (let ((argument (pop form)))
        (pcase argument
          ((guard (memq argument kinds))
           (error "Invalid arguments: %s provided multiple times" argument))
          ((or `&optional `&rest)
           (when (and kind (eq argument '&optional))
             (error "Invalid arguments: &optional may not succeed %s" kind))
           (unless (and form
                        (not (memq (car form)
                                   '(&optional &rest))))
             (error "Specifier is missing an argument: %s" argument))
           (when (and (eq argument '&rest)
                      (cdr form)
                      (not (eq (cadr form) Struct:Function:arrow-symbol)))
             (error "Extra argument after &rest provided: %s" form))
           (setq kind argument)
           (push argument kinds))
          ((guard (eq argument Struct:Function:arrow-symbol))
           (unless form
             (error "Function-arrow requires an argument"))
           (when (cdr form)
             (error "Return-type should be the final element: %s" form))
           (setq return-type (pop form)))
          (argument
           (push (Struct:Argument:read argument kind) arguments)))))
    (cons (nreverse arguments) return-type)))

(defun Struct:Function:emit-declaration (self)
  `(declare-function
    ,(Struct:get self :qualified-name)
    ,(Struct:get self :filename)
    ,(Struct:Function:emit-arguments self)))

(defun Struct:Function:emit-definition (self &optional transformer flush?)
  `(defalias ',(Struct:get self :qualified-name)
     ,(Struct:Function:emit-lambda self transformer flush?)
     ,(Struct:get self :documentation)))

(defun Struct:Function:emit-lambda (self &optional transformer flush?)
  `(lambda ,(Struct:Function:emit-arguments self)
     ,@(when-let (documentation (Struct:get self :documentation))
         (list documentation))
     ,@(Struct:Function:emit-body self transformer flush?)))

(defun Struct:Function:emit-arguments (self)
  (let ((previous-kind nil)
        (result nil)
        (arguments (Struct:get self :arguments)))
    (while arguments
      (let ((argument (pop arguments)))
        (-let ((kind (Struct:get argument :kind)))
          (when (and kind (not (eq kind previous-kind)))
            (push kind result)
            (setq previous-kind kind))
          (push (Struct:get argument :name)
                result))))
    (nreverse result)))

(defun Struct:Function:emit-body (self &optional transformer flush?)
  (let ((body (append (Struct:Function:emit-body-preamble self)
                      (if transformer
                          (funcall transformer self)
                        (Struct:get self :body)))))
    (when flush?
      (Struct:set self :body nil))
    body))

(defun Struct:Function:emit-body-preamble (self)
  (let* ((arguments (--remove (string-prefix-p "_" (symbol-name (Struct:get it :name)))
                              (Struct:get self :arguments)))
         (annotated (--filter (Struct:get it :type) arguments)))
    (append (--map `(cl-check-type ,(Struct:get it :name)
                                   ,(Struct:get it :type))
                   (-filter #'Struct:Argument:regular? annotated))
            (--map `(cl-check-type ,(Struct:get it :name)
                                   (or null ,(Struct:get it :type)))
                   (-filter #'Struct:Argument:optional? annotated))
            (--map `(cl-check-type ,(Struct:get it :name)
                                   (List ,(Struct:get it :type)))
                   (-filter #'Struct:Argument:rest? annotated))
            (--map `(or ,(Struct:get it :name)
                        (setq ,(Struct:get it :name)
                              ,(Struct:get it :default)))
                   (-filter #'Struct:Argument:default? arguments)))))

(defun Struct:Function:type (fn &optional as-method?)
  (let* ((argument-types (--map
                          (or (Struct:get it :type) 'Any)
                          (Struct:get fn :arguments)))
         (arguments (--map
                     (cond
                      ((memq it '(&rest &optional)) it)
                      (argument-types (pop argument-types))
                      (t (error "internal error")))
                     (Struct:Function:emit-arguments fn)))
         (return-type (or (Struct:get fn :return-type) 'Any)))
    (when (and as-method? (not (Struct:Function:method? fn)))
      (error "Function is not a method: %s" fn))
    `(-> ,(if as-method? (cdr arguments) arguments) ,return-type)))

(defun Struct:Function:method? (fn)
  (-some-> (car (Struct:get fn :arguments))
    (Struct:get :name)
    (eq Struct:Function:self-symbol)))

(provide 'Struct/Function)
