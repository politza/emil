;; -*- lexical-binding: t -*-

(require 'Struct)

(Struct:define Struct:Argument
  "Defines an argument of a `Struct:FunctionDeclaration'."
  (name
   "The name of this argument."
   :type symbol)
  (type
   "The type of this argument."
   :type nil)
  (default
   "The default value of this argument."
   :type nil)
  (kind
   "The kind of this argument.

This may be one of `&optional' and `&rest' for optional and rest
arguments; or nil for regular ones."
   :type (member &optional &rest nil)))

(defun Struct:Argument:equivalent? (argument other)
  (cl-check-type argument Struct:Argument)
  (cl-check-type other Struct:Argument)
  (and (equal (Struct:get argument :type)
              (Struct:get other :type))
       (eq (Struct:get argument :kind)
           (Struct:get other :kind))))

(defun Struct:Argument:read (form &optional kind)
  "Reads an argument from FORM.

KIND should be one of `&optional', '&rest' or nil and specifies the
kind of the read argument."
  (when (symbolp form)
    (setq form (list form)))
  (unless (and (consp form)
               (<= (length form) 3)
               (symbolp (car form)))
    (error "Form is not a valid argument: %s" form))
  
  (Struct:Argument
   :name (pop form)
   :type (pop form)
   :default (pop form)
   :kind kind))

(defun Struct:Argument:optional? (self)
  (eq '&optional (Struct:get self :kind)))

(defun Struct:Argument:rest? (self)
  (eq '&rest (Struct:get self :kind)))

(defun Struct:Argument:regular? (self)
  (null (Struct:get self :kind)))

(defun Struct:Argument:default? (self)
  (not (null (Struct:get self :default))))

(provide 'Struct/Argument)
