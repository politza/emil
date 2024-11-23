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

This may be one of `&optional', `&rest', `&struct' for optional, rest
and struct arguments; or nil for regular ones."
   :type (member &optional &rest &struct nil)))

(defun Struct:Argument:read (form &optional kind)
  "Reads an argument from FORM.

KIND should be one of `&optional', '&rest', `&struct' or nil and
specifies the kind of the read argument."
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

(provide 'Struct/Argument)
