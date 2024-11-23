;; -*- lexical-binding: t -*-

(require 'Struct)

(Struct:define Struct:Argument
  "Defines an argument of a `Struct:Function'."
  (name
   "The name of the argument."
   :type symbol)
  (type
   "The type of the argument."
   :type nil)
  (default
   "The default value of the argument.

It is actually a form, which will be evaluated in the context of
previous arguments."
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

(defun Struct:Argument:read-list (form)
  "Reads a complete argument-list from FORM."
  (cl-check-type form list)
  (let ((kind nil)
        (kinds nil)
        (arguments nil))
    (while form
      (let ((argument (pop form)))
        (pcase argument
          ((guard (memq argument kinds))
           (error "Invalid arguments: %s provided multiple times" argument))
          ((or `&optional `&rest `&struct)
           (when (and kind (eq argument '&optional))
             (error "Invalid arguments: &optional may not succeed %s" kind))
           (when (memq kind '(&rest &struct))
             (error "Invalid arguments: &rest and &struct are mutually exclusive"))
           (unless (and arguments
                        (not (memq (car arguments)
                                   '(&optional &rest &struct))))
             (error "Specifier is missing an argument: %s" argument))
           (setq kind argument)
           (push kind kinds))
          (argument
           (push (Struct:Argument:read argument kind) arguments)))))
    (nreverse arguments)))

(defun Struct:Argument:normalform (arguments)
  (let ((previous-kind nil)
        (result nil))
    (while arguments
      (-let (((&plist :name :kind)
              (Struct:properties (pop arguments))))
        (when (and kind (not (eq kind previous-kind)))
          (push kind result)
          (setq previous-kind kind))
        (push name result)))
    (nreverse result)))

(provide 'Struct/Argument)
