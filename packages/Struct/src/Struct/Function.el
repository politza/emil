;; -*- lexical-binding: t -*-

(require 'Struct)
(require 'Struct/Argument)

(defconst Struct:Function:arrow-symbol '->)

(defconst Struct:Function:fn-symbol 'fn)

(defconst Struct:Function:namespace-separator ':)

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
   :type list))

(defun Struct:Function:read (namespace form &optional namespace-separator)
  (declare (indent 1))
  (cl-check-type form cons)
  (cl-check-type namespace symbol)
  (unless namespace-separator
    (setq namespace-separator Struct:Function:namespace-separator))
  (-let (((fn name arguments documentation . body) form))
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

    (-let* (((arguments . return-type)
             (Struct:Function:-read-argument-list arguments))
            (qualified-name
             (intern (format "%s%s%s" namespace namespace-separator name))))
      (Struct:Function* name qualified-name arguments
                        return-type documentation body))))

(defun Struct:Function:-read-argument-list (form)
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
          ((or `&optional `&rest `&struct)
           (when (and kind (eq argument '&optional))
             (error "Invalid arguments: &optional may not succeed %s" kind))
           (when (memq kind '(&rest &struct))
             (error "Invalid arguments: &rest and &struct are mutually exclusive"))
           (unless (and form
                        (not (memq (car form)
                                   '(&optional &rest &struct))))
             (error "Specifier is missing an argument: %s" argument))
           (setq kind (if (eq argument '&struct) '&rest argument))
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

(defun Struct:Function:lambda-arguments (self)
  (let ((previous-kind nil)
        (result nil)
        (arguments (Struct:get self :arguments)))
    (while arguments
      (-let (((&plist :name :kind)
              (Struct:properties (pop arguments))))
        (when (and kind (not (eq kind previous-kind)))
          (push kind result)
          (setq previous-kind kind))
        (push name result)))
    (nreverse result)))

(provide 'Struct/Function)
