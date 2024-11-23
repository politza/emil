;; -*- lexical-binding: t -*-

(require 'Struct/Function)

(defmacro Struct:lambda (arguments &rest body)
  "Defines a function with some struct related features."
  (declare (indent 1))
  (Struct:Function:emit-lambda
   (Struct:Function:read 
     `(defmethod ,arguments ,@body))))

(defmacro Struct:defun (name arguments
                             &optional documentation 
                             &rest body)
  "Defines a function NAME with some struct related features."
  (Struct:Function:emit-definition
   (Struct:Function:read 
     `(defmethod ,name ,arguments ,documentation ,@body))))

(provide 'Struct/Impl)
