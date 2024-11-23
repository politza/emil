;; -*- lexical-binding: t -*-

(require 'pcase)
(require 'dash)
(require 'Struct/Primitives)

(pcase-defmacro Struct (name &rest properties)
  "Match against struct values.

Binds named properties to their corresponding values."
  (let ((properties (Struct:Pcase:-normalize-properties properties)))
    (--each properties
      (unless (Struct:Type:get-property
               (Struct:Type:get name :ensure)
               (car it))
        (error "Property is not a member of struct %s: %s" name (car it))))
    `(and (pred (lambda (value)
                  (and (consp value)
                       (eq ',name (car value)))))
	  ,@(mapcar (-lambda ((property . symbol))
                      `(app (pcase--flip Struct:unsafe-get ,property) ,symbol))
                    properties))))

(defun Struct:Pcase:-normalize-properties (properties)
  (let ((normalized nil))
    (while properties
      (let ((property (pop properties)))
        (cond
         ((and (symbolp property)
               (not (keywordp property)))
          (push (cons (Commons:symbol-to-keyword property) property)
                normalized))
         ((and (keywordp property)
               properties
               (symbolp (car properties))
               (not (keywordp (car properties))))
          (push (cons property (pop properties))
                normalized))
         ((keywordp property)
          (error "Property should be followed by a symbol to bind to: %s" property))
         (t
          (error "Property should be a symbol: %s" property)))))
    normalized))

(provide 'Struct/Pcase)
