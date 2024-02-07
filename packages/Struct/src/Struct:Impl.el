;; -*- lexical-binding: t -*-

(require 'Struct)
(require 'cl-lib)
(eval-when-compile (require 'cl-macs))
(eval-and-compile (require 'dash))

(defmacro Struct:Impl:define-function (struct-name name arguments &rest body)
  "Defines an associated function NAME for the struct-name named STRUCT-NAME.

This adds NAME to the function property of the type-definition of
the struct and updates the documentation of its type-constructor,
such that it will contain a reference to this function.

Otherwise this behaves like `defun', which see."
  (declare (indent 2) (doc-string 3))
  (let ((struct-type (make-symbol "struct-type"))
        (documentation (if (stringp (car-safe body)) (pop body)))
        (declare (if (eq 'declare (car-safe body)) (pop body))))
    `(let ((,struct-type (Struct:Type:get ',struct-name)))
       (defun ,name ,arguments ,documentation ,declare ,@body)
       (Struct:Impl:remove-definition ,struct-type :methods ',name)
       (Struct:Impl:add-definition ,struct-type :functions ',name)
       (Struct:update-documentation ',struct-name))))

(defmacro Struct:Impl:define-method (struct-name name arguments &rest body)
  "Defines a method NAME for the struct-name named STRUCT-NAME.

A method always accepts at least one argument, which must be
non-nil and of type STRUCT-NAME.  This condition is asserted by
adding a corresponding test to the beginning of BODY.

This adds NAME to the method property of the type-definition of
the struct and updates the documentation of its type-constructor,
such that it will contain a reference to this method.

Otherwise this behaves like `defun', which see."
  (declare (indent 2))
  (let* ((struct-type (make-symbol "struct-type"))
         (documentation (if (stringp (car-safe body)) (pop body)))
         (declare (if (eq 'declare (car-safe (car-safe body))) (pop body)))
         (self-argument (car-safe arguments))
         (type-predicate (intern (format "%s?" struct-name)))
         (type-predicate-form
          `(or (,type-predicate ,self-argument)
               (signal 'wrong-type-argument (list ',struct-name ,self-argument))))
         (body (cons type-predicate-form body)))
    (unless (and self-argument
                 (symbolp self-argument)
                 (not (memq self-argument '(&optional &rest))))
      (error "A method must have at least one non-optional self-argument"))
    `(let ((,struct-type (Struct:Type:get ',struct-name)))
       (defun ,name ,arguments ,documentation ,declare ,@body)
       (Struct:Impl:remove-definition ,struct-type :functions ',name)
       (Struct:Impl:add-definition ,struct-type :methods ',name)
       (Struct:update-documentation ',struct-name))))

(defun Struct:Impl:add-definition (struct-type definition-property definition-name)
  (unless (memq definition-name (Struct:get struct-type definition-property))
    (Struct:update- struct-type definition-property (push name it))))

(defun Struct:Impl:remove-definition (struct-type definition-property definition-name)
  (when (memq definition-name (Struct:get struct-type definition-property))
    (Struct:update- struct-type definition-property (remq name it))))

(defmacro Struct:Impl:implement (type &rest definitions)
  (declare (indent 1))
  (--each definitions
    (unless (memq (car-safe it) '(function method))
      (error "Only function and method definitions allowed here: %s")))
  `(progn ,@(--map (cl-ecase (car it)
                     (function `(Struct:Impl:define-function ,type ,@(cdr it)))
                     (method `(Struct:Impl:define-method ,type ,@(cdr it))))
                   definitions)))

(defalias 'Struct:implement 'Struct:Impl:implement)

(Struct:implement TestStruct
  (function TestStruct:get-value ()
            42)

  (method TestStruct:get-foo (self)
    43)

  (function TestStruct:get-value-documented ()
            "Returns a value."
            42)

  (method TestStruct:get-foo-documented (self)
    "Returns foo."
    43))
  
(provide 'Struct:Impl)
;;; Struct:Impl.el ends here
