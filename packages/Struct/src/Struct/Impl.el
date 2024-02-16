;; -*- lexical-binding: t -*-

(defmacro Struct:lambda (arguments &rest body)
  "Defines a function with some struct related features."
  (declare (indent 1))
  (Struct:lambda-check-arguments arguments)

  `(lambda ,(Struct:lambda-normalize-arguments arguments)
     ,@(Struct:-lambda-emit-type-checks arguments)
     ,(Struct:-lambda-emit-handle-struct-rest arguments)
     ,@body))

(defmacro Struct:defun (name arguments
                             &optional documentation declare
                             &rest body)
  "Defines a function NAME with some struct related features."
  (declare (indent defun) (doc-string 3))
  (unless (or (stringp documentation)
              (null documentation))
    (push documentation body)
    (setq documentation nil))
  (unless (or (eq (car-safe declare) 'declare)
              (null declare))
    (push declare body)
    (setq declare nil))
  (Struct:lambda-check-arguments arguments)

  `(defun ,name ,(Struct:lambda-normalize-arguments arguments)
     ,(format "%s\n\n%s" (or documentation "") (cons 'fn arguments))
     ,declare
     ,@(Struct:-lambda-emit-type-checks arguments)
     ,(Struct:-lambda-emit-handle-struct-rest arguments)
     ,@body))

(defun Struct:lambda-check-arguments (arguments)
  "Check that arguments can be used with `Struct:lambda'.

Throws an error, if ARGUMENTS are incompatible with that macro."
  (when (and (memq '&rest arguments)
             (memq '&struct arguments))
    (error "&rest and &struct are mutually exclusive: %s" arguments))
  (when-let (struct-rest (memq '&struct arguments))
    (pcase (length struct-rest)
      (1 (error "&struct is missing an argument: %s" arguments))
      (2 nil)
      (_ (error "&struct argument must be last: %s" arguments)))
    (let ((argument (cadr struct-rest)))
      (unless (and (consp argument)
                   (= (length argument))
                   (-every? #'symbolp argument))
        (error "&struct argument should specify a struct-type: %s"
               arguments))))
  (when-let (rest-rest (memq '&rest arguments))
    (pcase (length rest-rest)
      (1 (error "&rest is missing an argument: %s" arguments))
      (2 nil)
      (_ (error "&rest argument must be last: %s" arguments)))
    (let ((argument (cadr rest-rest)))
      (when (consp argument)
        (error "Providing a type for &rest arguments is not supported: %s"
               arguments))))
  (--each arguments
    (unless (or (symbolp it)
                (and (consp it)
                     (= (length it) 2)))
      (error "Argument should be a symbol or have the form (argument type): %s"
             arguments))))

(defun Struct:lambda-normalize-arguments (arguments)
  (--map (cond
          ((eq it '&struct) '&rest)
          ((symbolp it) it)
          ((consp it) (car it))
          (t (error "Invalid argument: %s" it)))
         arguments))

(defun Struct:-lambda-emit-type-checks (arguments)
  (when (or (memq '&struct arguments)
            (memq '&rest arguments))
    (setq arguments (butlast arguments 2)))
  (-let (((non-optional (_ . optional))
          (--split-with (not (eq '&optional it)) arguments)))
    (append
     (--map `(cl-check-type ,(car it) ,(cadr it))
            (-filter #'consp non-optional))
     (--map `(cl-check-type ,(car it) (or null ,(cadr it)))
            (-filter #'consp optional)))))

(defun Struct:-lambda-emit-handle-struct-rest (arguments)
  (-when-let ((struct type)
              (cadr (memq '&struct arguments)))
    `(setq ,struct (Struct:-lambda-handle-struct-rest ',type ,struct))))

(defun Struct:-lambda-handle-struct-rest(type rest)
  (if (and (consp rest)
           (= 1 (length rest))
           (eq type (car-safe (car rest))))
      (car rest)
    (Struct:Type:get type :ensure)
    (apply type rest)))

(provide 'Struct/Impl)
