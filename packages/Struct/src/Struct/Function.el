;; -*- lexical-binding: t -*-

(require 'Struct)
(require 'Struct/Argument)
(require 'eieio-core)

(defconst Struct:Function:arrow-symbol '->)

(defconst Struct:Function:fn-symbol 'defmethod)

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
      (error "Declare form not supported: %s" (caar body)))

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
          ((or `&optional `&rest `&struct)
           (when (and kind (eq argument '&optional))
             (error "Invalid arguments: &optional may not succeed %s" kind))
           (when (memq kind '(&rest &struct))
             (error "Invalid arguments: &rest and &struct are mutually exclusive"))
           (unless (and form
                        (not (memq (car form)
                                   '(&optional &rest &struct))))
             (error "Specifier is missing an argument: %s" argument))
           (when (and (memq argument '(&rest &struct))
                      (cdr form))
             (error "Extra argument after &rest or &struct: %s" form))
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

(defun Struct:Function:emit-definition (self &optional transformer)
  `(defalias ',(Struct:get self :qualified-name)
     ,(Struct:Function:emit-lambda self transformer)
     ,(Struct:get self :documentation)))

(defun Struct:Function:emit-lambda (self &optional transformer)
  `(lambda ,(Struct:Function:emit-arguments self)
     ,@(when-let (documentation (Struct:get self :documentation))
         (list documentation))
     ,@(Struct:Function:emit-body self transformer)))

(defun Struct:Function:emit-arguments (self)
  (let ((previous-kind nil)
        (result nil)
        (arguments (Struct:get self :arguments)))
    (while arguments
      (let ((argument (pop arguments)))
        (-let ((kind (Struct:get argument :kind)))
          (when (and kind (not (eq kind previous-kind)))
            (push (if (eq kind '&struct) '&rest kind) result)
            (setq previous-kind kind))
          (push (Struct:get argument :name) result))))
    (nreverse result)))

(defun Struct:Function:emit-body (self &optional transformer)
  (let ((body (append (Struct:Function:emit-body-preamble self)
                      (Struct:get self :body))))
    (if transformer
        (funcall transformer body)
      body)))

(defun Struct:Function:emit-body-preamble (self)
  (let* ((arguments (Struct:get self :arguments))
         (annotated (--filter (Struct:get it :type) arguments)))
    (append (--map `(cl-check-type ,(Struct:get it :name)
                                   ,(Struct:get it :type))
                   (-filter #'Struct:Argument:regular? annotated))
            (--map `(cl-check-type ,(Struct:get it :name)
                                   (or null ,(Struct:get it :type)))
                   (-filter #'Struct:Argument:optional? annotated))
            (--map `(cl-check-type ,(Struct:get it :name)
                                   (list-of ,(Struct:get it :type)))
                   (-filter #'Struct:Argument:rest? annotated))
            (--map `(or ,(Struct:get it :name)
                        (setq ,(Struct:get it :name)
                              ,(Struct:get it :default)))
                   (-filter #'Struct:Argument:default? arguments))
            (--map (prog1
                       `(setq ,(Struct:get it :name)
                          (Struct:Function:-struct-argument-handler
                           ',(Struct:get it :type)
                           ,(Struct:get it :name)))
                     ;; (Struct:Type:get (Struct:get it :type) :ensure)
                     )
                   (-filter #'Struct:Argument:struct? arguments)))))

(defun Struct:Function:-struct-argument-handler (type rest)
  (if (and (consp rest)
           (= 1 (length rest))
           (eq type (car-safe (car rest))))
      (car rest)
    (apply type rest)))

(provide 'Struct/Function)
