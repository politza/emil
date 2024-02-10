;; -*- lexical-binding: t -*-

(require 'Struct)
(eval-and-compile
  (require 'dash))

(defconst Trait:definition-symbol 'Trait:definition-symbol
  "Ths symbol by which to associate traits with their name.

The trait-definition is put on the symbol property-list using this value.")

(Struct:define Trait
  "Represents a trait-type."
  (name
    "The name of this trait."
   :required t :read-only t :type symbol)
  (methods
    "An association-list mapping method-names to their definitions."
   :type list)
  (implementing-types
    "A list of types implementing this trait."
   :type list)
  (supertraits
    "A list of required traits for implementing this trait."
   :type list))

(defun Trait:get (name &optional ensure)
  "Returns the `Trait' definition of NAME.

Returns nil, if NAME does not name a trait; unless ENSURE is non-nil, in
which case a `wrong-type-argument' is signaled."
  (or (get name Trait:definition-symbol)
      (and ensure
           (signal 'wrong-type-argument (list 'Trait name)))))

(Struct:define Trait:Method
  "Represents a trait-method."
  (name
  "The name of this method."
   :required t :read-only t :type symbol)
  (arguments
  "The declared argument list of this method."
   :read-only t :type list)
  (documentation
  "A string describing this method."
   :read-only t :type (or string null))
  (default-implementation
   "An optional default implementation of this method. If not
provided, this method is required for implementors to implement."
   :read-only t :type (or function null))
  (implementations
   "An alist mapping types to their implementation of this method."
   :type list)
  (dispatch-function
   "A function responsible for dispatching this method."
   :required t :read-only t :type function))

(Struct:defmethod Trait:Method:required? ((method Trait:Method))
  "Returns non-nil, if implementing METHOD is required.

This is the case, if METHOD does not define a default implementation."
  (null (Struct:get method :default-implementation)))

(Struct:defmethod Trait:Method:arity ((method Trait:Method))
  "Returns the number of accepted arguments of METHOD.

The value is a pair `\(MIN . MAX\)'. See also `func-arity'."
  (func-arity `(lambda ,(Struct:get method :arguments))))

(defmacro Trait:define (name supertraits &optional documentation &rest methods)
  "Defines a new trait named NAME.

(fn NAME ([SUPERTRAIT]*) [DOCUMENTATION]? [(defmethod METHOD-NAME ARGUMENTS [DOCUMENTATION]? . BODY)]*)"
  (declare (indent 2) (doc-string 3))
  (unless (symbolp name)
    (signal 'wrong-type-argument `(symbol ,name)))
  (unless (and (listp supertraits)
               (-every? #'symbolp supertraits))
    (signal 'wrong-type-argument `((list symbol) ,supertraits)))
  (unless (or (null documentation)
              (stringp documentation))
    (push documentation methods)
    (setq documentation nil))

  `(Trait:define*
    (Trait :name ',name
           :supertraits (copy-sequence ',supertraits)
           :methods
           (list ,@(--map (Trait:-construct-method name it)
                          methods)))))

(defun Trait:-construct-method (trait method)
  (unless (consp method)
    (error "Method definition should be a non-empty list: %s" method))
  (-let (((head name arguments documentation . body) method))
    (unless (eq 'defmethod head)
      (error "Method declaration should start with defmethod: %s" head))
    (unless (symbolp name)
      (error "Method name should be a symbol: %s" name))
    (unless (and (listp arguments)
                 (-every? #'symbolp arguments))
      (error "Invalid method arguments declaration: %s" arguments))
    (unless (consp arguments)
      (error "Trait method must accept at least one argument: %s" name))
    (unless (or (stringp documentation)
                (null documentation))
      (push documentation body)
      (setq documentation nil))
    (when (eq 'declare (car-safe (car body)))
      (error "Declare not supported for methods"))
    
    `(cons ',name
           (Trait:Method
            :name ',name
            :documentation ,documentation
            :default-implementation
            ,(and body
                  `(lambda ,arguments ,@body))
            :arguments (copy-sequence ',arguments)
            :dispatch-function
            (lambda (&rest arguments)
              (Trait:dispatch ',trait ',name arguments))))))

(defun Trait:define* (trait)
  "Defines a new trait according to the given definition."
  (-let (((&plist :name :methods :supertraits)
          (Struct:properties trait)))
    (Trait:undefine name)
    (--each supertraits (Trait:get it :ensure))  
    (--each methods
      (defalias (car it)
        (Struct:get (cdr it) :dispatch-function))
      (put (car it) 'function-documentation
           `(Trait:Method:documentation ',name ',(car it))))
    (put name Trait:definition-symbol trait)
    (put name 'function-documentation
         `(Trait:documentation ',name))
    name))

(defun Trait:undefine (name)
  "Undefines the trait named NAME.

This attempts to undo all effects of `Trait:define'.  This function is
idempotent."
  (when-let (trait (Trait:get name))
    (--each (Struct:get trait :methods)
      (fmakunbound (car it))
      (put (car it) 'function-documentation nil))
    (put name 'function-documentation nil)
    (put name Trait:definition-symbol nil)))

(defun Trait:documentation (name)
  "Return a documentation-string for trait named NAME."
  (-> (Trait:get name :ensure)
      (Struct:get :documentation)))

(defun Trait:Method:documentation (trait name)
  "Return a documentation-string for TRAIT's method named NAME."
  (-> (cdr (assq name (->
                       (Trait:get trait :ensure)
                       (Struct:get :methods))))
      (Struct:get :documentation)))

(defmacro Trait:implement (trait type &rest methods)
  "Defines an implementation of TRAIT for TYPE.

(fn TRAIT TYPE [(defmethod METHOD-NAME ARGUMENTS . BODY)]*)"
  (declare (indent 2))
  (unless (symbolp trait)
    (signal 'wrong-type-argument `(symbol ,trait)))
  (unless (symbolp type)
    (signal 'wrong-type-argument `(symbol ,type)))

  `(Trait:implement*
    ',trait ',type
    (list ,@(-map #'Trait:-construct-implementation methods))))

(defun Trait:-construct-implementation (method)
  (unless (consp method)
    (error "Expected a non-empty list: %s" method))
  (-let (((head name arguments . body) method))
    (unless (eq 'defmethod head)
      (error "Method implementation should start with defmethod: %s" head))
    (unless (symbolp name)
      (error "Method name should be a symbol: %s" name))
    (unless (and (listp arguments)
                 (-every? #'symbolp arguments))
      (error "Invalid method argument-list declaration: %s" arguments))
    (when (eq 'declare (car-safe (car body)))
      (error "Declare not supported for methods"))
    
    `(cons ',name (lambda ,arguments ,@body))))

(defun Trait:implement* (trait type implementations)
  "Defines a implementation of TRAIT for TYPE."
  (-let* ((trait-struct (Trait:get trait :ensure))
          ((&plist :supertraits :methods) (Struct:properties trait-struct)))
    (--each supertraits
      (unless (memq type (Struct:get (Trait:get it :ensure)
                                     :implementing-types))
        (error "Required supertrait not implemented by type: %s" it)))
    (--each methods
      (unless (or (assq (car it) implementations)
                  (not (Trait:Method:required? (cdr it))))
        (error "Required method not implemented: %s" (car it))))
    (--each implementations
      (unless (assq (car it) methods)
        (error "Method not declared by this trait: %s" (car it))))
    (--each methods
      (when-let (impl (cdr (assq (car it) implementations)))
        (unless (equal (Trait:Method:arity (cdr it))
                       (func-arity impl))
          (error "Signature not compatible with method declared by trait: %s, %s, %s"
                 (car it)
                 (Trait:Method:arity (cdr it))
                 (func-arity impl)))))
    (Struct:update trait-struct :implementing-types (-partial #'cons type))
    (--each methods
      (when-let (impl (cdr (assq (car it) implementations)))
        (Struct:update (cdr it) :implementations
                       (lambda (value) (cons (cons type impl) value)))))
    trait))

(defun Trait:dispatch (trait method arguments)
  "Dispatch call of TRAIT's METHOD using ARGUMENTS."
  (let (type trait-struct method-struct impl)
    (unless (consp arguments)
      (signal 'wrong-number-of-arguments (list 0)))
    (setq type (Trait:type-of (car arguments)))
    (unless (setq trait-struct (Trait:get trait))
      (error "Trait not defined: %s" trait))
    (unless (setq method-struct
                  (cdr (assq method (Struct:unsafe-get trait-struct :methods))))
      (error "Trait method not defined: %s, %s" trait method))
    (unless (setq impl (or (cdr (assq type
                                      (Struct:unsafe-get method-struct :implementations)))
                           (Struct:unsafe-get method-struct :default-implementation)))
      (unless (memq type (Struct:unsafe-get trait-struct :implementing-types))
        (error "Type does not implement trait: %s, %s" type trait))
      (error "Required method not implemented by type: %s, %s" method type))
    (apply impl arguments)))

(defun Trait:type-of (value)
  "Returns the type of VALUE.

If VALUE is a non-empty list with a symbol as first element,
returns that symbol as the type of VALUE.

Otherwise, calls `type-of' on VALUE and return its result."
  (or (and (consp value)
           (symbolp (car value))
           (car value))
      (type-of value)))

(provide 'Trait)
;;; Trait.el ends here
