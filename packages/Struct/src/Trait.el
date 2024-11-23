;; -*- lexical-binding: t -*-

(require 'Struct)
(require 'Struct/Impl)
(require 'Struct/Function)
(require 'dash)
(require 'cl-macs)

(defconst Trait:definition-symbol 'Trait:definition-symbol
  "Ths symbol by which to associate traits with their name.

The trait-definition is put on the symbol's property-list using this value.")

(Struct:define Trait
  "Represents a trait-type."
  (name
   "The name of this trait."
   :type symbol)
  (methods
   "An association-list mapping method-names to their definition."
   :type list)
  (implementing-types
   "A list of types implementing this trait."
   :type list :mutable t)
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
  (function
   "The function-declaration of this method."
   :type Struct:Function)
  (default-implementation
   "An optional default implementation of this method. If not
provided, this method is required for implementors to implement."
   :type (or null function))
  (implementations
   "An alist mapping types to their implementation of this method."
   :type list :mutable t)
  (dispatch-function
   "A function responsible for dispatching this method."
   :type function))

(defun Trait:Method:required? (method)
  "Returns non-nil, if implementing METHOD is required.

This is the case, if METHOD does not define a default implementation."
  (null (Struct:get method :default-implementation)))

(defmacro Trait:define (name supertraits &optional documentation &rest body)
  "Defines a new trait named NAME.

(fn NAME ([SUPERTRAIT]*) [DOCUMENTATION]? [(fn METHOD-NAME ARGUMENTS [DOCUMENTATION]? . BODY)]*)"
  (declare (indent 2) (doc-string 3))
  (unless (symbolp name)
    (signal 'wrong-type-argument `(symbol ,name)))
  (unless (and (listp supertraits)
               (-every? #'symbolp supertraits))
    (signal 'wrong-type-argument `((list symbol) ,supertraits)))
  (unless (or (null documentation)
              (stringp documentation))
    (push documentation body)
    (setq documentation nil))

  (let ((methods (-map #'Struct:Function:read body)))
  `(eval-and-compile
     ,@(--map (Struct:Function:emit-declaration it) methods)
     (Trait:define*
      (Trait :name ',name
             :supertraits (copy-sequence ',supertraits)
             :methods
             (list ,@(--map (Trait:-construct-method-definition name it) methods)))))))

(defun Trait:-construct-method-definition (trait function)
  (let ((name (Struct:get function :qualified-name))
        (arguments (Struct:get function :arguments))
        (body? (not (null (Struct:get function :body)))))
    (unless arguments
      (error "A method requires at least one argument: %s" name))
    (when (Struct:get (car arguments) :type)
      (error "Self argument of method can not be typed: %s" name))
    (when (Struct:get (car arguments) :default)
      (error "Self argument of method can not have a default: %s" name))
    `(cons ',name
           (Trait:Method
            :function (copy-sequence ',function)
            :default-implementation
            ,(and body?
                  (Struct:Function:emit-lambda function))
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
      ;; (put (car it) 'function-documentation
      ;;      `(Trait:Method:documentation ',name ',(car it)))
      )
    (put name Trait:definition-symbol trait)
    name))

(defun Trait:undefine (name)
  "Undefines the trait named NAME.

This attempts to undo all effects of `Trait:define'.  This function is
idempotent."
  (when-let (trait (Trait:get name))
    (--each (Struct:get trait :methods)
      (fmakunbound (car it))
      (put (car it) 'function-documentation nil))
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
      (Struct:get :declaration)
      (Struct:get :documentation)))

(defmacro Trait:implement (trait type &rest body)
  "Defines an implementation of TRAIT for TYPE.

(fn TRAIT TYPE [(fn METHOD-NAME ARGUMENTS . BODY)]*)"
  (declare (indent 2))
  (unless (symbolp trait)
    (signal 'wrong-type-argument `(symbol ,trait)))
  (unless (symbolp type)
    (signal 'wrong-type-argument `(symbol ,type)))
  (let ((functions (-map #'Struct:Function:read body)))
    `(eval-and-compile
       (Trait:unimplement ',trait ',type)
       (Trait:-check-implementation ',trait ',type (copy-sequence ',functions))   
       (Trait:implement*
        ',trait ',type
        (list ,@(-map #'Trait:-construct-method-impl functions))))))

(defun Trait:-check-implementation (trait type functions)
  (-let* ((entries (--annotate (Struct:get it :qualified-name) functions))
          (struct (Trait:get trait :ensure))
          (supertraits (Struct:get struct :supertraits))
          (methods (Struct:get struct :methods)))
    (--each supertraits
      (unless (memq type (Struct:get (Trait:get it :ensure) :implementing-types))
        (error "Required supertrait not implemented by type: %s" it)))
    (--each methods
      (unless (or (assq (car it) entries)
                  (not (Trait:Method:required? (cdr it))))
        (error "Required method not implemented: %s" (car it))))
    (--each entries
      (unless (assq (car it) methods)
        (error "Method not declared by this trait: %s" (car it))))
    (--each methods
      (when-let (entry (assq (car it) entries))
        (unless (Struct:Function:equivalent-arguments?
                 (cdr entry) (Struct:get (cdr it) :function))
          (error "Signature incompatible with method declared by trait: %s, %s"
                 trait (car it)))))))

(defun Trait:-construct-method-impl (function)
  `(cons ',(Struct:get function :qualified-name)
        ,(Struct:Function:emit-lambda function)))

(defun Trait:implement* (trait type functions)
  "Defines an implementation of TRAIT for TYPE."
  (let ((struct (Trait:get trait :ensure)))
    (Struct:update struct :implementing-types (-partial #'cons type))
    (--each (Struct:get struct :methods)
      (when-let (entry (assq (car it) functions))
        (Struct:update (cdr it) :implementations
          (-partial #'cons (cons type (cdr entry))))))
    type))

(defun Trait:unimplement (trait type)
  (when-let (struct (Trait:get trait))
    (Struct:update struct :implementing-types (-partial #'remove type))
    (--each (Struct:get struct :methods)
      (Struct:update (cdr it) :implementations
        (lambda (implementations)
          (remove (assq type implementations) implementations))))
    nil))

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
    (unless (setq impl (or (cdr (assq type (Struct:unsafe-get
                                            method-struct :implementations)))
                           (Struct:unsafe-get
                            method-struct :default-implementation)))
      (unless (memq type (Struct:unsafe-get trait-struct :implementing-types))
        (error "Type does not implement trait: %s, %s" type trait))
      (error "Required method not implemented by type: %s, %s" method type))
    (apply impl arguments)))

(defun Trait:implements? (type trait)
  "Return non-nil, if type TYPE implements trait TRAIT.

TYPE and TRAIT should both be symbols.

Signals a `wrong-type-argument', if TRAIT is not a defined trait."
  (memq type (Struct:get (Trait:get trait :ensure) :implementing-types)))

(cl-deftype Trait (&rest traits)
  `(satisfies ,(lambda (value)
                 (let ((type (Trait:type-of value)))
                   (cl-every (lambda (trait)
                               (Trait:implements? type trait))
                             traits)))))

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
