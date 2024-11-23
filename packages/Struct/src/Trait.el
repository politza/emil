;; -*- lexical-binding: t -*-

(require 'Struct)
(require 'Struct/Impl)
(require 'Struct/Function)
(require 'dash)
(require 'cl-macs)

(declare-function Emil:Syntax:transform "Emil/Syntax" (function))

(defconst Trait:definition-symbol 'Trait:definition-symbol
  "Ths symbol by which to associate traits with their name.

The trait-definition is put on the symbol's property-list using this value.")

(defconst Trait:implemented-symbol 'Trait:implemented
  "Ths symbol by which to associate a symbol with its implemented traits.

The trait-symbols are put on the symbol's property-list using this value.")

(Struct:define Trait
  "Represents a trait-type."
  (name
   "The name of this trait."
   :type symbol)
  (functions
   "An association-list mapping function-names to their definition."
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

(defun Trait:name? (name)
  "Returns `t', if NAME is the name of a trait."
  (not (null (get name Trait:definition-symbol))))

(Struct:define Trait:Function
  "Represents a trait-function."
  (function
   "The function-declaration of this function."
   :type Struct:Function)
  (default-implementation
   "An optional default implementation of this function. If not
provided, this function is required for implementors to implement."
   :type (or null function))
  (implementations
   "An alist mapping types to their implementation of this function."
   :type list :mutable t)
  (dispatch-function
   "A function responsible for dispatching this function."
   :type function))

(defun Trait:Function:required? (function)
  "Returns non-nil, if implementing FUNCTION is required.

This is the case, if FUNCTION does not define a default implementation."
  (null (Struct:get function :default-implementation)))

(defvar Trait:declared-traits nil)

(defmacro Trait:define (name supertraits &optional documentation &rest properties-and-body)
  "Defines a new trait named NAME.

(fn NAME ([SUPERTRAIT]*) [DOCUMENTATION]? [(fn FUNCTION-NAME ARGUMENTS [DOCUMENTATION]? . BODY)]*)"
  (declare (indent 2) (doc-string 3))
  (unless (symbolp name)
    (signal 'wrong-type-argument `(symbol ,name)))
  (unless (and (listp supertraits)
               (-every? #'symbolp supertraits))
    (signal 'wrong-type-argument `((list symbol) ,supertraits)))
  (unless (or (null documentation)
              (stringp documentation))
    (push documentation properties-and-body)
    (setq documentation nil))

  (-let* (((properties body)
           (Commons:split-property-list properties-and-body))
          (disable-syntax (plist-get properties :disable-syntax))
          (functions (--map (Struct:Function:read it (unless disable-syntax name))
                            body))
          (transformer (unless (or disable-syntax
                                   (not (require 'Emil nil t)))
                         #'Emil:Syntax:transform))
          (Trait:declared-traits
           (cons (list name supertraits functions)
                 Trait:declared-traits)))
  `(eval-and-compile
     ,@(--map (Struct:Function:emit-declaration it) functions)
     (Trait:define*
      (Trait :name ',name
             :supertraits (copy-sequence ',supertraits)
             :functions
             (list ,@(--map (Trait:-construct-function-definition name it transformer)
                            functions)))))))

(defun Trait:-construct-function-definition (trait function transformer)
  (let ((name (Struct:get function :qualified-name))
        (arguments (Struct:get function :arguments))
        (body? (not (null (Struct:get function :body))))
        (trait-type `(Trait ,trait)))
    (unless arguments
      (error "A function requires at least one argument: %s" name))
    (unless (Struct:get (car arguments) :type)
      (Struct:unsafe-set (car arguments) :type trait-type))
    (unless (equal trait-type (Struct:get (car arguments) :type))
      (error "Dispatch argument of trait-function must have trait-type %s: %s"
             trait-type name))
    (when (Struct:get (car arguments) :default)
      (error "Dispatch argument of trait-function can not have a default: %s" name))
    `(cons ',name
           (Trait:Function
            :function (copy-sequence ',function)
            :default-implementation
            ,(and body?
                  (Struct:Function:emit-lambda function transformer t))
            :dispatch-function
            (lambda (&rest arguments)
              (Trait:dispatch ',trait ',name arguments))))))

(defun Trait:define* (trait)
  "Defines a new trait according to the given definition."
  (-let (((&plist :name :functions :supertraits)
          (Struct:properties trait)))
    (Trait:undefine name)
    (--each supertraits (Trait:get it :ensure))
    (--each functions
      (defalias (car it)
        (Struct:get (cdr it) :dispatch-function))
      (put (car it) 'Emil:Env:function-type
           (Struct:Function:type (Struct:get (cdr it) :function)))
      ;; (put (car it) 'function-documentation
      ;;      `(Trait:Function:documentation ',name ',(car it)))
      )
    (put name Trait:definition-symbol trait)
    name))

(defun Trait:undefine (name)
  "Undefines the trait named NAME.

This attempts to undo all effects of `Trait:define'.  This function is
idempotent."
  (when-let (trait (Trait:get name))
    (--each (Struct:get trait :functions)
      (fmakunbound (car it))
      (put (car it) 'Emil:Env:function-type nil)
      (put (car it) 'function-documentation nil))
    (put name Trait:definition-symbol nil)))

(defun Trait:documentation (name)
  "Return a documentation-string for trait named NAME."
  (-> (Trait:get name :ensure)
      (Struct:get :documentation)))

(defun Trait:Function:documentation (trait name)
  "Return a documentation-string for TRAIT's function named NAME."
  (-> (cdr (assq name (->
                       (Trait:get trait :ensure)
                       (Struct:get :functions))))
      (Struct:get :declaration)
      (Struct:get :documentation)))

(defmacro Trait:implement (trait type &rest properties-and-body)
  "Defines an implementation of TRAIT for TYPE.

(fn TRAIT TYPE [(fn FUNCTION-NAME ARGUMENTS . BODY)]*)"
  (declare (indent 2))
  (unless (symbolp trait)
    (signal 'wrong-type-argument `(symbol ,trait)))
  (unless (symbolp type)
    (signal 'wrong-type-argument `(symbol ,type)))
  (-let* (((properties body)
           (Commons:split-property-list properties-and-body))
          (disable-syntax (plist-get properties :disable-syntax))
          (functions (Trait:-check-implementation
                      trait type (--map (Struct:Function:read
                                          it (unless disable-syntax trait))
                                        body)))
          (transformer (unless (or disable-syntax
                                   (not (require 'Emil nil t)))
                         #'Emil:Syntax:transform)))
    (Trait:unimplement trait type)
    (Trait:-declare-implementation trait type)
    ;; FIXME: Trait should be unimplemented, if defining them throws an error.
    `(progn
       ;; Ensure that a type's traits are available at compile-time.
       (Trait:unimplement ',trait ',type)
       (Trait:-declare-implementation ',trait ',type)
       ;; Don't use `eval-and-compile', so the compiler will emit
       ;; warnings with meaningful source-positions.
       (Trait:implement*
        ',trait ',type
        (list ,@(Trait:-emit-functions functions transformer))))))

(defun Trait:-check-implementation (trait type functions)
  (-let* ((function-alist (--annotate (Struct:get it :qualified-name) functions))
          (trait-struct (Trait:get trait :ensure))
          (supertraits (Struct:get trait-struct :supertraits))
          (trait-functions (Struct:get trait-struct :functions)))
    (--each supertraits
      (unless (memq type (Struct:get (Trait:get it :ensure) :implementing-types))
        (error "Required supertrait not implemented by type: %s" it)))
    (--each trait-functions
      (unless (or (assq (car it) function-alist)
                  (not (Trait:Function:required? (cdr it))))
        (error "Required function not implemented: %s" (car it))))
    (--each function-alist
      (unless (assq (car it) trait-functions)
        (error "Function not declared by this trait: %s" (car it)))
      (let ((arguments (Struct:get (cdr it) :arguments)))
        (unless arguments
          (error "Function argument-list can not be empty: %s" (car it)))
        (unless (Struct:get (car arguments) :type)
          (Struct:unsafe-set (car arguments) :type type))
        (unless (equal (Struct:get (car arguments) :type) type)
          (error "Dispatch argument of trait-function must have implementors type %s: %s"
                 type (car it)))))
    (--each trait-functions
      (when-let (entry (assq (car it) function-alist))
        (unless (Trait:-function-compatible?
                 (Struct:get (cdr it) :function) (cdr entry))
          (error "Signature incompatible with function declared by trait: %s, %s"
                 trait (car it)))))
    functions))

(defun Trait:-function-compatible? (trait-fn fn)
  (let ((trait-fn-arity (Struct:Function:arity trait-fn))
        (fn-arity (Struct:Function:arity fn)))
    (and (<= (car fn-arity) (car trait-fn-arity))
         (>= (cdr fn-arity) (cdr trait-fn-arity))
         (-every? (-lambda ((fn-argument . trait-fn-argument))
                    (or (null (Struct:get fn-argument :type))
                        (equal (Struct:get fn-argument :type)
                               (Struct:get trait-fn-argument :type))))
                  (-zip-pair (cdr (Struct:get fn :arguments))
                             (cdr (Struct:get trait-fn :arguments))))
         (or (null (Struct:get fn :return-type))
             (equal (Struct:get fn :return-type)
                    (Struct:get trait-fn :return-type))))))

(defun Trait:-emit-functions (functions transformer)
  (--map `(cons ',(Struct:get it :qualified-name)
                ,(Struct:Function:emit-lambda it transformer t))
         functions))

(defun Trait:-declare-implementation (trait type)
  "Declare that TRAIT is implemented by TYPE."
  (Struct:update (Trait:get trait :ensure)
                 :implementing-types (-partial #'cons type))
  (put type Trait:implemented-symbol
       (cons trait (get type Trait:implemented-symbol)))
  type)

(defun Trait:implement* (trait type functions)
  "Defines an implementation of TRAIT for TYPE."
  (--each (Struct:get (Trait:get trait :ensure) :functions)
    (when-let (entry (assq (car it) functions))
      (Struct:update (cdr it) :implementations
                     (-partial #'cons (cons type (cdr entry))))))
  type)

(defun Trait:unimplement (trait type)
  (when-let (struct (Trait:get trait))
    (Struct:update struct :implementing-types (-partial #'remove type))
    (put type Trait:implemented-symbol
         (remove trait (get type Trait:implemented-symbol)))
    (--each (Struct:get struct :functions)
      (Struct:update (cdr it) :implementations
        (lambda (implementations)
          (remove (assq type implementations) implementations))))
    nil))

(defun Trait:dispatch (trait function arguments)
  "Dispatch call of TRAIT's FUNCTION using ARGUMENTS."
  (let (type trait-struct function-struct impl)
    (unless (consp arguments)
      (signal 'wrong-number-of-arguments (list 0)))
    (setq type (Trait:type-of (car arguments)))
    (unless (setq trait-struct (Trait:get trait))
      (error "Trait not defined: %s" trait))
    (unless (setq function-struct
                  (cdr (assq function (Struct:unsafe-get trait-struct :functions))))
      (error "Trait function not defined: %s, %s" trait function))
    (unless (setq impl (or (cdr (assq type (Struct:unsafe-get
                                            function-struct :implementations)))
                           (and (memq type (Struct:unsafe-get trait-struct
                                                              :implementing-types))
                                (Struct:unsafe-get
                                 function-struct :default-implementation))))
      (unless (memq type (Struct:unsafe-get trait-struct :implementing-types))
        (error "Type does not implement trait: %s, %s" type trait))
      (error "Required function not implemented by type: %s, %s" function type))
    (apply impl arguments)))

(defun Trait:implements? (type trait)
  "Return `t', if type TYPE implements trait TRAIT.

TYPE and TRAIT should both be symbols.

Signals a `wrong-type-argument', if TRAIT is not a defined trait."
  (not (null (memq type (Struct:get (Trait:get trait :ensure)
                                    :implementing-types)))))

(defun Trait:implemented (type)
  "Returns a list of traits implemented by type.

TYPE should be a symbol.

Signals a `wrong-type-argument', if TYPE is not a symbol."
  (copy-sequence (get type Trait:implemented-symbol)))

(defun Trait:extends? (trait other)
  "Return `t', if trait TRAIT extends trait OTHER.

TRAIT and OTHER should both be symbols."
  (when-let (trait-struct (Trait:get trait))
    (not (null (memq other (Struct:get trait-struct :supertraits))))))

(cl-deftype Trait (&rest traits)
  `(satisfies ,(lambda (value)
                 (let ((type (Trait:type-of value)))
                   (cl-every (lambda (trait)
                               (Trait:implements? type trait))
                             traits)))))

(defun Trait:instanceof (object trait)
  "Return `t' if OBJECT implements TRAIT

OBJECT may be any value, TRAIT should be a trait symbol.

Signals a `wrong-type-argument', if TRAIT is not a symbol denoting a trait."
  (Trait:implements? (Trait:type-of object) trait))

(defun Trait:type-of (value)
  "Returns the type of VALUE.

If VALUE is a struct type, return it's name. Otherwise, calls
`type-of' on VALUE and return its result."
  (or (Struct:name value)
      (type-of value)))

(defun Trait:functions (trait)
  "Returns a list of functions declared by TRAIT.

TRAIT should be a symbol."
  (if-let (declared (assq trait Trait:declared-traits))
      (-let (((_ supertraits functions) declared))
        (append functions (-mapcat #'Trait:functions supertraits)))
    (when-let (type (Trait:get trait))
      (--map (Struct:get (cdr it) :function)
             (Struct:get type :functions)))))

(defun Trait:implemented-functions (type)
  "Returns a list of trait-functions implemented by TYPE.

TYPE should be a symbol."
  (-mapcat #'Trait:functions (Trait:implemented type)))

(provide 'Trait)
;;; Trait.el ends here
