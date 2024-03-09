;; -*- lexical-binding: t -*-

(require 'cl-macs)
(require 'dash)
(require 'Commons)
(require 'Struct)
(require 'Trait)

;; Shut up cl-type-check's compiler warnings.
(cl-deftype Emil:Type:Var ())
(cl-deftype Emil:Type:VarInst ())
(cl-deftype Emil:Type:Generator ())

(Trait:define Emil:Type ()
  (fn Emil:Type:monomorph? (self)
    "Returns non-nil, if this is a monomorphic type.

Monomorphic types do not contain any type-variables.  E.g. the
builtin type `number' is monomorphic, while the function `mapcar'
can be viewed as polymorphic, since it maps lists of arbitrary
type."
    (ignore self)
    t)

  (fn Emil:Type:free-variables (self)
    "Returns the list of instantiated free variables in this type.

Ordinarily, all variables of a type are bound via an enclosing
`Emil:Type:Forall' and can not be free. I.e. a variable can only
become free in a type during type-checking."
    (ignore self)
    nil)

  (fn Emil:Type:substitute (self (source Emil:Type:Var)
                                 (target Emil:Type:VarInst))
    "Substitutes a type-variable SOURCE with an instance TARGET in this type.

Returns the substituted type."
    self)

  (fn Emil:Type:print (self)
    "Returns a readable representation of this type."))

(Struct:define Emil:Type:Any
  "Represents the union of all types.

Every type is a subtype of this type, including itself.

This type is no subtype of any other type, excluding itself.")

(Trait:implement Emil:Type Emil:Type:Any
  (fn Emil:Type:print (_) 'Any))

(Struct:define Emil:Type:Never
  "Represents the bottom type.

This type has no values. It is a subtype of any other type,
including itself. No other type is a subtype of this type,
excluding itself.")

(Trait:implement Emil:Type Emil:Type:Never
  (fn Emil:Type:print (_) 'Never))

(Struct:define Emil:Type:Null
  "Represents the null type.

This type has a single value, which is `nil'. Currently, this
type is a subtype of any other type, excluding `Never' and
`Void'.")

(Trait:implement Emil:Type Emil:Type:Null
  (fn Emil:Type:print (_) 'Null))

(Struct:define Emil:Type:Void
  "Represents the void type.

This type has no values. This type is a subtype of no other type,
excluding `Any' and itself. No other type is a subtype of this
type, excluding `Never' and itself.")

(Trait:implement Emil:Type Emil:Type:Void
  (fn Emil:Type:print (_) 'Void))

(Struct:define Emil:Type:Basic
  "Represents a basic custom or builtin type."
  (name
   "The name of the name."
   :type symbol))

(Trait:implement Emil:Type Emil:Type:Basic
  (fn Emil:Type:print (self)
    (Struct:get self :name)))

(Struct:define Emil:Type:Fn
  "Represents the function type."
  (arguments
   "The list of argument types."
   :type (List (Trait Emil:Type)))
  (rest?
   "Whether the last argument is a &rest one."
   :type boolean)
  (returns
   "The return type of this function."
   :type (Trait Emil:Type))
  (min-arity
   "The minimum number of required arguments."
   :type number))

(Struct:implement Emil:Type:Fn
  (fn Emil:Type:Fn:instantiate (self (generator Emil:Type:Generator))
    "Instantiate this function with type-variables.

Returns a variant of this function in which all types are
replaced with instances of type-variables."
    (Emil:Type:Fn*
     ,@self
     :arguments (--map (Emil:Type:Generator:next generator)
                       (Struct:get self :arguments))
     :returns (Emil:Type:Generator:next generator)))

  (fn Emil:Type:Fn:arguments (self)
    "Returns the list of argument-types."
    (Struct:get self :arguments))

  (fn Emil:Type:Fn:butrest-arguments (self)
    "Returns the list of argument-types, excluding the &rest one."
    (if (Struct:get self :rest?)
        (-butlast (Struct:get self :arguments))
      (Struct:get self :arguments)))

  (fn Emil:Type:Fn:rest-argument (self)
    "Returns the &rest argument, if present."
    (if (Struct:get self :rest?)
        (-last-item (Struct:get self :arguments))))

  (fn Emil:Type:Fn:returns (self)
    "Returns the return type."
    (Struct:get self :returns))

  (fn Emil:Type:Fn:arity (self)
    "Returns the minimal and maximal accepted number of arguments.

Returns a cons \(MIN . MAX\), where MAX may be `most-positive-fixnum',
if the function uses a &rest argument."
    (let ((min (Struct:get self :min-arity))
          (max (if (Struct:get self :rest?)
                   most-positive-fixnum
                 (length (Struct:get self :arguments)))))
      (cons min max))))

(Trait:implement Emil:Type Emil:Type:Fn
  (fn Emil:Type:print (self)
    (let ((fixed (-take (Struct:get self :min-arity)
                        (Struct:get self :arguments)))
          (optional (-drop (Struct:get self :min-arity)
                           (Emil:Type:Fn:butrest-arguments self)))
          (rest (Emil:Type:Fn:rest-argument self)))
      `(-> (,@(-map #'Emil:Type:print fixed)
            ,@(if optional '(&optional))
            ,@(-map #'Emil:Type:print optional)
            ,@(if rest (list '&rest (Emil:Type:print rest))))
           ,(Emil:Type:print (Struct:get self :returns)))))

  (fn Emil:Type:monomorph? (self)
    (and (--every? (Emil:Type:monomorph? it)
                   (Struct:get self :arguments))
         (Emil:Type:monomorph? (Struct:get self :returns))))

  (fn Emil:Type:free-variables (self)
    (-concat (-mapcat #'Emil:Type:free-variables
                      (Struct:get self :arguments))
             (Emil:Type:free-variables
                    (Struct:get self :returns))))

  (fn Emil:Type:substitute (self (source Emil:Type:Var)
                                 (target Emil:Type:VarInst))
    (Emil:Type:Fn*
     ,@self
     :arguments (--map (Emil:Type:substitute it source target)
                       (Struct:get self :arguments))
     :returns (Emil:Type:substitute (Struct:get self :returns)
                                    source target))))

(Struct:define Emil:Type:Forall
  "Represents a polymorphic type."
  (parameters
   "The list of parameters of type `Emil:Type:Var'."
   :type list)
  (type
   "The type this type parameterizes.

Currently, only function types are supported."
   :type (Trait Emil:Type)))

(Struct:implement Emil:Type:Forall
  (fn Emil:Type:Forall:parameters (self)
    "Returns the list of parameters"
    (Struct:get self :parameters))

  (fn Emil:Type:Forall:type (self)
    "Returns the parameterized type."
    (Struct:get self :type)))

(Trait:implement Emil:Type Emil:Type:Forall
  (fn Emil:Type:print (self)
    (Emil:Type:print (Struct:get self :type)))

  (fn Emil:Type:monomorph? (self)
    (ignore self)
    (null (Struct:get self :parameters)))

  (fn Emil:Type:free-variables (self)
    (Emil:Type:free-variables (Struct:get self :type)))

  (fn Emil:Type:substitute (self (source Emil:Type:Var)
                                 (target Emil:Type:VarInst))
    (Emil:Type:Forall*
     ,@self
     :type (Emil:Type:substitute (Struct:get self :type)
                                 source target))))

(Struct:define Emil:Type:Var
  "Represents a type-variable."
  (name
   "The name of the variable."
   :type symbol))

(Trait:implement Emil:Type Emil:Type:Var
  (fn Emil:Type:print (self)
    `(quote ,(Struct:get self :name)))

  (fn Emil:Type:substitute (self (source Emil:Type:Var)
                                 (target Emil:Type:VarInst))
    (if (equal self source)
        target
      self)))

(Struct:define Emil:Type:VarInst
  "Represents an instance of a type-variable."
  (name
   "The name of the variable."
   :type symbol))

(Trait:implement Emil:Type Emil:Type:VarInst
  (fn Emil:Type:print (self)
    `(quote (quote ,(Struct:get self :name))))

  (fn Emil:Type:free-variables (self)
    (list self)))

(Struct:define Emil:Type:Generator
  "Generator for instances of type `Emil:Type:VarInst'."
  (counter :type number :default -1 :mutable t))

(defun Emil:Type:Generator:next (self)
  (Emil:Type:VarInst
   :name (->> (Struct:update self :counter #'1+)
              (format "t%d")
              (intern))))

(defun Emil:Type:read (form)
  "Reads a type from form FORM and returns it.

FORM should have one of the following forms.

One of the symbols `Null', `Any', `Never' or `Void' for the
corresponding type.

Any other, non-constant symbol for a custom or builtin basic type,
e.g. `overlay', `string' or `my-custom-type'.

`\(quote SYMBOL\)' for a type-variable. SYMBOL should not be a
 constant, e.g. `nil'. It should also start with a letter, in order to
 ensure some forward extensibility. Currently, type-variables can only
 be used as part of a function-type.

`(-> ARGUMENTS RETURN)' for a function type. ARGUMENTS should be a
list of readable types as described here. It may also contain the
keywords `&optional' and `&rest', in order to indicate optional- and
rest-arguments. RETURN should likewise be a readable type. If either
ARGUMENTS or RETURN contains resp. is a type-variable, the resulting
type will be polymorphic."
  (Emil:Type:-read form))

(Commons:define-error Emil:invalid-type-form
  "Invalid type form")

(defun Emil:Type:-read (form &optional allow-type-variables)
  (pcase form
    ('Null (Emil:Type:Null))
    ('Any (Emil:Type:Any))
    ('Never (Emil:Type:Never))
    ('Void (Emil:Type:Void))
    ((pred symbolp) (Emil:Type:Basic :name form))
    ((and `(quote ,name) (guard (symbolp name)))
     (unless allow-type-variables
       (Emil:invalid-type-form "Type variables not allowed here: %s" form))
     (when (Commons:constant-symbol? name)
       (Emil:invalid-type-form
        "Constant symbol used as type variable: %s in %s"
        name form))
     (unless (string-match-p "\\`[a-zA-Z]" (symbol-name name))
       (Emil:invalid-type-form
        "Type variables should start with a letter: %s in %s" name form))
     (Emil:Type:Var :name name))
    (`(-> ,arguments ,returns)
     (Emil:Type:-read-fn arguments returns))
    (_ (Emil:invalid-type-form "Failed to read form as a type: %s" form))))

(defun Emil:Type:-read-fn (arguments-form returns-form)
  (let ((optional? nil)
        (rest? nil)
        (min-arity 0)
        (arguments nil)
        (returns (Emil:Type:-read returns-form t))
        (parameters nil)
        (form (list arguments-form returns-form)))
    (while arguments-form
      (let ((argument-form (pop arguments-form)))
        (pcase argument-form
          ('&optional
           (when rest?
             (Emil:invalid-type-form "&optional can not follow &rest: %s" form))
           (unless arguments-form
             (Emil:invalid-type-form
              "&optional should be followed by an argument: %s" form))
           (setq optional? t))
          ('&rest
           (unless (= 1 (length arguments-form))
             (Emil:invalid-type-form
              "&rest should be followed by one argument: %s" form))
           (let ((type (Emil:Type:-read (pop arguments-form) t)))
             (push type arguments)
             (when (Emil:Type:Var? type)
               (cl-pushnew type parameters :test #'equal)))
           (setq rest? t))
          (_
           (let ((type (Emil:Type:-read argument-form t)))
             (push type arguments)
             (when (Emil:Type:Var? type)
               (cl-pushnew type parameters :test #'equal))
             (unless optional?
               (cl-incf min-arity)))))))

    (setq arguments (nreverse arguments))
    (when (Emil:Type:Var? returns)
      (cl-pushnew returns parameters :test #'equal))
    (let ((type (Emil:Type:Fn* arguments rest? returns min-arity)))
      (if parameters
          (Emil:Type:Forall* parameters type)
        type))))

(provide 'Emil/Type)
