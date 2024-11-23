;; -*- lexical-binding: t -*-

(require 'cl-macs)
(require 'dash)
(require 'Commons)
(require 'Struct)
(require 'Trait)
(require 'Emil/Util)
(require 'Emil/Error)

;; Shut up cl-type-check's compiler warnings.
(cl-deftype Emil:Type:Variable nil
    (list 'satisfies #'Emil:Type:Variable?))
(cl-deftype Emil:Type:Existential nil
    (list 'satisfies #'Emil:Type:Existential?))

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

  (fn Emil:Type:substitute (self (source Emil:Type:Variable)
                                 (target Emil:Type:Existential))
    "Substitutes a type-variable SOURCE with an instance TARGET in this type.

Returns the substituted type."
    self)

  (fn Emil:Type:substitute-all (self sources targets)
    (unless (= (length sources) (length targets))
      (error "Length of sources and targets should be equal"))
    (--reduce-from
     (Emil:Type:substitute acc (car it) (cdr it))
     self
     (-zip-pair sources targets)))

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
   "The name of the type."
   :type symbol))

(Trait:implement Emil:Type Emil:Type:Basic
  (fn Emil:Type:print (self)
    (Struct:get self :name)))

(Struct:define Emil:Type:Arrow
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

(Struct:implement Emil:Type:Arrow
  (fn Emil:Type:Arrow:arguments (self)
    "Returns the list of argument."
    (Struct:get self :arguments))

  (fn Emil:Type:Arrow:returns (self)
    "Returns the return type."
    (Struct:get self :returns))

  (fn Emil:Type:Arrow:arity (self &optional numeric?)
    "Returns the minimal and maximal accepted number of arguments.

Returns a cons \(MIN . MAX\), where MAX may be the symbol `many', if
the function uses a &rest argument. If NUMERIC? is non-nil, use
`most-positive-fixnum' instead.

See also `func-arity'."
    (let ((min (Struct:get self :min-arity))
          (max (if (Struct:get self :rest?)
                   (if numeric? most-positive-fixnum 'many)
                 (length (Struct:get self :arguments)))))
      (cons min max)))

  (fn Emil:Type:Arrow:normalize-arity (arity)
    (cond
     ((Emil:Type:Arrow? arity)
      (Emil:Type:Arrow:arity arity t))
     ((eq 'many (cdr arity))
      (cons (car arity) most-positive-fixnum))
     (t arity)))

  (fn Emil:Type:Arrow:arity-assignable-to? (self other)
    "Return `t', if function is arity-wise assignable to OTHER.

Argument OTHER should be either another `Emil:Type:Arrow' type; or a
cons \(MIN . MAX\), with MIN and MAX being integers. MAX may also be
the symbol `many' for an unlimited number of arguments.

Returns `t', if this function can accept any number of arguments that
are also accepted by OTHER; else `nil'."
    (-let (((other-min . other-max)
            (Emil:Type:Arrow:normalize-arity other))
           ((min . max) (Emil:Type:Arrow:arity self t)))
      (and (<= min other-min)
           (>= max other-max))))

  (fn Emil:Type:Arrow:arity-assignable-from? (self other)
    "Return `t', if OTHER is arity-wise assignable to this function.

This is the inverse of `Emil:Type:Arrow:arity-assignable-to?', which
see."
    (-let (((other-min . other-max)
            (Emil:Type:Arrow:normalize-arity other))
           ((min . max) (Emil:Type:Arrow:arity self t)))
      (and (<= other-min min)
           (>= other-max max))))

  (fn Emil:Type:Arrow:adjusted-arguments (self count)
    "Returns an argument-list with COUNT nominal arguments.

Signals an error if COUNT is less than the minimal accepted number of
arguments of this function; or if COUNT is larger then the maximal
accepted ones."
    (let ((rest? (Struct:get self :rest?))
          (min-arity (Struct:get self :min-arity))
          (arguments (Emil:Type:Arrow:arguments self)))
      (Emil:Type:Arrow:-adjusted-arguments arguments count min-arity rest?)))

  (fn Emil:Type:Arrow:lambda-adjusted-arguments (argument-list count)
    "Like `Emil:Type:Arrow:adjusted-arguments', but for lambda arguments."
    (let ((rest? (memq '&rest argument-list))
          (min-arity (car (func-arity `(lambda ,argument-list))))
          (arguments (Emil:Type:Arrow:lambda-variables argument-list)))
      (Emil:Type:Arrow:-adjusted-arguments arguments count min-arity rest?)))

  (fn Emil:Type:Arrow:-adjusted-arguments (arguments count min-arity rest?)
    (cond
     ((= (length arguments) count)
      arguments)
     ((< count min-arity)
      (error "Attempted to adjust arguments below min-arity: %d, %s"
             count arguments))
     ((and (> count (length arguments))
           (not rest?))
      (error "Attempted to enlarge arguments of a non-rest function: %s" arguments))
     (t
      (append (-take count arguments)
              (-repeat (- count (length arguments))
                       (car (last arguments)))))))

  (fn Emil:Type:Arrow:lambda-variables (arguments)
    "Return ARGUMENTS excluding &optional and &rest keywords."
    (--filter (not (memq it '(&optional &rest))) arguments)))

(Trait:implement Emil:Type Emil:Type:Arrow
  (fn Emil:Type:print (self)
    (let* ((arguments (Struct:get self :arguments))
           (rest? (Struct:get self :rest?))
           (fixed (-take (Struct:get self :min-arity)
                         arguments))
           (optional (-drop (Struct:get self :min-arity)
                            (if rest? (butlast arguments) arguments)))
           (rest (if rest? (car (last arguments)))))
      `(-> (,@(-map #'Emil:Type:print fixed)
            ,@(if optional (list '&optional))
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

  (fn Emil:Type:substitute (self (source Emil:Type:Variable)
                                 (target Emil:Type:Existential))
    (Emil:Type:Arrow*
     ,@self
     :arguments (--map (Emil:Type:substitute it source target)
                       (Struct:get self :arguments))
     :returns (Emil:Type:substitute (Struct:get self :returns)
                                    source target))))

(Struct:define Emil:Type:Forall
  "Represents a polymorphic type."
  (parameters
   "The list of parameters of type `Emil:Type:Variable'."
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
    (null (Struct:get self :parameters)))

  (fn Emil:Type:free-variables (self)
    (Emil:Type:free-variables (Struct:get self :type)))

  (fn Emil:Type:substitute (self (source Emil:Type:Variable)
                                 (target Emil:Type:Existential))
    (Emil:Type:Forall*
     ,@self
     :type (Emil:Type:substitute (Struct:get self :type)
                                 source target))))

(Struct:define Emil:Type:Variable
  "Represents a type-variable."
  (name
   "The name of the variable."
   :type symbol))

(Trait:implement Emil:Type Emil:Type:Variable
  (fn Emil:Type:print (self)
    `(quote ,(Struct:get self :name)))

  (fn Emil:Type:substitute (self (source Emil:Type:Variable)
                                 (target Emil:Type:Existential))
    (if (equal self source)
        target
      self)))

(Struct:define Emil:Type:Existential
  "Represents an instance of a type-variable."
  (name
   "The name of the variable."
   :type symbol))

(Trait:implement Emil:Type Emil:Type:Existential
  (fn Emil:Type:print (self)
    `(quote ,(intern (format "%s?" (Struct:get self :name)))))

  (fn Emil:Type:free-variables (self)
    (list (Struct:get self :name))))

(Struct:define Emil:Type:Compound
  "Represents a compound type.

Compound types are currently always covariant."
  (name
   "The name of this type's constructor."
    :type symbol)
  (parameters
   "The parameters of this type."
   :type (List (Trait Emil:Type))))

(Trait:implement Emil:Type Emil:Type:Compound
  (fn Emil:Type:print (self)
    (cons (Struct:get self :name)
          (-map #'Emil:Type:print
                (Struct:get self :parameters))))

  (fn Emil:Type:free-variables (self)
    (-mapcat #'Emil:Type:free-variables
             (Struct:get self :parameters)))

  (fn Emil:Type:monomorph? (self)
    (--every? (Emil:Type:monomorph? it)
              (Struct:get self :parameters)))

  (fn Emil:Type:substitute (self (source Emil:Type:Variable)
                                 (target Emil:Type:Existential))
    (Emil:Type:Compound*
     ,@self
     :parameters (--map (Emil:Type:substitute it source target)
                        (Struct:get self :parameters)))))

(defun Emil:Type:check-builtin-compound-type (type)
  (pcase type
    ((Struct Emil:Type:Compound name parameters)
     (when (and (eq 'List name)
                (/= (length parameters) 1))
       (Emil:invalid-type-form "List constructor has one parameter: %s" type))
     (when (and (eq 'Cons name)
                (/= (length parameters) 2))
       (Emil:invalid-type-form "Cons constructor has two parameters: %s" type))))
  type)

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
  (cl-labels ((variables (type)
                (pcase-exhaustive type
                  ((Struct Emil:Type:Arrow arguments returns)
                   (append (variables returns)
                           (-mapcat #'variables arguments)))
                  ((Struct Emil:Type:Compound parameters)
                   (-mapcat #'variables parameters))
                  ((Struct Emil:Type:Variable name)
                   (list name))
                  (_ nil))))
    (let* ((type (Emil:Type:-read form))
           (parameters (sort (-uniq (variables type)) #'string-lessp)))
      (if parameters
          (Emil:Type:Forall
           :parameters (--map (Emil:Type:Variable :name it) parameters)
           :type type)
        type))))

(defun Emil:Type:function? (type)
  (or (Emil:Type:Arrow? type)
      (and (Emil:Type:Forall? type)
           (Emil:Type:Arrow? (Struct:get type :type)))))

(defun Emil:Type:read-function (form)
  "Like `Emil:Type:read', but rejects non-function types."
  (let ((type (Emil:Type:read form)))
    (unless (Emil:Type:function? type)
      (error "Expected to read a function-type: %s" form))
    type))

(defun Emil:Type:-read (form)
  "Reads a type from FORM.

The result may contain type-variables."
  (pcase form
    ('Null (Emil:Type:Null))
    ('Any (Emil:Type:Any))
    ('Never (Emil:Type:Never))
    ('Void (Emil:Type:Void))
    ((pred symbolp)
     (Emil:Type:-assert-valid-name form)
     (Emil:Type:Basic :name form))
    ((and `(quote ,name)
          (guard (symbolp name)))
     (Emil:Type:-assert-valid-name name)
     (Emil:Type:Variable :name name))
    ((and `(-> ,arguments ,returns)
          (guard (listp arguments)))
     (Emil:Type:-read-fn arguments returns))
    ((and `(,name . ,parameters)
          (guard (and (symbolp name)
                      (listp parameters))))
     (Emil:Type:-assert-valid-name name)
     (Emil:Type:check-builtin-compound-type
      (Emil:Type:Compound
       :name name
       :parameters (-map #'Emil:Type:-read parameters))))
    (_ (Emil:invalid-type-form "Failed to read form as a type: %s" form))))

(defun Emil:Type:-assert-valid-name (name)
  (unless (symbolp name)
    (Emil:invalid-type-form "Expected a symbol: %s" name))
  (when (eq 'quote name)
    (Emil:invalid-type-form "quote can not be used as a name: %s" name))
  (when (string-suffix-p "?" (symbol-name name))
    (Emil:invalid-type-form "Names may not end with a `?': %s" name))
  (unless (string-match-p "\\`[a-zA-Z]" (symbol-name name))
    (Emil:invalid-type-form "Names should start with a letter: %s" name))
  (when (Commons:constant-symbol? name)
    (Emil:invalid-type-form "Names may not be constant symbols: %s" name)))

(defun Emil:Type:-read-fn (argument-forms returns-form)
  (let ((optional? nil)
        (rest? nil)
        (min-arity 0)
        (arguments nil)
        (returns (Emil:Type:-read returns-form))
        (form (list argument-forms returns-form)))
    (while argument-forms
      (let ((argument-form (pop argument-forms)))
        (pcase argument-form
          ('&optional
           (when rest?
             (Emil:invalid-type-form "&optional can not follow &rest: %s" form))
           (unless argument-forms
             (Emil:invalid-type-form
              "&optional should be followed by an argument: %s" form))
           (setq optional? t))
          ('&rest
           (unless (= 1 (length argument-forms))
             (Emil:invalid-type-form
              "&rest should be followed by one argument: %s" form))
           (push (Emil:Type:-read (pop argument-forms))
                   arguments)
           (setq rest? t))
          (_
           (push (Emil:Type:-read argument-form) arguments)
           (unless optional?
             (cl-incf min-arity))))))

    (setq arguments (nreverse arguments))
    (Emil:Type:Arrow* arguments rest? returns min-arity)))

(defun Emil:Type:normalize (type)
  "Return a normalized representation of TYPE.

This renames all type-variables with standard ones."
  (let* ((generator (Emil:Util:NameGenerator))
         (replacements nil))
    (cl-labels ((generate (name)
                  (Emil:Util:NameGenerator:next generator))
                (replace (type)
                  (unless (assoc type replacements)
                    (push (cons type (Emil:Util:NameGenerator:next generator))
                          replacements))
                  (cdr (assoc type replacements)))
                (normalize (type)
                  (pcase-exhaustive type
                    ((Struct Emil:Type:Forall parameters type)
                     (Emil:Type:Forall
                      :parameters (-map #'normalize parameters)
                      :type (normalize type)))
                    ((Struct Emil:Type:Arrow arguments returns)
                     (Emil:Type:Arrow*
                      ,@type
                      :arguments (-map #'normalize arguments)
                      :returns (normalize returns)))
                    ((Struct Emil:Type:Compound parameters)
                     (Emil:Type:Compound*
                      ,@type
                      :parameters(-map #'normalize parameters)))
                    ((Struct Emil:Type:Variable)
                     (Emil:Type:Variable :name (replace type)))
                    ((Struct Emil:Type:Existential)
                     (Emil:Type:Existential :name (replace type)))
                    (_ type))))
      (normalize type))))

(provide 'Emil/Type)
