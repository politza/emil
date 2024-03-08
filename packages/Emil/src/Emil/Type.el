;; -*- lexical-binding: t -*-

(require 'cl-macs)
(require 'dash)
(require 'Commons)
(require 'Struct)
(require 'Trait)

;; Shut up cl-type-check's compiler warnings.
(cl-deftype Emil:Type:Var ())
(cl-deftype Emil:Type:VarInst ())

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
  (argument-types
   "The list of argument types excluding &rest."
   :type (List (Trait Emil:Type)))
  (rest-type
   "The type of the optional &rest argument."
   :type (or null (Trait Emil:Type)))
  (return-type
   "The return type of this function."
   :type (Trait Emil:Type))
  (min-arity
   "The minimum number of arguments required."
   :type number))

(Trait:implement Emil:Type Emil:Type:Fn
  (fn Emil:Type:print (self)
    (let ((fixed (-take (Struct:get self :min-arity)
                                  (Struct:get self :argument-types)))
          (optional (-drop (Struct:get self :min-arity)
                                     (Struct:get self :argument-types)))
          (rest (Struct:get self :rest-type)))
      `(-> (,@(-map #'Emil:Type:print fixed)
            ,@(if optional '(&optional))
            ,@(-map #'Emil:Type:print optional)
            ,@(if rest (list '&rest (Emil:Type:print rest))))
           ,(Emil:Type:print (Struct:get self :return-type)))))

  (fn Emil:Type:monomorph? (self)
    (and (--every? (Emil:Type:monomorph? it)
                   (Struct:get self :argument-types))
         (or (not (Struct:get self :rest-type))
             (Emil:Type:monomorph? (Struct:get self :rest-type)))
         (Emil:Type:monomorph? (Struct:get self :return-type))))

  (fn Emil:Type:free-variables (self)
    (-concat (-mapcat #'Emil:Type:free-variables
                      (Struct:get self :argument-types))
             (and (Struct:get self :rest-type)
                  (Emil:Type:free-variables
                   (Struct:get self :rest-type)))
             (Emil:Type:free-variables
                    (Struct:get self :return-type)))))

(Struct:define Emil:Type:Forall
  "Represents a polymorphic type."
  (variables
   "The list of variables of type `Emil:Type:Var'."
   :type list)
  (type
   "The type this type parameterizes.

Currently, only function types are supported."
   :type (Trait Emil:Type)))

(Trait:implement Emil:Type Emil:Type:Forall
  (fn Emil:Type:print (self)
    (Emil:Type:print (Struct:get self :type)))

  (fn Emil:Type:monomorph? (self)
    (ignore self)
    (null (Struct:get self :variables)))

  (fn Emil:Type:free-variables (self)
    (Emil:Type:free-variables (Struct:get self :type))))

(Struct:define Emil:Type:Var
  "Represents a type-variable."
  (name
   "The name of the variable."
   :type symbol))

(Trait:implement Emil:Type Emil:Type:Var
  (fn Emil:Type:print (self)
    `(quote ,(Struct:get self :name))))

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
       (Emil:invalid-type-form "Constant symbol used as type variable: %s in %s"
         name form))
     (unless (string-match-p "\\`[a-zA-Z]" (symbol-name name))
       (Emil:invalid-type-form "Type variables should start with a letter: %s in %s"
         name form))
     (Emil:Type:Var :name name))
    (`(-> ,arguments ,returns)
     (Emil:Type:-read-fn arguments returns))
    (_ (Emil:invalid-type-form "Failed to read form as a type: %s" form))))

(defun Emil:Type:-read-fn (arguments returns)
  (let ((optional? nil)
        (rest? nil)
        (min-arity 0)
        (argument-types nil)
        (rest-type nil)
        (return-type (Emil:Type:-read returns t))
        (type-variables nil)
        (form (list arguments returns)))
    (while arguments
      (let ((argument (pop arguments)))
        (pcase argument
          ('&optional
           (when rest?
             (Emil:invalid-type-form "&optional can not follow &rest: %s" form))
           (unless arguments
             (Emil:invalid-type-form "&optional should be followed by an argument: %s" form))
           (setq optional? t))
          ('&rest
           (unless (= 1 (length arguments))
             (Emil:invalid-type-form "&rest should be followed by one argument: %s" form))
           (setq rest-type (Emil:Type:-read (pop arguments) t))
           (when (Emil:Type:Var? rest-type)
             (cl-pushnew rest-type type-variables :test #'equal)))
          (_
           (let ((type (Emil:Type:-read argument t)))
             (push type argument-types)
             (when (Emil:Type:Var? type)
               (cl-pushnew type type-variables :test #'equal))
             (unless optional?
               (cl-incf min-arity)))))))

    (setq argument-types (nreverse argument-types))
    (when (Emil:Type:Var? return-type)
      (cl-pushnew return-type type-variables :test #'equal))
    (let ((type (Emil:Type:Fn* argument-types rest-type return-type min-arity)))
      (if type-variables
          (Emil:Type:Forall* :variables type-variables type)
        type))))

(provide 'Emil/Type)
