;; -*- lexical-binding: t -*-

;; Copyright (C) 2024  Andreas Politz

;; Author: Andreas Politz

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

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
  :disable-syntax t
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
  "Represents a wildcard type.

Every type can be assigned to this type, including itself. This type
can be assinged to any other type, including itself. ")

(Trait:implement Emil:Type Emil:Type:Any
  :disable-syntax t
  (fn Emil:Type:print (_) 'Any))

(Struct:define Emil:Type:Never
  "Represents the \"nothing to see here\" type.

This type has no values. This type can't be assigned to any other
type, including itself. No other type can be assigned to this type,
including itself.")

(Trait:implement Emil:Type Emil:Type:Never
  :disable-syntax t
  (fn Emil:Type:print (_) 'Never))

(Struct:define Emil:Type:Null
  "Represents the null type.

This type has a single value, which is `nil'. This type can be
assigned to any other type, excluding `Never'.")

(Trait:implement Emil:Type Emil:Type:Null
  :disable-syntax t
  (fn Emil:Type:print (_) 'Null))

(Struct:define Emil:Type:Void
  "Represents the void type.

This type has no values. Thie type can not be assigned to any other
type, excluding itself. Any type can be assigned to this type,
including itself.")

(Trait:implement Emil:Type Emil:Type:Void
  :disable-syntax t
  (fn Emil:Type:print (_) 'Void))

(Struct:define Emil:Type:Basic
  "Represents a basic custom or builtin type or alias."
  (name
   "The name of the type."
   :type symbol))

(Trait:implement Emil:Type Emil:Type:Basic
  :disable-syntax t
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
  :disable-syntax t
  (fn Emil:Type:Arrow:arguments (self)
    "Returns the list of argument."
    (Struct:get self :arguments))

  (fn Emil:Type:Arrow:returns (self)
    "Returns the return type."
    (Struct:get self :returns))

  (fn Emil:Type:Arrow:rest? (self)
    "Returns non-nil, if the last argument is a rest one."
    (Struct:get self :rest?))

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
           (>= other-max max)))))

(Trait:implement Emil:Type Emil:Type:Arrow
  :disable-syntax t
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
  :disable-syntax t
  (fn Emil:Type:Forall:parameters (self)
    "Returns the list of parameters"
    (Struct:get self :parameters))

  (fn Emil:Type:Forall:type (self)
    "Returns the parameterized type."
    (Struct:get self :type)))

(Trait:implement Emil:Type Emil:Type:Forall
  :disable-syntax t
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
  :disable-syntax t
  (fn Emil:Type:print (self)
    `(quote ,(Struct:get self :name)))

  (fn Emil:Type:substitute (self (source Emil:Type:Variable)
                                 (target Emil:Type:Existential))
    (if (equal self source) target self)))

(Struct:define Emil:Type:Existential
  "Represents an instance of a type-variable."
  (name
   "The name of the variable."
   :type symbol))

(Trait:implement Emil:Type Emil:Type:Existential
  :disable-syntax t
  (fn Emil:Type:print (self)
    `(quote ,(Struct:get self :name)))

  (fn Emil:Type:free-variables (self)
    (list (Struct:get self :name))))

(Struct:define Emil:Type:Compound
  "Represents a compound type.

Compound types are currently always covariant."
  (name
   "The name of this type's constructor."
    :type symbol)
  (arguments
   "The arguments of this type."
   :type (List (Trait Emil:Type))))

(Trait:implement Emil:Type Emil:Type:Compound
  :disable-syntax t
  (fn Emil:Type:print (self)
    (cons (Struct:get self :name)
          (-map #'Emil:Type:print
                (Struct:get self :arguments))))

  (fn Emil:Type:free-variables (self)
    (-mapcat #'Emil:Type:free-variables
             (Struct:get self :arguments)))

  (fn Emil:Type:monomorph? (self)
    (--every? (Emil:Type:monomorph? it)
              (Struct:get self :arguments)))

  (fn Emil:Type:substitute (self (source Emil:Type:Variable)
                                 (target Emil:Type:Existential))
    (Emil:Type:Compound
     :name (Struct:get self :name)
     :arguments (--map (Emil:Type:substitute it source target)
                        (Struct:get self :arguments)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                  ((Struct Emil:Type:Compound arguments)
                   (-mapcat #'variables arguments))
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
    ((or 'Null 'null) (Emil:Type:Null))
    ((or 'Any 't) (Emil:Type:Any))
    ('Never (Emil:Type:Never))
    ('Void (Emil:Type:Void))
    (`(,(or 'integer 'float 'real 'number) . ,_)
     (Emil:Type:-read (car form)))
    ((or `(or ,(or 'Null 'null) ,type)
         `(or ,type ,(or 'Null 'null)))
     (Emil:Type:-read type))
    (`(,(or 'or 'and 'not 'member 'satisfies) . ,_)
     (Emil:Type:Any))
    ((pred symbolp)
     (Emil:Type:-assert-valid-name form form)
     (Emil:Type:Basic :name form))
    ((and `(quote ,name)
          (guard (symbolp name)))
     (Emil:Type:-assert-valid-name name form)
     (Emil:Type:Variable :name name))
    ((and `(-> ,arguments ,returns)
          (guard (listp arguments)))
     (Emil:Type:-read-fn arguments returns))
    ((and `(,name . ,arguments)
          (guard (and (symbolp name)
                      (listp arguments))))
     (Emil:Type:-assert-valid-name name form)
     (Emil:Type:check-builtin-compound-type
      (Emil:Type:Compound
       :name name
       :arguments (-map #'Emil:Type:-read arguments))))
    (_ (Emil:syntax-error "Failed to read form as a type: %s" form))))

(defun Emil:Type:-assert-valid-name (name form)
  (unless (symbolp name)
    (Emil:syntax-error "Expected a symbol: %s in %s" name form))
  (when (eq 'quote name)
    (Emil:syntax-error "Quote can not be used as a name: %s in %s" name form))
  (when (string-suffix-p "?" (symbol-name name))
    (Emil:syntax-error "Names may not end with a `?': %s in %s" name form))
  (unless (string-match-p "\\`[a-zA-Z]" (symbol-name name))
    (Emil:syntax-error "Names should start with a letter: %s in %s" name form))
  (when (Commons:constant-symbol? name)
    (Emil:syntax-error "Names may not be constant symbols: %s in %s" name form)))

(defun Emil:Type:-read-fn (argument-forms return-form)
  (let ((optional? nil)
        (rest? nil)
        (min-arity 0)
        (arguments nil)
        (returns (Emil:Type:-read return-form))
        (form (list argument-forms return-form)))
    (while argument-forms
      (let ((argument-form (pop argument-forms)))
        (pcase argument-form
          ('&optional
           (when rest?
             (Emil:syntax-error "&optional can not follow &rest: %s" form))
           (unless argument-forms
             (Emil:syntax-error
              "&optional should be followed by an argument: %s" form))
           (setq optional? t))
          ('&rest
           (unless (= 1 (length argument-forms))
             (Emil:syntax-error
              "&rest should be followed by one argument: %s" form))
           (push (Emil:Type:-read (pop argument-forms))
                   arguments)
           (setq rest? t))
          (_
           (push (Emil:Type:-read argument-form) arguments)
           (unless optional?
             (cl-incf min-arity))))))

    (Emil:Type:Arrow* :arguments (nreverse arguments) rest? returns min-arity)))

(defun Emil:Type:normalize (type)
  "Return a normalized representation of TYPE.

This renames all type-variables with standard ones."
  (let* ((generator (Emil:Util:NameGenerator))
         (replacements nil))
    (cl-labels ((replace (type)
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
                    ((Struct Emil:Type:Compound arguments)
                     (Emil:Type:Compound*
                      ,@type
                      :arguments(-map #'normalize arguments)))
                    ((Struct Emil:Type:Variable)
                     (Emil:Type:Variable :name (replace type)))
                    ((Struct Emil:Type:Existential)
                     (Emil:Type:Existential :name (replace type)))
                    (_ type))))
      (normalize type))))

(defconst Emil:Type:builtin-compound-types
  '((Cons . 2)
    (List . 1)
    (Vector . 1)
    (Array . 1)
    (Sequence . 1)
    (Trait . 1))
  "An alist mapping builtin compound types to their arity.")

(defun Emil:Type:check-builtin-compound-type (type)
  (pcase type
    ((Struct Emil:Type:Compound name arguments)
     (when-let (arity (cdr (assq name Emil:Type:builtin-compound-types)))
       (unless (= arity (length arguments))
         (Emil:syntax-error
          "Builtin constructor %s requires %d arguments: %s"
          name arity (Emil:Type:print type))))))
  type)

(defconst Emil:Type:type-aliases
  (--map (cons (car it)
               (Emil:Type:read (cdr it)))
         '((vector . (Vector Any))
           (sequence . (Sequence Any))
           (list . (List Any))
           (array . (Array Any))
           (cons . (Cons Any Any))
           (boolean . symbol)
           (function . (-> (&rest Any) Any))))
  "An alist mapping aliases to their type.")

(defconst Emil:Type:compound-type-hierarchy
  '((List Sequence)
    (Vector Array Sequence)
    (Array Sequence)))

(defconst Emil:Type:basic-type-hierarchy
  (append cl--typeof-types
          `((number number-or-marker atom)
            (symbol atom))))

(defun Emil:Type:basic-subtype? (type other)
  "Returns non-nil, if builtin TYPE is a subtype of OTHER.

TYPE and OTHER should both be basic type-constructors, i.e. symbols."
  (or (eq type other)
      (memq other (cdr (assq type Emil:Type:basic-type-hierarchy)))))

(defun Emil:Type:compound-subtype? (type other)
  "Returns non-nil, if compound TYPE is a subtype of OTHER.

TYPE and OTHER should both be compound type-constructors, i.e.
symbols."
  (or (eq type other)
      (memq other (cdr (assq type Emil:Type:compound-type-hierarchy)))))

(defun Emil:Type:resolve-alias (type)
  "Returns the definition of type-alias TYPE.

Returns nil, if TYPE is not an alias."
  (when-let (name (and (Emil:Type:Basic? type)
                       (Struct:get type :name)))
    (cdr (assq name Emil:Type:type-aliases))))

(defun Emil:Type:trait? (type)
  "Return non `nil', if TYPE represents a trait-type."
  (and (Emil:Type:Compound? type)
       (eq 'Trait (Struct:get type :name))
       (= 1 (length (Struct:get type :arguments)))))

(cl-deftype Cons (head tail)
  `(and list
        (satisfies ,(lambda (cons)
                      (or (null cons)
                          (and (or (null (car cons))
                                   (cl-typep (car cons) head))
                               (or (null (cdr cons))
                                   (cl-typep (cdr cons) tail))))))))

(cl-deftype List (type)
  `(and list
        (satisfies ,(lambda (list)
                      (or (null list)
                          (null (car list))
                          (cl-typep (car list) type))))))

(cl-deftype Sequence (type)
  `(and sequence
        (satisfies ,(lambda (sequence)
                      (or (null sequence)
                          (= 0 (length sequence))
                          (cl-typep (elt sequence 0) type))))))

(cl-deftype Vector (type)
  `(and vector (Sequence ,type)))

(cl-deftype Array (type)
  `(and array (Sequence ,type)))

(provide 'Emil/Type)
