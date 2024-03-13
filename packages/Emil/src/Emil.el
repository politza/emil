;;; Emil.el --- Emacs mit Inferenz Logik.             -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Andreas Politz

;; Author: Andreas Politz <mail@andreas-politz.de>
;; Keywords: extensions
;; Version: 1.0.0beta1
;; Package-Requires: ((emacs "29.1") (dash "2.19.1") (Struct "1.0.0beta1") (Commons "1.0.0beta1") (Transformer "1.0.0beta1"))

(require 'dash)
(require 'pcase)
(require 'Struct)
(require 'Trait)
(require 'Transformer)
(require 'Emil/Type)
(require 'Emil/Context)
(require 'Emil/Message)
(require 'Emil/Util)
(require 'Emil/Env)

(Struct:define Emil:ExistentialGenerator
  "Generator for instances of type `Emil:Type:Existential'."
  (generator :default (Emil:Util:NameGenerator)))

(defun Emil:ExistentialGenerator:next (self)
  (Emil:Type:Existential
   :name (Emil:Util:NameGenerator:next (Struct:get self :generator))))

(defmacro Emil:is (_type form)
  "Declare that FORM is of type TYPE.

Apart from that, this just expands to FORM.

\(fn TYPE FORM\)"
  (declare (indent 1))
  form)

(Struct:define Emil:TypedForm
  (form
   "The encapsulated form.")
  (source
   "The source location of the form.")
  (type
   "The type of the form")
  (context
   "The context pertaining to this form."))

(Struct:define Emil
  (environment
   "The environment for looking up non-local variables and functions."
   :type (Trait Emil:Env) :default (Emil:Env:empty))
  (generator
   :type Emil:ExistentialGenerator :default (Emil:ExistentialGenerator))
  (messages :type list))

(Struct:implement Emil
  (fn Emil:infer (self form (context Emil:Context))
    (Transformer:transform-form self form context))

  (fn Emil:check (self form type (context Emil:Context))
    (pcase type
      ((Struct Emil:Type:Forall parameters :type forall-type)
       (let* ((marker (Emil:Context:Marker))
              (intermediate-context
               (Emil:Context:concat (reverse parameters) marker context))
              (result-context
               (Emil:check self form forall-type intermediate-context)))
         (Emil:Context:drop-until-after result-context marker)))
      ((and (Struct Emil:Type:Arrow returns)
            (let `(function (lambda ,arguments . ,body)) form))
       (unless (Emil:Type:Arrow:arity-assignable-from? type (func-arity form))
         (error "Function is not arity compatible: %s, %s" form type))
       (let* ((argument-count
               (max (--count (not (memq it '(&optional &rest))) arguments)
                    (Emil:Type:Arrow:arguments type)))
              (arguments-adjusted
               (Emil:Type:Arrow:lambda-adjusted-arguments arguments argument-count))
              (argument-types-adjusted
               (Emil:Type:Arrow:adjusted-arguments type argument-count))
              (bindings
               (--map (Emil:Context:Binding
                       :variable (car it) :type (cdr it))
                      (-zip-pair arguments-adjusted argument-types-adjusted)))
              (marker (Emil:Context:Marker))
              (intermediate-context
               (Emil:Context:concat (reverse bindings) marker context))
              (result-context (Emil:check self body returns intermediate-context)))
         (Emil:Context:drop-until-after result-context marker)))
      (_
       (-let (((inferred-type . intermediate-context)
               (Emil:infer self form context)))
         (Emil:subtype
          self intermediate-context
          (Emil:Context:resolve intermediate-context inferred-type)
          (Emil:Context:resolve intermediate-context type))))))

  (fn Emil:generate-existential (self)
    "Returns a new `Emil:Type:Existential'."
    (Emil:ExistentialGenerator:next (Struct:get self :generator)))

  (fn Emil:generate-existentials (self count)
    "Returns a list of COUNT new `Emil:Type:Existential's."
    (-map (lambda (_)
            (Emil:generate-existential self))
          (-repeat count nil)))

  (fn Emil:lookup-variable (self context variable)
    (or (Emil:Context:lookup-variable context variable)
        (Emil:Env:lookup-variable (Struct:get self :environment)
                                  variable context)))

  (fn Emil:lookup-function (self context function)
    (or (Emil:Context:lookup-function context function)
        (Emil:Env:lookup-function (Struct:get self :environment)
                                  function context)))

  (fn Emil:instantiate-arrow (self (type Emil:Type:Arrow))
    "Instantiates the function-type with existentials.

Returns a variant of this function in which all types are
replaced with instances of `Emil:Type:Existential'."
    (Emil:Type:Arrow*
     ,@type
     :arguments (Emil:generate-existentials
                 self (length (Emil:Type:Arrow:arguments type)))
     :returns (Emil:generate-existential self)))

  (fn Emil:instantiate (self (context Emil:Context)
                             (variable Emil:Type:Existential)
                             (type (Trait Emil:Type))
                             (relation (member :less-or-equal :greater-or-equal)))
    (let ((inverse-relation
           (if (eq relation :less-or-equal) :greater-or-equal :less-or-equal)))
      ;; Figure 10. Instantiation
      (pcase-exhaustive type
        ;; InstLSolve / InstRSolve
        ((and (pred Emil:Type:monomorph?)
              (let `(,top . ,bottom)
                (Emil:Context:hole context variable))
              (guard (Emil:Context:well-formed? bottom type)))
         (Emil:Context:concat
          top (Emil:Context:Solution* variable type) bottom))
        ;; InstLReach / InstRReach
        ((and (Struct Emil:Type:Existential)
              (let `(,top ,center ,bottom)
                (Emil:Context:double-hole context type variable)))
         (Emil:Context:concat top (Emil:Context:Solution
                                   :variable type :type variable)
                              center variable bottom))
        ;; InstLArr / InstRArr
        ((and (Struct Emil:Type:Arrow)
              (let `(,top . ,bottom)
                (Emil:Context:hole context variable)))
         (let* ((instance (Emil:instantiate-arrow self type))
                (solved-var-inst
                 (Emil:Context:Solution* variable :type instance))
                (initial-context
                 (Emil:Context:concat
                  top solved-var-inst
                  (Emil:Type:Arrow:returns instance)
                  (reverse (Emil:Type:Arrow:arguments instance))
                  bottom))
                (intermediate-context
                 (--reduce-from
                  (Emil:instantiate self acc (car it) (cdr it)
                                    inverse-relation)
                  initial-context
                  (-zip-pair (Emil:Type:Arrow:arguments instance)
                             (Emil:Type:Arrow:arguments type)))))
           (Emil:instantiate
            self
            intermediate-context
            (Struct:get instance :returns)
            (Emil:Context:resolve intermediate-context
                                  (Struct:get type :returns))
            relation)))
        ((and (Struct Emil:Type:Forall parameters type)
              (guard (Emil:Context:member? context variable)))
         (pcase-exhaustive relation
           (:less-or-equal
            ;; InstLAllR
            (let* ((marker (Emil:Context:Marker))
                   (initial-context (Emil:Context:concat
                                     (Emil:Context :entries (reverse parameters))
                                     marker context))
                   (intermediate-context (Emil:instantiate
                                          self initial-context
                                          variable type relation)))
              (Emil:Context:drop-until-after intermediate-context marker)))
           (:greater-or-equal
            ;; InstRAllL
            (let* ((parameters (Emil:Type:Forall:parameters type))
                   (instances (Emil:generate-existentials self (length parameters)))
                   (marker (Emil:Context:Marker))
                   (initial-context
                    (Emil:Context:concat
                     (Emil:Context :entries (reverse instances)) marker context))
                   (intermediate-context
                    (Emil:instantiate
                     self
                     initial-context
                     variable
                     (--reduce-from
                      (Emil:Type:substitute acc (car it) (cdr it))
                      (Emil:Type:Forall:type type)
                      (-zip-pair parameters instances))
                     relation)))
              (Emil:Context:drop-until-after intermediate-context marker))))))))

  (fn Emil:subtype (self (context Emil:Context)
                         (left (Trait Emil:Type))
                         (right (Trait Emil:Type)))
    ;; Figure 9. Algorithmic subtyping
    (pcase-exhaustive (list left right)
      ;; Var / Exvar
      ((and (or `(,(Struct Emil:Type:Variable)
                  ,(Struct Emil:Type:Variable))
                `(,(Struct Emil:Type:Existential)
                  ,(Struct Emil:Type:Existential)))
            (guard (equal left right)))
       context)
      ;; Arrow
      (`(,(Struct Emil:Type:Arrow
                  :arguments left-arguments :returns left-returns)
         ,(Struct Emil:Type:Arrow
                  :arguments right-arguments :returns right-returns
                  :rest? right-rest?))
       (unless (Emil:Type:Arrow:arity-assignable-to? left right)
         (error "Function is not arity-compatible: %s, %s" left right))

       (let* ((argument-count (if right-rest?
                                  (max (length left-arguments)
                                       (length right-arguments))
                                (length right-arguments)))
              (left-arguments-adjusted (Emil:Type:Arrow:adjusted-arguments
                                        left argument-count))
              (right-arguments-adjusted (Emil:Type:Arrow:adjusted-arguments
                                         right argument-count))
              (intermediate-context
               (--reduce-from
                (Emil:subtype self acc
                              (Emil:Context:resolve acc (car it))
                              (Emil:Context:resolve acc (cdr it)))
                context
                (-zip-pair right-arguments-adjusted
                           left-arguments-adjusted))))
         (Emil:subtype
          self intermediate-context
          (Emil:Context:resolve intermediate-context left-returns)
          (Emil:Context:resolve intermediate-context right-returns))))
      ;; Forall L
      (`(,(Struct Emil:Type:Forall parameters type) ,_)
       (let* ((instances (Emil:generate-existentials
                          self (length parameters)))
              (marker (Emil:Context:Marker))
              (initial-context
               (Emil:Context:concat
                (reverse instances)
                marker
                context))
              (intermediate-context
               (Emil:subtype
                self initial-context
                (Emil:Type:substitute-all type parameters instances)
                right)))
         (Emil:Context:drop-until-after intermediate-context marker)))
      ;; Forall R
      (`(,_ ,(Struct Emil:Type:Forall parameters type))
       (let* ((marker (Emil:Context:Marker))
              (initial-context
               (Emil:Context:concat
                (reverse parameters)
                marker
                context))
              (intermediate-context
               (Emil:subtype self initial-context left type)))
         (Emil:Context:drop-until-after intermediate-context marker)))
      ;; Instantiate L
      ((and `(,(Struct Emil:Type:Existential name) ,_)
            (guard (not (member name (Emil:Type:free-variables right)))))
       (Emil:instantiate self context left right :less-or-equal))
      ;; Instantiate R
      ((and `(,_ ,(Struct Emil:Type:Existential name))
            (guard (not (member name (Emil:Type:free-variables left)))))
       (Emil:instantiate self context right left :greater-or-equal))
      ;; Unit
      ((or `(,(Struct Emil:Type:Never) ,_)
           `(,_ ,(Struct Emil:Type:Any))
           (and `(,(Struct Emil:Type:Null) ,_)
                (guard (not (Emil:Type:Never? right))))
           (and `(,(Struct Emil:Type:Void) ,_)
                (guard (not (Emil:Type:Never? right))))
           (and `(,(Struct Emil:Type:Basic :name left-name)
                  ,(Struct Emil:Type:Basic :name right-name))
                (guard (pcase (list left-name right-name)
                         (`(integer number) t)
                         (`(float number) t)
                         ((guard (equal left-name right-name)) t)))))
       context)))

  (fn Emil:infer-do (self (context Emil:Context) forms)
    (--reduce-from (Transformer:transform-form self it (cdr acc))
                   (cons (Emil:Type:Null) context)
                   forms))

  (fn Emil:infer-application (self context arrow-type arguments)
    (pcase-exhaustive arrow-type
      ((Struct Emil:Type:Forall parameters type)
       (let* ((instances (Emil:generate-existentials
                          self (length parameters)))
              (instantiated-type
               (Emil:Type:substitute-all type parameters instances))
              (result-context
               (Emil:Context:concat (reverse instances) context)))
         (Emil:infer-application
          self result-context instantiated-type arguments)))
      ((Struct Emil:Type:Existential)
       (-let* ((instantiated-type (Emil:Type:Arrow
                                   :arguments (Emil:generate-existentials
                                               self (length arguments))
                                   :returns (Emil:generate-existential self)
                                   :min-arity (length arguments)))
               (instantiated-arguments
                (Emil:Type:Arrow:arguments instantiated-type))
               (instantiated-returns
                (Emil:Type:Arrow:returns instantiated-type))
               (solution (Emil:Context:Solution
                          :variable arrow-type :type instantiated-type))
               ((top . bottom)
                (Emil:Context:hole context arrow-type))
               (result-context
                (Emil:Context:concat
                 top solution
                 (Emil:Context :entries (reverse instantiated-arguments))
                 instantiated-returns
                 bottom)))
         (cons instantiated-returns
               (--reduce-from
                (Emil:check self (car it) (cdr it) acc)
                result-context
                (-zip-pair arguments instantiated-arguments)))))
      ((Struct Emil:Type:Arrow :arguments argument-types returns)
       (let ((argument-count (length arguments)))
         (unless (Emil:Type:Arrow:arity-assignable-to?
                  arrow-type (cons argument-count argument-count))
           (error "Application is not arity compatible: %d, %s"
                  argument-count arrow-type))
         (let ((adjusted-arguments
                (append arguments (-repeat (- (length argument-types)
                                              (length arguments))
                                           nil))))
           (cons returns
                 (--reduce-from
                  (Emil:check self (car it) (cdr it) acc)
                  context
                  (-zip-pair adjusted-arguments argument-types)))))))))

(Trait:implement Transformer Emil
  (fn Transformer:transform-number (_self _form number &optional context &rest _)
    (cons (Emil:Type:Basic :name (type-of number)) context))

  (fn Transformer:transform-string (_self _form string &optional context &rest _)
    (cons (Emil:Type:Basic :name (type-of string)) context))

  (fn Transformer:transform-vector (_self _form vector &optional context &rest _)
    (cons (Emil:Type:Basic :name (type-of vector)) context))

  (fn Transformer:transform-symbol (self _form symbol &optional context &rest _)
    (cond
     ((null symbol)
      (cons (Emil:Type:Null) context))
     ((or (eq symbol t) (keywordp symbol))
      (cons (Emil:Type:Basic :name 'symbol) context))
     (t
      (if-let (type (Emil:lookup-variable self context symbol))
          (cons type context)
        (error "Unbound variable: %s" symbol)))))

  (fn Transformer:transform-and (self _form conditions &optional context &rest _)
    (cons (Emil:Type:Any)
          (cdr (Emil:infer-do self context conditions))))

  (fn Transformer:transform-catch (self _form _tag body &optional context &rest _)
    (cons (Emil:Type:Any)
          (cdr (Emil:infer-do self context body))))

  (fn Transformer:transform-cond (self _form clauses &optional context &rest _)
    (cons (Emil:Type:Any)
          (cdr (--reduce-from
                (Emil:infer-do
                 self (cdr (Transformer:transform-form self (car it) (cdr acc)))
                 (cdr it))
                (cons (Emil:Type:Null) context) clauses))))

  (fn Transformer:transform-defconst (self _form _symbol init-value &optional
                                           _doc-string context &rest _)
    (cons (Emil:Type:Basic :name 'symbol)
          (cdr (Transformer:transform-form self init-value context))))

  (fn Transformer:transform-defvar (self _form _symbol &optional init-value
                                         _doc-string context &rest _)
    (cons (Emil:Type:Basic :name 'symbol)
          (cdr (Transformer:transform-form self init-value context))))

  (fn Transformer:transform-function (self _form argument
                                           &optional context &rest _)
    (pcase-exhaustive argument
      ((and `(lambda ,argument-list . ,body)
            (guard (listp argument-list)))
       (let* ((arity (func-arity argument))
              (min-arity (car arity))
              (rest? (eq 'many (cdr arity)))
              (filtered-argument-list
               (--filter (not (memq it '(&optional &rest)))
                         argument-list))
              (arguments (Emil:generate-existentials
                          self (length filtered-argument-list)))
              (returns (Emil:generate-existential self))
              (bindings (--map (Emil:Context:Binding
                                :variable (car it) :type (cdr it))
                               (-zip-pair filtered-argument-list arguments)))
              (marker (Emil:Context:Marker))
              (initial-context
               (Emil:Context:concat
                (reverse bindings) marker returns
                (reverse arguments) context))
              (intermediate-context
               (Emil:check self (cons 'progn body) returns initial-context)))
         (cons (Emil:Type:Arrow*
                arguments returns min-arity rest?)
               (Emil:Context:drop-until-after intermediate-context marker))))
      ((pred symbolp)
       (if-let (type (Emil:lookup-function self context argument))
           (cons type context)
         (error "Unbound function: %s" argument)))))

  (fn Transformer:transform-if (self _form condition then else
                                     &optional context &rest _)
    (cons (Emil:Type:Any)
          (cdr (Emil:infer-do self context `(,condition ,then ,@else)))))

  (fn Transformer:transform-interactive (_self _form _descriptor _modes
                                               &optional context &rest _)
    (cons (Emil:Type:Any) context))

  (fn Transformer:transform-let (self _form bindings body &optional context &rest _)
    (-let* (((binding-context . binding-types)
             (--reduce-from
              (-let (((type . intermediate-context)
                      (Transformer:transform-form self (cadr it) (car acc))))
                (cons intermediate-context (cons type (cdr acc))))
              (list context)
              bindings))
            (context-bindings
             (--map (Emil:Context:Binding
                     :variable (car it) :type (cdr it))
                    (-zip-pair (-map #'car bindings)
                               (reverse binding-types))))
            (marker (Emil:Context:Marker))
            (body-context
             (Emil:Context:concat
              (reverse context-bindings)
              marker binding-context))
            ((body-type . result-context)
             (Emil:infer-do self body-context body)))
      (cons (Emil:Context:resolve result-context body-type)
            (Emil:Context:drop-until-after result-context marker))))

  (fn Transformer:transform-let* (self _form bindings body &optional context &rest _)
    (-let* ((marker (Emil:Context:Marker))
            (body-context
             (--reduce-from
              (-let (((type . accumulated-context)
                      (Transformer:transform-form self (cadr it) acc)))
                (Emil:Context:concat
                 (Emil:Context:Binding :variable (car it) :type type)
                 accumulated-context))
              (Emil:Context:concat marker context)
              bindings))
            ((body-type . result-context)
             (Emil:infer-do self body-context body)))
      (cons (Emil:Context:resolve result-context body-type)
            (Emil:Context:drop-until-after result-context marker))))

  (fn Transformer:transform-or (self _form conditions &optional context &rest _)
    (cons (Emil:Type:Any)
          (cdr (Emil:infer-do self context conditions))))

  (fn Transformer:transform-prog1 (self _form first body &optional context &rest _)
    (-let (((first-type . first-context)
            (Transformer:transform-form self first context)))
      (cons first-type
            (cdr (Emil:infer-do self first-context body)))))

  (fn Transformer:transform-progn (self _form body &optional context &rest _)
    (Emil:infer-do self context body))

  (fn Transformer:transform-quote (_self _form _argument &optional context &rest _)
    (cons (Emil:Type:Any) context))

  (fn Transformer:transform-save-current-buffer (self _form body
                                                      &optional context &rest _)
    (Emil:infer-do self context body))

  (fn Transformer:transform-save-excursion (self _form body
                                                 &optional context &rest _)
    (Emil:infer-do self context body))

  (fn Transformer:transform-save-restriction (self _form body
                                                   &optional context &rest _)
    (Emil:infer-do self context body))

  (fn Transformer:transform-setq (_self _form _definitions
                                        &optional context &rest _)
    (cons (Emil:Type:Any) context))

  (fn Transformer:transform-unwind-protect (self _form body-form unwind-forms
                                                 &optional context &rest _)
    (prog1 (Transformer:transform-form self body-form context)
      (Emil:infer-do self context unwind-forms)))

  (fn Transformer:transform-while (self _form condition body
                                        &optional context &rest _)
    (Emil:infer-do self context (cons condition body)))

  (fn Transformer:transform-application (self form function arguments
                                              &optional context &rest _)

    (-let (((arrow-type . result-context)
            (Transformer:transform-function self form function context)))
      (Emil:infer-application
       self result-context
       (Emil:Context:resolve result-context arrow-type)
       arguments)))

  (fn Transformer:transform-macro (self _form macro arguments
                                        &optional context &rest _)
    (cond
     ((eq (Transformer:Form:value macro) 'Emil:is)
      (unless (= 2 (length arguments))
        (error "Syntax error: Emil:is: %s" arguments))
      (let ((type (Emil:Type:read (Transformer:Form:value (nth 0 arguments)))))
        (cons type
              (Emil:check self (Transformer:Form:value (nth 1 arguments))
                          type context))))
     (t
      (Transformer:transform-form
       self
       (macroexpand (-map #'Transformer:Form:value (cons macro arguments)))
       context)))))

(defun Emil:infer-form (form &optional environment)
  (-let* (((type . context)
           (Emil:infer (Emil* environment) form (Emil:Context))))
    (Emil:Type:print-normalized (Emil:Context:resolve context type))))

(provide 'Emil)
;;; Emil.el ends here
