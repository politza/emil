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
(require 'Emil/TypedForm)
(require 'Emil/Error)

;; See "Complete and Easy Bidirectional Typechecking for Higher-Rank
;; Polymorphism"; Jana Dunfield, Neelakantan R. Krishnaswami .

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

(Struct:define Emil
  (generator
   :type Emil:ExistentialGenerator :default (Emil:ExistentialGenerator))
  (messages :type list :mutable t))

(Struct:implement Emil
  (fn Emil:infer (self form (context Emil:Context) (environment (Trait Emil:Env)))
    (condition-case type-error
        (Transformer:transform-form self form context environment)
      (Emil:type-error
       (Emil:add-message
        self (Emil:Message
              :type :error
              :content (error-message-string type-error)
              :form form))
       (cons context (Emil:TypedForm:new form (Emil:Type:Any) environment)))))

  (fn Emil:check (self form type (context Emil:Context)
                       (environment (Trait Emil:Env)))
    (pcase-exhaustive type
      ((Struct Emil:Type:Forall)
       (Emil:check-forall self form type context environment))
      ((and (Struct Emil:Type:Arrow)
            (let `(function ,_) (Transformer:Form:unwrap-n form 1)))
       (Emil:check-lambda self form type context environment))
      (_
       (-let* (((form-context . typed-form)
                (Emil:infer self form context environment))
               (inferred-type
                (Emil:Context:resolve form-context
                                      (Struct:get typed-form :type)))
               (given-type
                (Emil:Context:resolve form-context type)))
         (cons
          (Emil:subtype self form-context inferred-type given-type)
          (Emil:TypedForm* ,@typed-form :type given-type))))))

  (fn Emil:check-forall (self form (type Emil:Type:Forall)
                              (context Emil:Context)
                              (environment (Trait Emil:Env)))
    (-let* ((parameters (Struct:get type :parameters))
            (forall-type (Struct:get type :type))
            (marker (Emil:Context:Marker))
            (intermediate-context
             (Emil:Context:concat (reverse parameters) marker context))
            ((result-context . result-form)
             (Emil:check
              self form forall-type intermediate-context environment)))
      (cons (Emil:Context:drop-until-after result-context marker)
            (Emil:TypedForm* ,@result-form type))))

  (fn Emil:check-lambda (self form (type Emil:Type:Arrow)
                              (context Emil:Context)
                              (environment (Trait Emil:Env)))
    (pcase-exhaustive type
      ((and (Struct Emil:Type:Arrow returns)
            (let `(function ,lambda) form)
            (let `(lambda ,arguments . ,body)
              (Transformer:Form:unwrap-n lambda 2)))
       (unless (Emil:Type:Arrow:arity-assignable-from? type (func-arity lambda))
         (Emil:type-error "Function is not arity compatible: %s, %s"
                          form type))
       (-let* ((argument-count
                (max (length (Emil:Type:Arrow:lambda-variables arguments))
                     (length (Emil:Type:Arrow:arguments type))))
               (arguments-adjusted
                (Emil:Type:Arrow:lambda-adjusted-arguments arguments argument-count))
               (argument-types-adjusted
                (Emil:Type:Arrow:adjusted-arguments type argument-count))
               (bindings
                (--map (Emil:Context:Binding
                        :variable (car it) :type (cdr it))
                       (-zip-pair arguments-adjusted argument-types-adjusted)))
               (marker (Emil:Context:Marker))
               ((body-context . body-forms)
                (Emil:check-do
                 self body returns
                 (Emil:Context:concat (reverse bindings) marker context)
                 environment)))
         (cons (Emil:Context:drop-until-after body-context marker)
               (Emil:TypedForm:new
                (list (car form)
                      (cons (-take 2 lambda) body-forms))
                returns environment))))))

  (fn Emil:instantiate (self (context Emil:Context)
                             (variable Emil:Type:Existential)
                             (type (Trait Emil:Type))
                             (relation
                              (member :less-or-equal :greater-or-equal)))
    ;; Figure 10. Instantiation
    (pcase-exhaustive type
      ((and (pred Emil:Type:monomorph?)
            (let `(,top . ,bottom)
              (Emil:Context:hole context variable))
            (guard (Emil:Context:well-formed? bottom type)))
       (Emil:Context:concat top (Emil:Context:Solution* variable type) bottom))
      ((and (Struct Emil:Type:Existential)
            (let `(,top ,center ,bottom)
              (Emil:Context:double-hole context type variable)))
       (Emil:Context:concat top (Emil:Context:Solution
                                 :variable type :type variable)
                            center variable bottom))
      ((Struct Emil:Type:Arrow)
       (Emil:instantiate-arrow self context variable type relation))
      ((Struct Emil:Type:Forall)
       (Emil:instantiate-forall self context variable type relation))
      ((Struct Emil:Type:Compound)
       (Emil:instantiate-compound self context variable type relation))
      (_
       (Emil:type-error "Unable to instantiate variable for type: %s, %s"
                        variable type))))

  (fn Emil:instantiate-arrow (self (context Emil:Context)
                                   (variable Emil:Type:Existential)
                                   (type Emil:Type:Arrow)
                                   relation)
    (pcase-exhaustive type
      ((and (Struct Emil:Type:Arrow)
            (let `(,top . ,bottom)
              (Emil:Context:hole context variable)))
       (let* ((instance (Emil:arrow-instantiate self type))
              (solution
               (Emil:Context:Solution* variable :type instance))
              (initial-context
               (Emil:Context:concat
                top solution
                (Emil:Type:Arrow:returns instance)
                (reverse (Emil:Type:Arrow:arguments instance))
                bottom))
              (inverse-relation
               (if (eq relation :less-or-equal)
                   :greater-or-equal :less-or-equal))
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
          relation)))))

  (fn Emil:instantiate-forall (self (context Emil:Context)
                                    (variable Emil:Type:Existential)
                                    (type Emil:Type:Forall)
                                    relation)
    (pcase-exhaustive type
      ((and (Struct Emil:Type:Forall parameters type)
            (guard (Emil:Context:member? context variable)))
       (pcase-exhaustive relation
         (:less-or-equal
          (let* ((marker (Emil:Context:Marker))
                 (initial-context
                  (Emil:Context:concat
                   (Emil:Context :entries (reverse parameters))
                   marker context))
                 (intermediate-context (Emil:instantiate
                                        self initial-context
                                        variable type relation)))
            (Emil:Context:drop-until-after intermediate-context marker)))
         (:greater-or-equal
          (let* ((parameters (Emil:Type:Forall:parameters type))
                 (instances
                  (Emil:generate-existentials self (length parameters)))
                 (marker (Emil:Context:Marker))
                 (initial-context
                  (Emil:Context:concat
                   (Emil:Context :entries (reverse instances)) marker context))
                 (intermediate-context
                  (Emil:instantiate
                   self initial-context variable
                   (--reduce-from
                    (Emil:Type:substitute acc (car it) (cdr it))
                    (Emil:Type:Forall:type type)
                    (-zip-pair parameters instances))
                   relation)))
            (Emil:Context:drop-until-after intermediate-context marker)))))))

  (fn Emil:instantiate-compound (self (context Emil:Context)
                                      (variable Emil:Type:Existential)
                                      (type Emil:Type:Compound)
                                      relation)
    (pcase-exhaustive type
      ((and (Struct Emil:Type:Compound parameters)
            (let `(,top . ,bottom)
              (Emil:Context:hole context variable)))
       (let* ((instance (Emil:compound-instantiate self type))
              (solution
               (Emil:Context:Solution* variable :type instance))
              (initial-context
               (Emil:Context:concat
                top solution
                (reverse (Struct:get instance :parameters))
                bottom)))
         (--reduce-from
          (Emil:instantiate self acc (car it) (cdr it) relation)
          initial-context
          (-zip-pair (Struct:get instance :parameters)
                     parameters))))))

  (fn Emil:subtype (self (context Emil:Context)
                         (left (Trait Emil:Type))
                         (right (Trait Emil:Type)))
    ;; Figure 9. Algorithmic subtyping
    (pcase-exhaustive (list left right)
      ((and (or `(,(Struct Emil:Type:Variable)
                  ,(Struct Emil:Type:Variable))
                `(,(Struct Emil:Type:Existential)
                  ,(Struct Emil:Type:Existential)))
            (guard (equal left right)))
       context)
      (`(,(Struct Emil:Type:Compound)
         ,(Struct Emil:Type:Compound))
       (Emil:subtype-compound self context left right))
      (`(,(Struct Emil:Type:Arrow)
         ,(Struct Emil:Type:Arrow))
       (Emil:subtype-arrow self context left right))
      (`(,(Struct Emil:Type:Forall parameters type) ,_)
       (let* ((instances (Emil:generate-existentials
                          self (length parameters)))
              (marker (Emil:Context:Marker))
              (initial-context
               (Emil:Context:concat (reverse instances) marker context))
              (intermediate-context
               (Emil:subtype self initial-context
                             (Emil:Type:substitute-all type parameters instances)
                             right)))
         (Emil:Context:drop-until-after intermediate-context marker)))
      (`(,_ ,(Struct Emil:Type:Forall parameters type))
       (let* ((marker (Emil:Context:Marker))
              (initial-context
               (Emil:Context:concat (reverse parameters) marker context))
              (intermediate-context
               (Emil:subtype self initial-context left type)))
         (Emil:Context:drop-until-after intermediate-context marker)))
      ((and `(,(Struct Emil:Type:Existential name) ,_)
            (guard (not (member name (Emil:Type:free-variables right)))))
       (Emil:instantiate self context left right :less-or-equal))
      ((and `(,_ ,(Struct Emil:Type:Existential name))
            (guard (not (member name (Emil:Type:free-variables left)))))
       (Emil:instantiate self context right left :greater-or-equal))
      (_
       (Emil:subtype-default self context left right))))

  (fn Emil:subtype-arrow (self (context Emil:Context)
                               (left Emil:Type:Arrow)
                               (right Emil:Type:Arrow))
    (pcase-exhaustive (list left right)
      (`(,(Struct Emil:Type:Arrow
                  :arguments left-arguments :returns left-returns)
         ,(Struct Emil:Type:Arrow
                  :arguments right-arguments :returns right-returns
                  :rest? right-rest?))
       (unless (Emil:Type:Arrow:arity-assignable-to? left right)
         (Emil:type-error "Function is not arity-compatible: %s, %s" left right))

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
          (Emil:Context:resolve intermediate-context right-returns))))))

  (fn Emil:subtype-compound (self (context Emil:Context)
                                  (left Emil:Type:Compound)
                                  (right Emil:Type:Compound))
    (pcase-exhaustive (list left right)
      (`(,(Struct Emil:Type:Compound
                  :name left-name :parameters left-parameters)
         ,(Struct Emil:Type:Compound
                  :name right-name :parameters right-parameters))
       (unless (eq left-name right-name)
         (Emil:type-error "%s is not assignable to %s"
                          (Emil:Type:print left)
                          (Emil:Type:print right)))
       (unless (= (length left-parameters)
                  (length right-parameters))
         (Emil:type-error "Types have different parameter counts: %s, %s"
                          (Emil:Type:print left)
                          (Emil:Type:print right)))
       (--reduce-from
        (Emil:subtype self acc
                      (Emil:Context:resolve acc (car it))
                      (Emil:Context:resolve acc (cdr it)))
        context
        (-zip-pair left-parameters right-parameters)))))

  (fn Emil:subtype-default (_self (context Emil:Context)
                                  (left (Trait Emil:Type))
                                  (right (Trait Emil:Type)))
    (unless (or (Emil:Type:Never? left)
                (Emil:Type:Any? right)
                (and (or (Emil:Type:Null? left)
                         (Emil:Type:Void? left))
                     (not (Emil:Type:Never? right)))
                (and (Emil:Type:Basic? left)
                     (Emil:Type:Basic? right)
                     (pcase (list (Struct:get left :name)
                                  (Struct:get right :name))
                       (`(integer number) t)
                       (`(float number) t)
                       (`(,left-name ,right-name)
                        (equal left-name right-name)))))
      (Emil:type-error
       "%s is not assignable to %s"
       (Emil:Type:print left)
       (Emil:Type:print right)))
    context)

  (fn Emil:infer-application (self (arrow-type (Trait Emil:Type))
                                   arguments context environment)
    (pcase-exhaustive arrow-type
      ((Struct Emil:Type:Forall parameters type)
       (let* ((instances (Emil:generate-existentials
                          self (length parameters)))
              (instantiated-type
               (Emil:Type:substitute-all type parameters instances))
              (result-context
               (Emil:Context:concat (reverse instances) context)))
         (Emil:infer-application
          self instantiated-type arguments result-context environment)))
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
               (initial-context
                (Emil:Context:concat
                 top solution
                 (Emil:Context :entries (reverse instantiated-arguments))
                 instantiated-returns
                 bottom))
               ((result-context . argument-forms)
                (Emil:Util:map-reduce
                 (-lambda (context (argument . instance))
                   (Emil:check self argument instance context environment))
                 initial-context
                 (-zip-pair arguments instantiated-arguments))))
         (cons result-context
               (cons instantiated-returns argument-forms))))
      ((Struct Emil:Type:Arrow :arguments argument-types returns)
       (let ((argument-count (length arguments)))
         (unless (Emil:Type:Arrow:arity-assignable-to?
                  arrow-type (cons argument-count argument-count))
           (Emil:type-error "Application is not arity compatible: %d, %s"
                            argument-count arrow-type))
         (-let* ((adjusted-arguments
                  (append arguments (-repeat (- (length argument-types)
                                                (length arguments))
                                             nil)))
                 ((result-context . argument-forms)
                  (Emil:Util:map-reduce
                   (-lambda (context (argument . instance))
                     (Emil:check self argument instance context environment))
                   context
                   (-zip-pair adjusted-arguments argument-types))))
           (cons result-context (cons returns argument-forms)))))
      (_
       (Emil:type-error "Function can not be applied to arguments: %s, %s"
                        (Emil:Type:print arrow-type)
                        arguments))))

  (fn Emil:generate-existential (self)
    "Returns a new `Emil:Type:Existential'."
    (Emil:ExistentialGenerator:next (Struct:get self :generator)))

  (fn Emil:generate-existentials (self count)
    "Returns a list of COUNT new `Emil:Type:Existential's."
    (-map (lambda (_)
            (Emil:generate-existential self))
          (-repeat count nil)))

  (fn Emil:lookup-variable (variable context environment)
    (or (Emil:Context:lookup-variable context variable)
        (Emil:Env:lookup-variable environment variable context)))

  (fn Emil:lookup-function (function context environment)
    (or (Emil:Context:lookup-function context function)
        (Emil:Env:lookup-function environment function context)))

  (fn Emil:arrow-instantiate (self (type Emil:Type:Arrow))
    "Instantiates the function-type with existentials.

Returns a variant of this function in which all types are
replaced with instances of `Emil:Type:Existential'."
    (Emil:Type:Arrow*
     ,@type
     :arguments (Emil:generate-existentials
                 self (length (Emil:Type:Arrow:arguments type)))
     :returns (Emil:generate-existential self)))

  (fn Emil:compound-instantiate (self (type Emil:Type:Compound))
    "Instantiates the compound-type with existentials."
    (Emil:Type:Compound*
     ,@type
     :parameters (Emil:generate-existentials
                  self (length (Struct:get type :parameters)))))

  (fn Emil:infer-do (self forms (context Emil:Context)
                          (environment (Trait Emil:Env)))
    (Emil:Util:map-reduce
     (-lambda (context form)
       (Emil:infer self form context environment))
     context
     forms))

  (fn Emil:check-do (self forms type (context Emil:Context)
                          (environment (Trait Emil:Env)))
    (-let (((context . form)
            (Emil:check self (cons 'progn forms) type context environment)))
      (cons context (cdr (Transformer:Form:value form)))))

  (fn Emil:infer-progn-like (self form (context Emil:Context)
                                  (environment (Trait Emil:Env)))
    (-let* ((body (cdr (Transformer:Form:value form)))
            ((context . forms)
             (Emil:infer-do self body context environment)))
      (cons context (Emil:TypedForm:new
                     (cons (car (Transformer:Form:value form)) forms)
                     (Emil:type-of-body forms)
                     environment))))

  (fn Emil:let*-thread-bindings (self bindings context environment)
    "Thread CONTEXT and ENVIRONMENT through let* BINDINGS."
    (Emil:Util:map-reduce
     (-lambda ((variable-context . variable-environment) (variable binding))
       (-let* (((binding-context . binding-form)
                (Emil:infer
                 self binding variable-context variable-environment))
               (type (Struct:get binding-form :type)))
         (cons (cons (Emil:Context:concat (Emil:Context:Binding* variable type)
                                          binding-context)
                     (Emil:Env:Alist :variables (list (cons variable type))
                                     :parent variable-environment))
               binding-form)))
     (cons context
           (Emil:Env:Alist :parent environment))
     bindings))

  (fn Emil:type-of-body (body-forms)
    (if body-forms
        (Struct:get (-last-item body-forms) :type)
      (Emil:Type:Null)))

  (fn Emil:add-message (self (message Emil:Message))
    (Struct:update self :messages (-rpartial #'append (list message))))

  (fn Emil:has-errors? (self)
    (--some? (eq :error (Struct:get it :type))
             (Struct:get self :messages))))

(Trait:implement Transformer Emil
  (fn Transformer:transform-number (_self form number &optional context environment
                                          &rest _)
    (cons context (Emil:TypedForm:new
                   form
                   (Emil:Type:Basic :name (type-of number))
                   environment)))

  (fn Transformer:transform-string (_self form _string &optional context environment
                                          &rest _)
    (cons context (Emil:TypedForm:new
                   form
                   (Emil:Type:Basic :name 'string)
                   environment)))

  (fn Transformer:transform-vector (_self form vector &optional context environment
                                          &rest _)
    (cons context (Emil:TypedForm:new
                   form
                   (Emil:Type:Basic :name (type-of vector))
                   environment)))

  (fn Transformer:transform-symbol (_self form symbol &optional context environment
                                          &rest _)
    (let ((type (cond
                 ((null symbol)
                  (Emil:Type:Null))
                 ((or (eq symbol t) (keywordp symbol))
                  (Emil:Type:Basic :name 'symbol))
                 (t
                  (or (Emil:lookup-variable symbol context environment)
                      (Emil:type-error "Unbound variable: %s" symbol))))))
      (cons context (Emil:TypedForm:new form type environment))))

  (fn Transformer:transform-and (self form _conditions &optional context environment
                                      &rest _)
    (-let (((context . typed-form)
            (Emil:infer-progn-like self form context environment)))
      (cons context (Emil:TypedForm* ,@typed-form :type (Emil:Type:Any)))))

  (fn Transformer:transform-catch (self form _tag _body &optional context environment
                                        &rest _)
    (-let (((context . typed-form)
            (Emil:infer-progn-like self form context environment)))
      (cons context (Emil:TypedForm* ,@typed-form :type (Emil:Type:Any)))))

  (fn Transformer:transform-cond (self form clauses &optional context environment
                                       &rest _)
    (-let (((context . forms)
            (Emil:Util:map-reduce
             (lambda (context clause)
               (Emil:infer-do self clause context environment))
             context
             clauses)))
      (cons context (Emil:TypedForm:new
                     (cons (car form) forms)
                     (Emil:Type:Any)
                     environment))))

  (fn Transformer:transform-defconst (self form _symbol init-value &optional
                                           _doc-string context environment
                                           &rest _)
    (-let (((context . init-form)
            (Emil:infer self init-value context environment)))
      (cons context (Emil:TypedForm:new
                     (append (-take 2 form)
                             (list init-form)
                             (-drop 3 form))
                     (Emil:Type:Basic :name 'symbol)
                     environment))))

  (fn Transformer:transform-defvar (self form _symbol &optional init-value
                                         _doc-string context environment
                                         &rest _)
    (-let (((context . init-form)
            (Emil:infer self init-value context environment)))
      (cons context (Emil:TypedForm:new
                     (append (-take 2 form)
                             (list init-form)
                             (-drop 3 form))
                     (Emil:Type:Basic :name 'symbol)
                     environment))))

  (fn Transformer:transform-function (self form argument
                                           &optional context environment &rest _)
    (pcase-exhaustive (Transformer:Form:unwrap-n argument 2)
      ((and `(lambda ,argument-list . ,body)
            (guard (listp argument-list)))
       (-let* ((arity (func-arity argument))
               (min-arity (car arity))
               (rest? (eq 'many (cdr arity)))
               (variables
                (Emil:Type:Arrow:lambda-variables argument-list))
               (arguments (Emil:generate-existentials
                           self (length variables)))
               (returns (Emil:generate-existential self))
               (bindings (--map (Emil:Context:Binding
                                 :variable (car it) :type (cdr it))
                                (-zip-pair variables arguments)))
               (marker (Emil:Context:Marker))
               (body-environment (Emil:Env:Alist :parent environment))
               (initial-context
                (Emil:Context:concat
                 (reverse bindings) marker returns
                 (reverse arguments) context))
               ((body-context . body-forms)
                (Emil:check-do self body
                               returns initial-context body-environment))
               (type (Emil:Type:Arrow* arguments returns min-arity rest?)))
         (Emil:Env:Alist:update-from body-environment body-context
                                     variables nil)
         (cons (Emil:Context:drop-until-after body-context marker)
               (Emil:TypedForm:new
                (list (car form)
                      (append (-take 2 (cadr form)) body-forms))
                type environment))))
      ((pred symbolp)
       (let ((type (or (Emil:lookup-function argument context environment)
                       (Emil:type-error "Unbound function: %s" argument))))
         (cons context
               (Emil:TypedForm:new form type environment))))
      (value
       (Emil:type-error "Not a function: %s" value))))

  (fn Transformer:transform-if (self form condition then else
                                     &optional context environment
                                     &rest _)
    (-let (((context . forms)
            (Emil:infer-do self `(,condition ,then ,@else) context environment)))
      (cons context
            (Emil:TypedForm:new
             (cons (car form) forms) (Emil:Type:Any) environment))))

  (fn Transformer:transform-interactive (_self form _descriptor _modes
                                               &optional context environment
                                               &rest _)
    (cons context (Emil:TypedForm:new form (Emil:Type:Any) environment)))

  (fn Transformer:transform-let (self form bindings body &optional
                                      context environment &rest _)
    (-let* ((variables (--map (Transformer:Form:value (car it)) bindings))
            ((bindings-context . binding-forms)
             (Emil:Util:map-reduce
              (-lambda (context (_variable binding))
                (Emil:infer self binding context environment))
              context
              bindings))
            (context-bindings
             (--map (Emil:Context:Binding
                     :variable (car it) :type (Struct:get (cdr it) :type))
                    (-zip-pair variables binding-forms)))
            (marker (Emil:Context:Marker))
            (body-environment (Emil:Env:Alist :parent environment))
            ((body-context . body-forms)
             (Emil:infer-do
              self body (Emil:Context:concat
                         (reverse context-bindings)
                         marker bindings-context)
              body-environment))
            (body-type (Emil:type-of-body body-forms)))
      (Emil:Env:Alist:update-from body-environment body-context
                                  variables nil)
      (cons (Emil:Context:drop-until-after body-context marker)
            (Emil:TypedForm:new
             (cons (car form)
                   (cons (-zip-with #'list variables binding-forms)
                         body-forms))
             (Emil:Context:resolve body-context body-type)
             environment))))

  (fn Transformer:transform-let* (self form bindings body &optional
                                       context environment &rest _)
    (-let* ((marker (Emil:Context:Marker))
            (variables (--map (Transformer:Form:value (car it)) bindings))
            (((bindings-context . body-environment) . binding-forms)
             (Emil:let*-thread-bindings
              self bindings (Emil:Context:concat marker context) environment))
            ((body-context . body-forms)
             (Emil:infer-do self body bindings-context body-environment))
            (body-type (Emil:type-of-body body-forms)))
      (--each (nreverse (--iterate (Struct:get it :parent)
                                   body-environment
                                   (length variables)))
        (Emil:Env:Alist:update-from
         it body-context (list (nth it-index variables)) nil t))
      (cons (Emil:Context:drop-until-after body-context marker)
            (Emil:TypedForm:new
             (cons (car form)
                   (cons (-zip-with #'list variables binding-forms)
                         body-forms))
             (Emil:Context:resolve body-context body-type)
             environment))))

  (fn Transformer:transform-or (self form _conditions &optional context environment
                                     &rest _)
    (-let (((context . typed-form)
            (Emil:infer-progn-like self form context environment)))
      (cons context (Emil:TypedForm* ,@typed-form :type (Emil:Type:Any)))))

  (fn Transformer:transform-prog1 (self form _first _body
                                        &optional context environment
                                        &rest _)
    (-let* (((context . typed-form)
             (Emil:infer-progn-like self form context environment))
            (type (Struct:get (nth 1 (Struct:get typed-form :form)) :type)))
      (cons context (Emil:TypedForm* ,@typed-form type))))

  (fn Transformer:transform-progn (self form _body &optional context environment
                                        &rest _)
    (Emil:infer-progn-like self form context environment))

  (fn Transformer:transform-quote (_self form _argument
                                         &optional context environment &rest _)
    (cons context (Emil:TypedForm:new form (Emil:Type:Any) environment)))

  (fn Transformer:transform-save-current-buffer (self form _body
                                                      &optional context environment
                                                      &rest _)
    (Emil:infer-progn-like self form context environment))

  (fn Transformer:transform-save-excursion (self form _body
                                                 &optional context environment
                                                 &rest _)
    (Emil:infer-progn-like self form context environment))

  (fn Transformer:transform-save-restriction (self form _body
                                                   &optional context environment
                                                   &rest _)
    (Emil:infer-progn-like self form context environment))

  (fn Transformer:transform-setq (_self form _definitions
                                        &optional context environment
                                        &rest _)
    (cons context (Emil:TypedForm:new
                   form
                   (Emil:Type:Any)
                   environment)))

  (fn Transformer:transform-unwind-protect (self form _body-form _unwind-forms
                                                 &optional context environment
                                                 &rest _)
    (-let* (((context . typed-form)
             (Emil:infer-progn-like self form context environment))
            (type (Struct:get (nth 1 (Struct:get typed-form :form)) :type)))
      (cons context (Emil:TypedForm* ,@typed-form type))))

  (fn Transformer:transform-while (self form _condition _body
                                        &optional context environment &rest _)
    (Emil:infer-progn-like self form context environment))

  (fn Transformer:transform-application (self _form function arguments
                                              &optional context environment
                                              &rest _)

    (-let* (((function-context . function-form)
             (Transformer:transform-function
              self `(function ,function) function context environment))
            ((context return-type . argument-forms)
             (Emil:infer-application
              self (Emil:Context:resolve
                    function-context (Struct:get function-form :type))
              arguments
              function-context environment)))
      (cons context
            (Emil:TypedForm:new
             (cons (nth 1 (Struct:get function-form :form))
                   argument-forms)
             return-type environment))))

  (fn Transformer:transform-macro (self form macro arguments
                                        &optional context environment &rest _)
    (cond
     ((eq (Transformer:Form:value macro) 'Emil:is)
      (unless (= 2 (length arguments))
        (Emil:type-error "Syntax error: Emil:is: %s" arguments))
      (-let* ((type (Emil:Type:read
                     (Transformer:Form:value (nth 0 arguments)))))
        (Emil:check self (nth 1 arguments) type context environment)))
     (t
      (Emil:infer
       self (macroexpand (Transformer:Form:unwrap form)) context environment)))))

(defun Emil:infer-type (form &optional environment)
  (-let* ((emil (Emil))
          ((context . typed-form)
           (Emil:infer emil form (Emil:Context)
                       (Emil:Env:Alist :parent environment))))
    (when (Emil:has-errors? emil)
      (signal 'Emil:type-error (list (Struct:get emil :messages))))
    (-> (Emil:Context:resolve context (Struct:get typed-form :type))
        Emil:Type:normalize
        Emil:Type:print)))

(defun Emil:transform (form &optional environment)
  (-let* ((emil (Emil))
          ((context . typed-form)
           (Emil:infer emil form (Emil:Context)
                       (Emil:Env:Alist :parent environment))))
    (when (Emil:has-errors? emil)
      (signal 'Emil:type-error (list (Struct:get emil :messages))))
    (Emil:TypedForm*
     ,@typed-form
     :type (Emil:Context:resolve context (Struct:get typed-form :type)))))

(provide 'Emil)
;;; Emil.el ends here
