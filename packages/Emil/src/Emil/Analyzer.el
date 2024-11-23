;; -*- lexical-binding: t -*-

(require 'dash)
(require 'Struct)
(require 'Emil/Util)
(require 'Emil/Context)
(require 'Emil/Type)
(require 'Emil/Message)

(Struct:define Emil:Analyzer
  (generator
   :type Emil:Util:ExistentialGenerator :default (Emil:Util:ExistentialGenerator))
  (messages :type list :mutable t))

(Struct:implement Emil:Analyzer
  (fn Emil:Analyzer:infer (self form (context Emil:Context)
                                (environment (Trait Emil:Env)))
    (condition-case type-error
        (Transformer:transform-form self form context environment)
      (Emil:type-error
       (Emil:Analyzer:add-message
        self (Emil:Message
              :type :error
              :content (error-message-string type-error)
              :form form))
       (cons context (Emil:TypedForm:new form (Emil:Type:Any) environment)))))

  (fn Emil:Analyzer:check (self form type (context Emil:Context)
                                (environment (Trait Emil:Env)))
    (pcase-exhaustive type
      ((Struct Emil:Type:Forall)
       (Emil:Analyzer:check-forall self form type context environment))
      ((and (Struct Emil:Type:Arrow)
            (let `(function ,_) (Transformer:Form:unwrap-n form 1)))
       (Emil:Analyzer:check-lambda self form type context environment))
      (_
       (-let* (((form-context . typed-form)
                (Emil:Analyzer:infer self form context environment))
               (inferred-type
                (Emil:Context:resolve form-context
                                      (Struct:get typed-form :type)))
               (given-type
                (Emil:Context:resolve form-context type)))
         (cons
          (Emil:Analyzer:subtype self form-context inferred-type given-type)
          (Emil:TypedForm* ,@typed-form :type given-type))))))

  (fn Emil:Analyzer:check-forall (self form (type Emil:Type:Forall)
                                       (context Emil:Context)
                                       (environment (Trait Emil:Env)))
    (-let* ((parameters (Struct:get type :parameters))
            (forall-type (Struct:get type :type))
            (marker (Emil:Context:Marker))
            (intermediate-context
             (Emil:Context:concat (reverse parameters) marker context))
            ((result-context . result-form)
             (Emil:Analyzer:check
              self form forall-type intermediate-context environment)))
      (cons (Emil:Context:drop-until-after result-context marker)
            (Emil:TypedForm* ,@result-form type))))

  (fn Emil:Analyzer:check-lambda (self form (type Emil:Type:Arrow)
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
                (Emil:Analyzer:check-do
                 self body returns
                 (Emil:Context:concat (reverse bindings) marker context)
                 environment)))
         (cons (Emil:Context:drop-until-after body-context marker)
               (Emil:TypedForm:new
                (list (car form)
                      (cons (-take 2 lambda) body-forms))
                returns environment))))))

  (fn Emil:Analyzer:instantiate (self (context Emil:Context)
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
       (Emil:Analyzer:instantiate-arrow self context variable type relation))
      ((Struct Emil:Type:Forall)
       (Emil:Analyzer:instantiate-forall self context variable type relation))
      ((Struct Emil:Type:Compound)
       (Emil:Analyzer:instantiate-compound self context variable type relation))
      (_
       (Emil:type-error "Unable to instantiate variable for type: %s, %s"
                        variable type))))

  (fn Emil:Analyzer:instantiate-arrow (self (context Emil:Context)
                                            (variable Emil:Type:Existential)
                                            (type Emil:Type:Arrow)
                                            relation)
    (pcase-exhaustive type
      ((and (Struct Emil:Type:Arrow)
            (let `(,top . ,bottom)
              (Emil:Context:hole context variable)))
       (let* ((instance (Emil:Analyzer:arrow-instantiate self type))
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
                (Emil:Analyzer:instantiate self acc (car it) (cdr it)
                                           inverse-relation)
                initial-context
                (-zip-pair (Emil:Type:Arrow:arguments instance)
                           (Emil:Type:Arrow:arguments type)))))
         (Emil:Analyzer:instantiate
          self
          intermediate-context
          (Struct:get instance :returns)
          (Emil:Context:resolve intermediate-context
                                (Struct:get type :returns))
          relation)))))

  (fn Emil:Analyzer:instantiate-forall (self (context Emil:Context)
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
                 (intermediate-context (Emil:Analyzer:instantiate
                                        self initial-context
                                        variable type relation)))
            (Emil:Context:drop-until-after intermediate-context marker)))
         (:greater-or-equal
          (let* ((parameters (Emil:Type:Forall:parameters type))
                 (instances
                  (Emil:Analyzer:generate-existentials self (length parameters)))
                 (marker (Emil:Context:Marker))
                 (initial-context
                  (Emil:Context:concat
                   (Emil:Context :entries (reverse instances)) marker context))
                 (intermediate-context
                  (Emil:Analyzer:instantiate
                   self initial-context variable
                   (--reduce-from
                    (Emil:Type:substitute acc (car it) (cdr it))
                    (Emil:Type:Forall:type type)
                    (-zip-pair parameters instances))
                   relation)))
            (Emil:Context:drop-until-after intermediate-context marker)))))))

  (fn Emil:Analyzer:instantiate-compound (self (context Emil:Context)
                                               (variable Emil:Type:Existential)
                                               (type Emil:Type:Compound)
                                               relation)
    (pcase-exhaustive type
      ((and (Struct Emil:Type:Compound parameters)
            (let `(,top . ,bottom)
              (Emil:Context:hole context variable)))
       (let* ((instance (Emil:Analyzer:compound-instantiate self type))
              (solution
               (Emil:Context:Solution* variable :type instance))
              (initial-context
               (Emil:Context:concat
                top solution
                (reverse (Struct:get instance :parameters))
                bottom)))
         (--reduce-from
          (Emil:Analyzer:instantiate self acc (car it) (cdr it) relation)
          initial-context
          (-zip-pair (Struct:get instance :parameters)
                     parameters))))))

  (fn Emil:Analyzer:subtype (self (context Emil:Context)
                                  (left (Trait Emil:Type))
                                  (right (Trait Emil:Type)))
    ;; Figure 9. Algorithmic subtyping
    (pcase-exhaustive (list left right)
      ((or `(,(Struct Emil:Type:Any) ,_)
           `(,_ ,(Struct Emil:Type:Any)))
       context)
      ((and (or `(,(Struct Emil:Type:Variable)
                  ,(Struct Emil:Type:Variable))
                `(,(Struct Emil:Type:Existential)
                  ,(Struct Emil:Type:Existential)))
            (guard (equal left right)))
       context)
      (`(,(Struct Emil:Type:Arrow)
         ,(Struct Emil:Type:Arrow))
       (Emil:Analyzer:subtype-arrow self context left right))
      (`(,(Struct Emil:Type:Forall parameters type) ,_)
       (let* ((instances (Emil:Analyzer:generate-existentials
                          self (length parameters)))
              (marker (Emil:Context:Marker))
              (initial-context
               (Emil:Context:concat (reverse instances) marker context))
              (intermediate-context
               (Emil:Analyzer:subtype
                self initial-context
                (Emil:Type:substitute-all type parameters instances)
                right)))
         (Emil:Context:drop-until-after intermediate-context marker)))
      (`(,_ ,(Struct Emil:Type:Forall parameters type))
       (let* ((marker (Emil:Context:Marker))
              (initial-context
               (Emil:Context:concat (reverse parameters) marker context))
              (intermediate-context
               (Emil:Analyzer:subtype self initial-context left type)))
         (Emil:Context:drop-until-after intermediate-context marker)))
      ((and `(,(Struct Emil:Type:Existential name) ,_)
            (guard (not (member name (Emil:Type:free-variables right)))))
       (Emil:Analyzer:instantiate self context left right :less-or-equal))
      ((and `(,_ ,(Struct Emil:Type:Existential name))
            (guard (not (member name (Emil:Type:free-variables left)))))
       (Emil:Analyzer:instantiate self context right left :greater-or-equal))
      ((or `(,(Struct Emil:Type:Compound) ,_)
           `(,_ ,(Struct Emil:Type:Compound)))
       (Emil:Analyzer:subtype-compound self context left right))
      (_
       (Emil:Analyzer:subtype-default self context left right))))

  (fn Emil:Analyzer:subtype-arrow (self (context Emil:Context)
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
               (Emil:Analyzer:subtype-pairwise
                self context
                right-arguments-adjusted
                left-arguments-adjusted)))
         (Emil:Analyzer:subtype
          self intermediate-context
          (Emil:Context:resolve intermediate-context left-returns)
          (Emil:Context:resolve intermediate-context right-returns))))))

  (fn Emil:Analyzer:subtype-compound (self (context Emil:Context)
                                           (left (Trait Emil:Type))
                                           (right (Trait Emil:Type)))
    (pcase-exhaustive (list left right)
      (`(,(Struct Emil:Type:Compound
                  :name left-name :parameters left-parameters)
         ,(Struct Emil:Type:Compound
                  :name right-name :parameters right-parameters))
       (cond
        ((and (eq left-name 'Cons) (eq right-name 'List))
         (Emil:Analyzer:subtype-pairwise
          self context
          left-parameters
          (list (car right-parameters) right)))
        ((and (eq left-name 'List) (eq right-name 'Cons))
         (Emil:Analyzer:subtype-pairwise
          self context
          (list (car left-parameters) left)
          right-parameters))
        ((and (eq left-name right-name)
              (= (length left-parameters)
                 (length right-parameters)))
         (Emil:Analyzer:subtype-pairwise
          self context left-parameters right-parameters))
        (t (Emil:Analyzer:subtype-default self context left right))))
      (_ (Emil:Analyzer:subtype-default self context left right))))

  (fn Emil:Analyzer:subtype-default (_self (context Emil:Context)
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

  (fn Emil:Analyzer:infer-application (self (arrow-type (Trait Emil:Type))
                                            arguments context environment)
    (pcase-exhaustive arrow-type
      ((Struct Emil:Type:Forall parameters type)
       (let* ((instances (Emil:Analyzer:generate-existentials
                          self (length parameters)))
              (instantiated-type
               (Emil:Type:substitute-all type parameters instances))
              (result-context
               (Emil:Context:concat (reverse instances) context)))
         (Emil:Analyzer:infer-application
          self instantiated-type arguments result-context environment)))
      ((Struct Emil:Type:Existential)
       (-let* ((instantiated-type (Emil:Type:Arrow
                                   :arguments (Emil:Analyzer:generate-existentials
                                               self (length arguments))
                                   :returns (Emil:Analyzer:generate-existential self)
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
                   (Emil:Analyzer:check self argument instance context environment))
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
                     (Emil:Analyzer:check self argument instance context environment))
                   context
                   (-zip-pair adjusted-arguments argument-types))))
           (cons result-context (cons returns argument-forms)))))
      (_
       (Emil:type-error "Function can not be applied to arguments: %s, %s"
                        (Emil:Type:print arrow-type)
                        arguments))))

  (fn Emil:Analyzer:generate-existential (self)
    "Returns a new `Emil:Type:Existential'."
    (Emil:Util:ExistentialGenerator:next (Struct:get self :generator)))

  (fn Emil:Analyzer:generate-existentials (self count)
    "Returns a list of COUNT new `Emil:Type:Existential's."
    (-map (lambda (_)
            (Emil:Analyzer:generate-existential self))
          (-repeat count nil)))

  (fn Emil:Analyzer:lookup-variable (variable context environment)
    (or (Emil:Context:lookup-variable context variable)
        (Emil:Env:lookup-variable environment variable context)))

  (fn Emil:Analyzer:lookup-function (function context environment)
    (or (Emil:Context:lookup-function context function)
        (Emil:Env:lookup-function environment function context)))

  (fn Emil:Analyzer:arrow-instantiate (self (type Emil:Type:Arrow))
    "Instantiates the function-type with existentials.

Returns a variant of this function in which all types are
replaced with instances of `Emil:Type:Existential'."
    (Emil:Type:Arrow*
     ,@type
     :arguments (Emil:Analyzer:generate-existentials
                 self (length (Emil:Type:Arrow:arguments type)))
     :returns (Emil:Analyzer:generate-existential self)))

  (fn Emil:Analyzer:compound-instantiate (self (type Emil:Type:Compound))
    "Instantiates the compound-type with existentials."
    (Emil:Type:Compound*
     ,@type
     :parameters (Emil:Analyzer:generate-existentials
                  self (length (Struct:get type :parameters)))))

  (fn Emil:Analyzer:infer-do (self forms (context Emil:Context)
                                   (environment (Trait Emil:Env)))
    (Emil:Util:map-reduce
     (-lambda (context form)
       (Emil:Analyzer:infer self form context environment))
     context
     forms))

  (fn Emil:Analyzer:check-do (self forms type (context Emil:Context)
                                   (environment (Trait Emil:Env)))
    (-let (((context . form)
            (Emil:Analyzer:check self (cons 'progn forms) type context environment)))
      (cons context (cdr (Transformer:Form:value form)))))

  (fn Emil:Analyzer:subtype-pairwise (self context left-types right-types)
    (unless (= (length left-types) (length right-types))
      (Emil:error "Left and right types of different lengths passed: %s, %s"
                  left-types right-types))
    (--reduce-from
     (Emil:Analyzer:subtype self acc
                            (Emil:Context:resolve acc (car it))
                            (Emil:Context:resolve acc (cdr it)))
     context
     (-zip-pair left-types right-types)))

  (fn Emil:Analyzer:infer-progn-like (self form (context Emil:Context)
                                           (environment (Trait Emil:Env)))
    (-let* ((body (cdr (Transformer:Form:value form)))
            ((context . forms)
             (Emil:Analyzer:infer-do self body context environment)))
      (cons context (Emil:TypedForm:new
                     (cons (car (Transformer:Form:value form)) forms)
                     (Emil:Analyzer:type-of-body forms)
                     environment))))

  (fn Emil:let*-thread-bindings (self bindings context environment)
    "Thread CONTEXT and ENVIRONMENT through let* BINDINGS."
    (Emil:Util:map-reduce
     (-lambda ((variable-context . variable-environment) (variable binding))
       (-let* (((binding-context . binding-form)
                (Emil:Analyzer:infer
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

  (fn Emil:Analyzer:type-of-body (body-forms)
    (if body-forms
        (Struct:get (-last-item body-forms) :type)
      (Emil:Type:Null)))

  (fn Emil:Analyzer:add-message (self (message Emil:Message))
    (Struct:update self :messages (-rpartial #'append (list message))))

  (fn Emil:Analyzer:has-errors? (self)
    (--some? (eq :error (Struct:get it :type))
             (Struct:get self :messages))))

(provide 'Emil/Analyzer)
