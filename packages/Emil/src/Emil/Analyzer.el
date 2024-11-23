;; -*- lexical-binding: t -*-

(require 'dash)
(require 'Struct)
(require 'Trait)
(require 'Emil/Util)
(require 'Emil/Context)
(require 'Emil/Type)
(require 'Emil/Message)
(require 'Emil/Form)
(require 'Transformer)

(Struct:define Emil:Analyzer
  (generator
   :type Emil:Util:ExistentialGenerator :default (Emil:Util:ExistentialGenerator))
  (messages :type list :mutable t))

(Struct:implement Emil:Analyzer
  :disable-syntax t
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
       (cons context (Emil:Form:Invalid
                      :form form
                      :type (Emil:Type:Any))))))

  (fn Emil:Analyzer:check (self form type (context Emil:Context)
                                (environment (Trait Emil:Env)))
    (pcase-exhaustive type
      ((Struct Emil:Type:Forall)
       (Emil:Analyzer:check-forall self form type context environment))
      ((and (Struct Emil:Type:Arrow)
            (let `(function ,lambda) form)
            (let `(lambda . ,_) lambda))
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
          (progn
            (Struct:unsafe-set typed-form :type given-type)
            typed-form))))))

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
      (cons (Emil:Context:drop-until-after result-context marker) result-form)))

  (fn Emil:Analyzer:check-lambda (self form (type Emil:Type:Arrow)
                                       (context Emil:Context)
                                       (environment (Trait Emil:Env)))
    (pcase-exhaustive type
      ((and (Struct Emil:Type:Arrow returns)
            (let `(function ,lambda) form)
            (let `(lambda ,arguments . ,body) lambda))
       (-let* ((bindings
                (--map (Emil:Context:Binding
                        :variable (car it) :type (cdr it))
                       (Emil:Analyzer:lambda-bindings type lambda)))
               (marker (Emil:Context:Marker))
               ((body-context . body-forms)
                (Emil:Analyzer:check-do
                 self body returns
                 (Emil:Context:concat (reverse bindings) marker context)
                 environment)))
         (cons (Emil:Context:drop-until-after body-context marker)
               (Emil:Form:Function
                :value (Emil:Form:Lambda
                        :arguments arguments
                        :body body-forms)
                :type returns))))))

  (fn Emil:Analyzer:lambda-bindings ((type Emil:Type:Arrow) lambda)
    (let* ((lambda-arguments (nth 1 lambda))
           (variables (Emil:Analyzer:lambda-variables lambda-arguments))
           (variable-count (length variables))
           (variable-rest? (memq '&rest lambda-arguments))
           (arguments (Emil:Type:Arrow:arguments type))
           (argument-count (length arguments))
           (last-argument (-last-item arguments))
           (argument-rest? (Struct:get type :rest?))
           (bindings nil))
      (unless (Emil:Type:Arrow:arity-assignable-from? type (func-arity lambda))
        (Emil:type-error "Function is not assignable to %s: (lambda %s)" (Emil:Type:print type)
                         lambda-arguments))
      (when (> variable-count 0)
        (dotimes (_ (min argument-count (1- variable-count)))
          (push (cons (pop variables) (pop arguments))
                bindings))
        (dotimes (_ (- variable-count
                       (min argument-count (1- variable-count))
                       1))
          (push (cons (pop variables) (if argument-rest? last-argument (Emil:Type:Null)))
                bindings))
        (let ((last-type
               (if (not variable-rest?)
                   (if (= variable-count argument-count) last-argument (Emil:Type:Null))
                 (Emil:Type:Compound
                  :name 'List
                  :arguments
                  (list (if (< variable-count argument-count)
                            (Emil:Type:Any)
                          (if (or (= variable-count argument-count)
                                  (and (> variable-count argument-count)
                                       argument-rest?))
                              last-argument
                            (Emil:Type:Null))))))))
          (push (cons (pop variables) last-type) bindings)))
      (nreverse bindings)))

  (fn Emil:Analyzer:check-do (self forms type (context Emil:Context)
                                   (environment (Trait Emil:Env)))
    (-let (((context . progn-form)
            (Emil:Analyzer:check self (cons 'progn forms) type context environment)))
      (cons context (Struct:get progn-form :body))))

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
       (let* ((instance
               (Emil:Type:Arrow*
                ,@type
                :arguments (Emil:Analyzer:generate-existentials
                            self (length (Emil:Type:Arrow:arguments type)))
                :returns (Emil:Analyzer:generate-existential self)))
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
                   (Emil:Type:substitute-all
                    (Emil:Type:Forall:type type)
                    parameters instances)
                   relation)))
            (Emil:Context:drop-until-after intermediate-context marker)))))))

  (fn Emil:Analyzer:instantiate-compound (self (context Emil:Context)
                                               (variable Emil:Type:Existential)
                                               (type Emil:Type:Compound)
                                               relation)
    (pcase-exhaustive type
      ((and (Struct Emil:Type:Compound arguments)
            (let `(,top . ,bottom)
              (Emil:Context:hole context variable)))
       (let* ((instance (Emil:Type:Compound*
                         ,@type
                         :arguments (Emil:Analyzer:generate-existentials
                                     self (length (Struct:get type :arguments)))))
              (solution
               (Emil:Context:Solution* variable :type instance))
              (initial-context
               (Emil:Context:concat
                top solution
                (reverse (Struct:get instance :arguments))
                bottom)))
         (--reduce-from
          (Emil:Analyzer:instantiate self acc (car it) (cdr it) relation)
          initial-context
          (-zip-pair (Struct:get instance :arguments)
                     arguments))))))

  (fn Emil:Analyzer:subtype (self (context Emil:Context)
                                  (left (Trait Emil:Type))
                                  (right (Trait Emil:Type)))
    (when-let (real-left (Emil:Type:resolve-alias left))
      (setq left real-left))
    (when-let (real-right (Emil:Type:resolve-alias right))
      (setq right real-right))
    ;; Figure 9. Algorithmic subtyping
    (pcase-exhaustive (list left right)
      ((or `(,(Struct Emil:Type:Never) ,_)
           `(,(Struct Emil:Type:Void) ,_)
           `(,_ ,(Struct Emil:Type:Never)))
       (Emil:Analyzer:subtype-error left right))
      ((or `(,(Struct Emil:Type:Any) ,_)
           `(,_ ,(Struct Emil:Type:Any))
           `(,(Struct Emil:Type:Null) ,_)
           `(,_ ,(Struct Emil:Type:Void)))
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
      (`(,(Struct Emil:Type:Arrow :returns left-returns)
         ,(Struct Emil:Type:Arrow :returns right-returns))
       (let* ((argument-pairs
               (Emil:Analyzer:subtype-arrow-pairs left right))
              (result-context
               (Emil:Analyzer:subtype-pairwise
                self context
                (-map #'cdr argument-pairs)
                (-map #'car argument-pairs))))
         (Emil:Analyzer:subtype
          self result-context
          (Emil:Context:resolve result-context left-returns)
          (Emil:Context:resolve result-context right-returns))))))

  (fn Emil:Analyzer:subtype-arrow-pairs ((left Emil:Type:Arrow) (right Emil:Type:Arrow))
    (unless (Emil:Type:Arrow:arity-assignable-to? left right)
      (Emil:type-error "Function is not arity-compatible: %s, %s" left right))
    (let* ((left-arguments (Emil:Type:Arrow:arguments left))
           (left-count (length left-arguments))
           (left-rest (and (Emil:Type:Arrow:rest? left)
                           (-last-item left-arguments)))
           (right-arguments (Emil:Type:Arrow:arguments right))
           (right-count (length right-arguments))
           (right-rest (and (Emil:Type:Arrow:rest? right)
                            (-last-item right-arguments))))
      (cond
       ((and left-rest right-rest)
        (-zip-pair (append left-arguments
                           (-repeat (- (max right-count left-count) left-count) left-rest))
                   (append right-arguments
                           (-repeat (- (max right-count left-count) right-count) right-rest))))
       (left-rest
        (-zip-pair (append left-arguments
                           (-repeat (- right-count left-count) left-rest))
                   right-arguments))
       (t
        (-zip-pair left-arguments
                   (-take left-count right-arguments))))))

  (fn Emil:Analyzer:subtype-compound (self (context Emil:Context)
                                           (left (Trait Emil:Type))
                                           (right (Trait Emil:Type)))
    (pcase-exhaustive right
      ((Struct Emil:Type:Compound
               :name right-name :arguments right-arguments)
       (cond
        ((eq right-name 'Trait)
         (unless (Trait:name? (car right-arguments))
           (Emil:type-error "%s is not a defined trait" (car right-arguments)))
         (unless (and (or (Emil:Type:Basic? left)
                          (Emil:Type:Compound? left))
                      (Trait:implements? (Struct:get left :name)
                                         (car right-arguments)))
           (Emil:Analyzer:subtype-error left right))
         context)
        (t
         (pcase-exhaustive left
           ((Struct Emil:Type:Basic :name left-name)
            (cond
             ((and (memq left-name '(string char-table bool-vector))
                   (memq right-name '(Array Sequence)))
              (Emil:Analyzer:subtype
               self context
               (pcase-exhaustive left-name
                 ('string (Emil:Type:Basic :name 'integer))
                 ('char-table (Emil:Type:Any))
                 ('bool-vector (Emil:Type:Basic :name 'symbol)))
               (car right-arguments)))
             (t (Emil:Analyzer:subtype-error left right))))
           ((Struct Emil:Type:Compound)
            (Emil:Analyzer:subtype-compounds
             self context left right))
           (_ (Emil:Analyzer:subtype-error left right))))))
      (_ (Emil:Analyzer:subtype-error left right))))

  (fn Emil:Analyzer:subtype-compounds (self (context Emil:Context)
                                            (left (Trait Emil:Type))
                                            (right (Trait Emil:Type)))
    (let ((left-name (Struct:get left :name))
          (right-name (Struct:get right :name))
          (left-arguments (Struct:get left :arguments))
          (right-arguments (Struct:get right :arguments)))
      (cond
       ((and (eq left-name 'Cons)
             (memq right-name '(List Sequence)))
        (Emil:Analyzer:subtype-pairwise
         self context
         left-arguments
         (list (car right-arguments) right)))
       ((and (eq left-name 'List)
             (eq right-name 'Cons))
        (Emil:Analyzer:subtype-pairwise
         self context
         (list (car left-arguments) left)
         right-arguments))
       ((and (eq left-name 'Trait)
             (eq right-name 'Trait))
        (unless (and (Trait:name? (car left-arguments))
                     (Trait:name? (car right-arguments))
                     (Trait:extends? (car left-arguments)
                                     (car right-arguments)))
          (Emil:Analyzer:subtype-error left right))
        context)
       ((and (Emil:Type:compound-subtype? left-name right-name)
             (= (length left-arguments)
                (length right-arguments)))
        (Emil:Analyzer:subtype-pairwise
         self context left-arguments right-arguments))
       (t (Emil:Analyzer:subtype-error left right)))))

  (fn Emil:Analyzer:subtype-default (_self (context Emil:Context)
                                           (left (Trait Emil:Type))
                                           (right (Trait Emil:Type)))
    (cond
     ((and (Emil:Type:Basic? left)
           (Emil:Type:Basic? right)
           (Emil:Type:basic-subtype?
            (Struct:get left :name)
            (Struct:get right :name)))
      context)
     (t (Emil:Analyzer:subtype-error left right))))

  (fn Emil:Analyzer:subtype-error (left right)
    (Emil:type-error
     "%s is not assignable to %s"
     (Emil:Type:print left)
     (Emil:Type:print right)))

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
      ((Struct Emil:Type:Arrow  returns)
       (-let* ((argument-pairs
                (Emil:infer-application-arrrow-pairs arrow-type arguments))
               ((result-context . argument-forms)
                (Emil:Util:map-reduce
                 (-lambda (context (type . argument))
                   (Emil:Analyzer:check
                    self argument type context environment))
                 context
                 argument-pairs)))
         (cons result-context (cons returns argument-forms))))
      (_
       (Emil:type-error "Function can not be applied to arguments: %s, %s"
                        (Emil:Type:print arrow-type)
                        arguments))))

  (fn Emil:infer-application-arrrow-pairs ((arrow-type Emil:Type:Arrow) arguments)
    (let* ((argument-count (length arguments))
           (types (Emil:Type:Arrow:arguments arrow-type))
           (type-count (length types)))
      (unless (Emil:Type:Arrow:arity-assignable-to?
               arrow-type (cons argument-count argument-count))
        (Emil:type-error "Application is not arity compatible: %d, %s"
                         argument-count arrow-type))
      (-zip-pair (append types
                         (-repeat (- argument-count type-count)
                                  (-last-item types)))
                 arguments)))

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

  (fn Emil:Analyzer:add-message (self (message Emil:Message))
    (Struct:update self :messages (-rpartial #'append (list message))))

  (fn Emil:Analyzer:has-errors? (self)
    (--some? (eq :error (Struct:get it :type))
             (Struct:get self :messages)))

  (fn Emil:Analyzer:lambda-variables (arguments)
    "Return ARGUMENTS excluding &optional and &rest keywords."
    (--filter (not (memq it '(&optional &rest))) arguments)))

(provide 'Emil/Analyzer)
