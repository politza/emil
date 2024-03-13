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
               (Emil:Context:concat
                (Emil:Context :entries (reverse parameters)
                              :parent context)
                marker context))
              (typed-form
               (Emil:check self form forall-type intermediate-context))
              (result-context
               (Emil:Context:drop-until-after
                (Struct:get typed-form :context) marker)))
         (Emil:TypedForm* ,@typed-form :context result-context)))
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
               (Emil:Context:concat
                (Emil:Context :entries (reverse bindings)
                              :parent context)
                marker context))
              (typed-form (Emil:check self body returns intermediate-context))
              (result-context
               (Emil:Context:drop-until-after
                (Struct:get typed-form :context) marker)))
         (Emil:TypedForm* ,@typed-form :context result-context)))
      (_
       (let* ((typed-form (Emil:infer self form context))
              (intermediate-context (Struct:get typed-form :context))
              (inferred-type (Struct:get typed-form :type)))
         (Emil:TypedForm*
          ,@typed-form
          :context
          (Emil:subtype
           self intermediate-context
           (Emil:Context:resolve intermediate-context inferred-type)
           (Emil:Context:resolve intermediate-context type)))))))

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
                                     (Emil:Context :entries (reverse parameters)
                                                   :parent context)
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
                     (Emil:Context :entries (reverse instances)
                                   :parent context) marker context))
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
                (Emil:Context :entries (reverse instances)
                              :parent context)
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
                (Emil:Context :entries (reverse parameters)
                              :parent context)
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
    (Emil:Util:map-accum
     (lambda (context form)
       (let ((result (Transformer:transform-form self form context)))
         (cons (Struct:get result :context) result)))
     context
     forms))

  (fn Emil:infer-application (self context arrow-type arguments)
    (pcase-exhaustive arrow-type
      ((Struct Emil:Type:Forall parameters type)
       (let* ((instances (Emil:generate-existentials
                          self (length parameters)))
              (instantiated-type
               (Emil:Type:substitute-all type parameters instances))
              (result-context
               (Emil:Context:concat
                (Emil:Context :entries (reverse instances)
                              :parent context)
                context)))
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
  (fn Transformer:transform-number (_self form number &optional context &rest _)
    (Emil:TypedForm*
     context form :type (Emil:Type:Basic :name (type-of number))))

  (fn Transformer:transform-string (_self form string &optional context &rest _)
    (Emil:TypedForm*
     context form :type (Emil:Type:Basic :name 'string)))

  (fn Transformer:transform-vector (_self form vector &optional context &rest _)
    (Emil:TypedForm*
     context form :type (Emil:Type:Basic :name (type-of vector))))

  (fn Transformer:transform-symbol (self form symbol &optional context &rest _)
    (Emil:TypedForm*
     context form
     :type (cond
            ((null symbol)
             (Emil:Type:Null))
            ((or (eq symbol t) (keywordp symbol))
             (Emil:Type:Basic :name 'symbol))
            (t
             (or (Emil:lookup-variable self context symbol)
                 (error "Unbound variable: %s" symbol))))))

  (fn Transformer:transform-and (self form conditions &optional context &rest _)
    (-let (((result-context . forms)
            (Emil:infer-do self context conditions)))
      (Emil:TypedForm*
       :form `(and ,@forms)
       :context result-context
       :type (Emil:Type:Any))))

  (fn Transformer:transform-catch (self form tag body &optional context &rest _)
    (-let (((result-context . forms)
            (Emil:infer-do self context body)))
      (Emil:TypedForm*
       :form `(catch ,tag ,@forms)
       :context result-context
       :type (Emil:Type:Any))))

  (fn Transformer:transform-cond (self form clauses &optional context &rest _)
    (-let (((result-context . forms)
            (Emil:infer-do self context clauses)))
      (Emil:TypedForm*
       :form `(cond ,@forms)
       :context result-context
       :type (Emil:Type:Any))))

  (fn Transformer:transform-defconst (self form symbol init-value &optional
                                           doc-string context &rest _)
    (pcase-let ((Struct Emil:TypedForm :form init-form :context result-context)
                (Transformer:transform-form self init-value context))
      (Emil:TypedForm*
       :form `(defconst ,symbol ,init-form ,doc-string)
       :context result-context
       :type (Emil:Type:Basic :name 'symbol))))

  (fn Transformer:transform-defvar (self form symbol &optional init-value
                                         doc-string context &rest _)
    (pcase-let ((Struct Emil:TypedForm :form init-form :context result-context)
                (Transformer:transform-form self init-value context))
      (Emil:TypedForm*
       :form `(defvar ,symbol ,init-form ,doc-string)
       :context result-context
       :type (Emil:Type:Basic :name 'symbol))))

  (fn Transformer:transform-function (self form argument
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
                (Emil:Context :entries (reverse bindings)
                              :parent context)
                marker returns
                (reverse arguments) context))
              (typed-form
               (Emil:check self (cons 'progn body) returns initial-context))
              (result-context
               (Emil:Context:drop-until-after
                (Struct:get typed-form :context) marker)))
         (Emil:TypedForm
          :form `(function (lambda ,argument-list ,@(Struct:get typed-form :form)))
          :type (Emil:Type:Arrow* arguments returns min-arity rest?)
          :context result-context))
       ((pred symbolp)
        (let ((type (or (Emil:lookup-function self context argument)
                        (error "Unbound function: %s" argument))))
          (Emil:TypedForm* type context :form `(function ,argument))))))

  (fn Transformer:transform-if (self form condition then else
                                     &optional context &rest _)
    (-let (((result-context condition-form then-form . else-forms)
            (Emil:infer-do self context `(,condition ,then ,@else))))
      (Emil:TypedForm
       :form `(if ,condition-form ,then-form ,@else-forms)
       :type (Emil:Type:Any)
       :context result-context)))

  (fn Transformer:transform-interactive (_self form _descriptor _modes
                                               &optional context &rest _)
    (Emil:TypedForm
     :form form
     :type (Emil:Type:Any)
     :context context))

  (fn Transformer:transform-let (self form bindings body &optional context &rest _)
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
              (Emil:Context :entries (reverse context-bindings)
                            :parent context)
              marker binding-context))
            ((body-type . result-context)
             (Emil:infer-do self body-context body)))
      (cons (Emil:Context:resolve result-context body-type)
            (Emil:Context:concat
             (Emil:Context :parent context)
             (Emil:Context:drop-until-after result-context marker)))))

  (fn Transformer:transform-let* (self form bindings body &optional context &rest _)
    (-let* ((marker (Emil:Context:Marker))
            (body-context
             (--reduce-from
              (-let (((type . accumulated-context)
                      (Transformer:transform-form self (cadr it) acc)))
                (Emil:Context:concat
                 (Emil:Context :parent acc)
                 (Emil:Context:Binding :variable (car it) :type type)
                 accumulated-context))
              (Emil:Context:concat
               (Emil:Context :parent context)
               marker context)
              bindings))
            ((body-type . result-context)
             (Emil:infer-do self body-context body)))
      (cons (Emil:Context:resolve result-context body-type)
            (Emil:Context:concat
             (Emil:Context :parent context)
             (Emil:Context:drop-until-after result-context marker)))))

  (fn Transformer:transform-or (self form conditions &optional context &rest _)
    (-let (((result-context . forms)
            (Emil:infer-do self context conditions)))
      (Emil:TypedForm*
       :form `(or ,@forms)
       :context result-context
       :type (Emil:Type:Any))))

  (fn Transformer:transform-prog1 (self form first body &optional context &rest _)
    (-let ((first-form 
            (Transformer:transform-form self first context))
           ((result-context . body-forms)
            (Emil:infer-do self (Struct:get first-form :context) body)))
      (Emil:TypedForm*
       ,@first-form
       :form `(prog1 ,first-form ,@body-forms)
       :context result-context)))

  (fn Transformer:transform-progn (self form body &optional context &rest _)
    (let (((result-context . body-forms)
           (Emil:infer-do self context body)))
      (Emil:TypedForm
       :form `(progn ,@body-forms)
       :type (if body-forms (-last-item body-forms) (Emil:Type:Null))
       :context result-context))))

  (fn Transformer:transform-quote (_self form _argument &optional context &rest _)
    (Emil:TypedForm*
     form context
     :type (Emil:Type:Any)))

  (fn Transformer:transform-save-current-buffer (self form body
                                                      &optional context &rest _)
    (-let (((result-context . forms)
            (Emil:infer-do self context body)))
      (Emil:TypedForm*
       :form `(save-current-buffer ,@forms)
       :context result-context
       :type (Emil:Type:Any))))

  (fn Transformer:transform-save-excursion (self form body
                                                 &optional context &rest _)
    (-let (((result-context . forms)
            (Emil:infer-do self context body)))
      (Emil:TypedForm*
       :form `(save-current-buffer ,@forms)
       :context result-context
       :type (Emil:Type:Any))))

  (fn Transformer:transform-save-restriction (self form body
                                                   &optional context &rest _)
    (Emil:infer-do self context body))

  (fn Transformer:transform-setq (_self form _definitions
                                        &optional context &rest _)
    (cons (Emil:Type:Any) context))

  (fn Transformer:transform-unwind-protect (self form body-form unwind-forms
                                                 &optional context &rest _)
    (prog1 (Transformer:transform-form self body-form context)
      (Emil:infer-do self context unwind-forms)))

  (fn Transformer:transform-while (self form condition body
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

  (fn Transformer:transform-macro (self form macro arguments
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
