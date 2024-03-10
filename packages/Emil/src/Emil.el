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

(Struct:define Emil:ExistentialGenerator
  "Generator for instances of type `Emil:Type:Existential'."
  (counter :type number :default -1 :mutable t))

(defun Emil:ExistentialGenerator:next (self)
  (Emil:Type:Existential
   :name (->> (Struct:update self :counter #'1+)
              (format "t%d")
              (intern))))

(Struct:define Emil
  (generator
   :type Emil:ExistentialGenerator :default (Emil:ExistentialGenerator))
  (messages :type list))

(Struct:implement Emil
  (fn Emil:infer (self form (context Emil:Context))
    (Transformer:transform-form
     (Struct:get self :infer-transformer) form context))
  
  (fn Emil:check (self form type (context Emil:Context))
    (pcase type
      ((Struct Emil:Type:Forall parameters :type forall-type)
       (let* ((marker (Emil:Context:Marker))
              (intermediate-context
               (Emil:Context:concat (reverse parameters) marker context))
              (result-context
               (Emil:check self form forall-type intermediate-context)))
         (Emil:Context:drop-until-after result-context marker)))
      ((and (Struct Emil:Type:Arrow :arguments argument-types returns)
            (let `(function (lambda ,arguments . ,body)) form))
       ;; FIXME: &optional and &rest not handled.
       (let* ((bindings
               (--map (Emil:Context:Binding
                       :name (car it) :type (cdr it))
                      (-zip-pair arguments argument-types)))
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

  (fn Emil:instantiate-arrow (self (type Emil:Type:Arrow))
    "Instantiates the function-type with existentials.

Returns a variant of this function in which all types are
replaced with instances of `Emil:Type:Existential'."
    (Emil:Type:Arrow*
     ,@self
     :arguments (Emil:generate-existentials
                 self (length (Struct:get self :arguments)))
     :returns (Emil:generate-existential self)))

  (fn Emil:instantiate-left (self (context Emil:Context)
                                  (variable Emil:Type:Existential)
                                  (type (Trait Emil:Type)))
    ;; Figure 10. Instantiation
    (pcase-exhaustive type
      ;; InstLSolve
      ((and (pred (Emil:Type:monomorph? type))
            (let `(,top ,bottom)
              (Emil:Context:hole context variable))
            (guard (Emil:Context:well-formed? bottom type)))
       (Emil:Context:concat
        top (Emil:Context:Solution* variable type) bottom))
      ;; InstLReach
      ((and (Struct Emil:Type:Existential)
            (let `(,top ,center ,bottom)
              (Emil:Context:double-hole context type variable)))
       (Emil:Context:concat top (Emil:Context:Solution
                                 :existential type :type variable)
                            center variable bottom))
      ;; InstLArr
      ((and (Struct Emil:Type:Arrow)
            (let `(,top ,bottom)
              (Emil:Context:hole context variable)))
       (let* ((instance (Emil:instantiate-arrow
                         type (Struct:get self :generator)))
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
                (Emil:instantiate-right self (car it) (cdr it) acc)
                initial-context
                (-zip-pair (Emil:Type:Arrow:arguments type)
                           (Emil:Type:Arrow:arguments instance)))))
         (Emil:instantiate-left
          self
          intermediate-context
          (Struct:get instance :returns)
          (Emil:Context:resolve intermediate-context (Struct:get type :returns)))))
      ;; InstLAllR
      ((and (Struct Emil:Type:Forall parameters type)
            (guard (Emil:Context:member? context variable)))
       (let* ((marker (Emil:Context:Marker))
              (initial-context (Emil:Context:concat
                                (Emil:Context :entries (reverse parameters))
                                marker context))
              (intermediate-context (Emil:instantiate-left
                                     self initial-context variable type)))
         (Emil:Context:drop-until-after intermediate-context marker)))))

  (fn Emil:instantiate-right (self (context Emil:Context)
                                   (type (Trait Emil:Type))
                                   (variable Emil:Type:Existential))
    ;; Figure 10. Instantiation
    (pcase-exhaustive type
      ;; InstRSolve
      ((and (pred (Emil:Type:monomorph? type))
            (let `(,top ,bottom)
              (Emil:Context:hole context variable))
            (guard (Emil:Context:well-formed? bottom type)))
       (Emil:Context:concat
        top (Emil:Context:Solution* variable type) bottom))
      ;; InstRReach
      ((and (Struct Emil:Type:Existential)
            (let `(,top ,center ,bottom)
              (Emil:Context:double-hole context type variable)))
       (Emil:Context:concat top (Emil:Context:Solution
                                 :existential type :type variable)
                            center variable bottom))
      ;; InstRArr
      ((and (Struct Emil:Type:Arrow)
            (let `(,top ,bottom)
              (Emil:Context:hole context variable)))
       (let* ((instance (Emil:instantiate-arrow
                         type (Struct:get self :generator)))
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
                (Emil:instantiate-left self (car it) (cdr it) acc)
                initial-context
                (-zip-pair (Emil:Type:Arrow:arguments instance)
                           (Emil:Type:Arrow:arguments type)))))
         (Emil:instantiate-right
          self
          (Emil:Context:resolve
           intermediate-context (Struct:get type :returns))
          (Struct:get instance :returns)
          intermediate-context)))
      ;; InstRAllR
      ((and (Struct Emil:Type:Forall)
            (guard (Emil:Context:member? context variable)))
       (let* ((parameters (Emil:Type:Forall:parameters type))
              (instances (-map (lambda (_)
                                 (Emil:generate-existential self))
                               parameters))
              (marker (Emil:Context:Marker))
              (initial-context
               (Emil:Context:concat
                (Emil:Context :entries (reverse instances)) marker context))
              (intermediate-context
               (Emil:instantiate-right
                self
                initial-context
                (--reduce-from
                 (Emil:Type:substitute acc (car it) (cdr it))
                 (Emil:Type:Forall:type type)
                 (-zip-pair parameters instances))
                variable)))
         (Emil:Context:drop-until-after intermediate-context marker)))))

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
       nil)
      ;; Arrow
      (`(,(Struct Emil:Type:Arrow
                  :arguments left-arguments :returns left-returns)
         ,(Struct Emil:Type:Arrow
                  :arguments right-arguments :returns right-returns))
       (-let (((left-min . left-max) (Emil:Type:Arrow:arity left))
              ((right-min . right-max) (Emil:Type:Arrow:arity right)))
         (unless (and (<= left-min right-min)
                      (>= left-max right-max))
           (error "Function is not arity-compatible"))
         (Emil:subtype
          self (--reduce-from
                (Emil:subtype self acc (car it) (cdr it))
                context
                (-zip-pair right-arguments left-arguments))
          left-returns right-returns)))
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
       (Emil:instantiate-left self context left right))
      ;; Instantiate R
      ((and `(,_ ,(Struct Emil:Type:Existential name))
            (guard (not (member name (Emil:Type:free-variables left)))))
       (Emil:instantiate-right self context left right))
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
       nil)))

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
                          :existential arrow-type :type instantiated-type))
               ((top bottom)
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
       (cons returns
             (--reduce-from
              (Emil:check self (car it) (cdr it) acc)
              context
              (-zip-pair arguments argument-types)))))))

(Trait:implement Transformer Emil
  (fn Transformer:transform-number (_self _form number &optional context &rest _)
    (list (type-of number) context))

  (fn Transformer:transform-string (_self _form string &optional context &rest _)
    (list (type-of string) context))

  (fn Transformer:transform-vector (_self _form vector &optional context &rest _)
    (list (type-of vector) context))

  (fn Transformer:transform-symbol (_self _form symbol &optional context &rest _)
    (let ((type (Emil:Context:lookup-binding context symbol)))
      (unless type
        (error "Unbound variable: %s" symbol))
      (cons type context)))

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
                 self (cdr (Transformer:transform-form self (cdr acc) (car it)))
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

  (fn Transformer:transform-function (self _form argument &optional context &rest _)
    (pcase-exhaustive argument
      ((and `(lambda ,argument-list . ,body)
            (guard (listp argument-list)))
       (let* ((arguments (Emil:generate-existentials
                          self (length argument-list)))
              (returns (Emil:generate-existential self))
              (bindings (--map (Emil:Context:Binding
                                :name (car it) :type (cdr it))
                               (-zip-pair argument-list arguments)))
              (marker (Emil:Context:Marker))
              (initial-context
               (Emil:Context:concat
                (reverse bindings) returns
                (reverse arguments) context))
              (intermediate-context
               (Emil:check self body returns initial-context)))
         (cons (Emil:Type:Arrow*
                arguments returns :min-arity (length arguments))
               (Emil:Context:drop-until-after intermediate-context marker))))
      ((pred symbolp)
       (if-let (type (Emil:Context:lookup-binding context argument))
           (cons type context)
         (error "Unbound variable: %s" argument)))))

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
                     :name (car it) :type (cdr it))
                    (-zip-pair (-map #'car bindings)
                               binding-types)))
            (marker (Emil:Context:Marker))
            (body-context
             (Emil:Context:concat
              (reverse context-bindings)
              marker binding-context))
            ((body-type . result-context)
             (Emil:infer-do self body-context body)))
      (cons body-type (Emil:Context:drop-until-after result-context marker))))

  (fn Transformer:transform-let* (self _form bindings body &optional context &rest _)
    (-let* ((marker (Emil:Context:Marker))
            (body-context
             (--reduce-from
              (-let (((type . accumulated-context)
                      (Transformer:transform-form self (cadr it) acc)))
                (Emil:Context:concat
                 (Emil:Context:Binding :name (car it) :type type)
                 accumulated-context))
              (Emil:Context:concat marker context)
              bindings))
            ((body-type . result-context)
             (Emil:infer-do self body-context body)))
      (cons body-type (Emil:Context:drop-until-after result-context marker))))

  (fn Transformer:transform-or (self _form conditions &optional context &rest _)
    (cons (Emil:Type:Any)
          (cdr (Emil:infer-do self context conditions))))

  (fn Transformer:transform-prog1 (self _form first body &optional context &rest _)
    (-let (((first-type . initial-context)
            (Transformer:transform-form self first context)))
      (cons first-type
            (cdr (Emil:infer-do self initial-context body)))))

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
                                        &optional _context &rest _)
    (error "Not implemented: setq"))

  (fn Transformer:transform-unwind-protect (_self _form _unwind-form _forms
                                                  &optional _context &rest _)
    (error "Not implemented: unwind-protect"))

  (fn Transformer:transform-while (self _form condition body
                                        &optional context &rest _)
    (Emil:infer-do self context (cons condition body)))

  (fn Transformer:transform-application (self _form function arguments
                                              &optional context &rest _)
    (-let (((arrow-type . result-context)
            (Transformer:transform-form self function context)))
      (Emil:infer-application
       self result-context
       (Emil:Context:resolve result-context arrow-type)
       arguments)))

  (fn Transformer:transform-macro (self _form macro arguments
                                        &optional context &rest _)
    (Transformer:transform-form
     self
     (macroexpand (-map #'Form:value (cons macro arguments)))
     context)))

(defun Emil:infer-form (form)
  (-let* (((type . context)
           (Emil:infer (Emil) form (Emil:Context))))
    (Emil:Context:resolve context type)))

(provide 'Emil)
;;; Emil.el ends here
