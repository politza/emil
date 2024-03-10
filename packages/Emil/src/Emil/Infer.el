;; -*- lexical-binding: t -*-

(require 'Transformer)
(require 'Emil/Type)
(require 'Emil/Context)

(declare-function Emil:generate-existentials nil)
(declare-function Emil:generate-existential nil)
(declare-function Emil:check nil)

(Struct:define Emil:Infer
  (emil :type Emil))

(Struct:implement Emil:Infer
  (fn Emil:Infer:do (self (context Emil:Context) forms)
    (--reduce-from (Transformer:transform-form self it (cdr acc))
                   (cons (Emil:Type:Null) context)
                   forms))

  (fn Emil:Infer:application (self context arrow-type arguments)
    (pcase-exhaustive arrow-type
      ((Struct Emil:Type:Forall parameters type)
       (let* ((instances (Emil:generate-existentials
                          (Struct:get self :emil)
                          (length parameters)))
              (instantiated-type
               (Emil:Type:substitute-all type parameters instances))
              (result-context
               (Emil:Context:concat (reverse instances) context)))
         (Emil:Infer:application
          self result-context instantiated-type arguments)))
      ((Struct Emil:Type:Existential)
       (-let* ((emil (Struct:get self :emil))
               (instantiated-type (Emil:Type:Arrow
                                   :arguments (Emil:generate-existentials
                                               emil (length arguments))
                                   :returns (Emil:generate-existential emil)
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
                (Emil:check (Struct:get self :emil)
                            (car it) (cdr it) acc)
                result-context
                (-zip-pair arguments instantiated-arguments)))))
      ((Struct Emil:Type:Arrow :arguments argument-types returns)
       (cons returns
             (--reduce-from
              (Emil:check (Struct:get self :emil)
                          (car it) (cdr it) acc)
              context
              (-zip-pair arguments argument-types)))))))

(Trait:implement Transformer Emil:Infer
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
          (cdr (Emil:Infer:do self context conditions))))

  (fn Transformer:transform-catch (self _form _tag body &optional context &rest _)
    (cons (Emil:Type:Any)
          (cdr (Emil:Infer:do self context body))))

  (fn Transformer:transform-cond (self _form clauses &optional context &rest _)
    (cons (Emil:Type:Any)
          (cdr (--reduce-from
                (Emil:Infer:do
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
       (let* ((emil (Struct:get self :emil))
              (arguments (Emil:generate-existentials
                          emil (length argument-list)))
              (returns (Emil:generate-existential emil))
              (bindings (--map (Emil:Context:Binding
                                :name (car it) :type (cdr it))
                               (-zip-pair argument-list arguments)))
              (marker (Emil:Context:Marker))
              (initial-context
               (Emil:Context:concat
                (reverse bindings) returns
                (reverse arguments) context))
              (intermediate-context
               (Emil:check emil body returns initial-context)))
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
          (cdr (Emil:Infer:do self context `(,condition ,then ,@else)))))

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
             (Emil:Infer:do self body-context body)))
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
             (Emil:Infer:do self body-context body)))
      (cons body-type (Emil:Context:drop-until-after result-context marker))))

  (fn Transformer:transform-or (self _form conditions &optional context &rest _)
    (cons (Emil:Type:Any)
          (cdr (Emil:Infer:do self context conditions))))

  (fn Transformer:transform-prog1 (self _form first body &optional context &rest _)
    (-let (((first-type . initial-context)
            (Transformer:transform-form self first context)))
      (cons first-type
            (cdr (Emil:Infer:do self initial-context body)))))

  (fn Transformer:transform-progn (self _form body &optional context &rest _)
    (Emil:Infer:do self context body))

  (fn Transformer:transform-quote (_self _form _argument &optional context &rest _)
    (cons (Emil:Type:Any) context))

  (fn Transformer:transform-save-current-buffer (self _form body
                                                      &optional context &rest _)
    (Emil:Infer:do self context body))

  (fn Transformer:transform-save-excursion (self _form body
                                                 &optional context &rest _)
    (Emil:Infer:do self context body))

  (fn Transformer:transform-save-restriction (self _form body
                                                   &optional context &rest _)
    (Emil:Infer:do self context body))

  (fn Transformer:transform-setq (_self _form _definitions
                                        &optional _context &rest _)
    (error "Not implemented: setq"))

  (fn Transformer:transform-unwind-protect (_self _form _unwind-form _forms
                                                  &optional _context &rest _)
    (error "Not implemented: unwind-protect"))

  (fn Transformer:transform-while (self _form condition body
                                        &optional context &rest _)
    (Emil:Infer:do self context (cons condition body)))

  (fn Transformer:transform-application (self _form function arguments
                                              &optional context &rest _)
    (-let (((arrow-type . result-context)
            (Transformer:transform-form self function context)))
      (Emil:Infer:application
       self result-context
       (Emil:Context:resolve result-context arrow-type)
       arguments)))

  (fn Transformer:transform-macro (self _form macro arguments
                                        &optional context &rest _)
    (Transformer:transform-form
     self
     (macroexpand (-map #'Form:value (cons macro arguments)))
     context)))

(provide 'Emil/Infer)
