;; -*- lexical-binding: t -*-

(require 'dash)
(require 'Transformer)
(require 'Emil/Analyzer)
(require 'Emil/Annotation)
(require 'Emil/Form)
(require 'Emil/Util)

(Struct:implement Emil:Analyzer
  :disable-syntax t
  (fn Emil:Analyzer:infer-do (self forms (context Emil:Context)
                                   (environment (Trait Emil:Env)))
    (Emil:Util:map-reduce
     (-lambda (context form)
       (Emil:Analyzer:infer self form context environment))
     context
     forms))

  (fn Emil:Analyzer:infer-progn-like (self form (context Emil:Context)
                                           (environment (Trait Emil:Env)))
    (-let* ((body (cdr form))
            ((context . body-forms)
             (Emil:Analyzer:infer-do self body context environment)))
      (cons context (Emil:Form:PrognLike
                     :kind (car form)
                     :body body-forms
                     :type (Emil:Analyzer:type-of-body body-forms)))))

  (fn Emil:Analyzer:thread-let*-bindings (self bindings context environment)
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

  (fn Emil:Analyzer:transform-annotation (self _form macro arguments &optional context environment)
    (unless (memq macro Emil:Annotation:macros)
      (error "Internal error: Expected Emil:is or Emil:as: %s" macro))
    (unless (= 2 (length arguments))
      (Emil:type-error "Syntax error: %s: %s" macro arguments))
    (-let ((type (Emil:Type:read (nth 1 arguments))))
      (pcase-exhaustive macro
        ('Emil:is
            (Emil:Analyzer:check self (nth 0 arguments) type context environment))
        ('Emil:as
            (-let (((context . typed-form)
                    (Emil:Analyzer:infer self (nth 0 arguments) context environment)))
              (cons context (Emil:Form:with-type typed-form type)))))))

  (fn Emil:Analyzer:macroexpand-maybe (form environment)
    (if (and (macrop (car-safe form))
             (not (memq (car-safe form)
                        Emil:Annotation:macros)))
        (macroexpand form (Emil:Env:macro-environment environment))
      form)))

(Trait:implement Transformer Emil:Analyzer
  :disable-syntax t
  (fn Transformer:transform-number (_self form &optional context _environment
                                          &rest _)
    (cons context (Emil:Form:Atom
                   :value form
                   :type (Emil:Type:Basic :name (type-of form)))))

  (fn Transformer:transform-string (_self form &optional context _environment
                                          &rest _)
    (cons context (Emil:Form:Atom
                   :value form
                   :type (Emil:Type:Basic :name 'string))))

  (fn Transformer:transform-vector (_self form &optional context _environment
                                          &rest _)
    (cons context (Emil:Form:Atom
                   :value form
                   :type (Emil:Type:Basic :name (type-of form)))))

  (fn Transformer:transform-symbol (_self form &optional context environment
                                          &rest _)
    (let ((type (cond
                 ((null form)
                  (Emil:Type:Null))
                 ((or (eq form t) (keywordp form))
                  (Emil:Type:Basic :name 'symbol))
                 (t
                  (or (Emil:Analyzer:lookup-variable form context environment)
                      (Emil:type-error "Unbound variable: %s" form))))))
      (cons context (Emil:Form:Atom
                     :value form
                     :type type))))

  (fn Transformer:transform-and (self _form conditions &optional context environment
                                      &rest _)
    (-let (((context . condition-forms)
            (Emil:Analyzer:infer-do self conditions context environment)))
      (cons context (Emil:Form:And
                     :conditions condition-forms
                     :type (Emil:Type:Any)))))

  (fn Transformer:transform-catch (self _form tag body &optional context environment
                                        &rest _)
    (-let (((context . catch-forms)
            (Emil:Analyzer:infer-do self (cons tag body) context environment)))
      (cons context (Emil:Form:Catch
                     :tag (car catch-forms)
                     :body (cdr catch-forms)
                     :type (Emil:Type:Any)))))

  (fn Transformer:transform-cond (self _form clauses &optional context environment
                                       &rest _)
    (-let (((context . forms)
            (Emil:Util:map-reduce
             (lambda (context clause)
               (Emil:Analyzer:infer-do self clause context environment))
             context
             clauses)))
      (cons context (Emil:Form:Cond
                     :clauses (--map (Emil:Form:Clause
                                      :condition (car it)
                                      :body (cdr it))
                                     forms)
                     :type (Emil:Type:Any)))))

  (fn Transformer:transform-defconst (self _form symbol init-value &optional
                                           documentation context environment
                                           &rest _)
    (-let (((context . init-form)
            (Emil:Analyzer:infer self init-value context environment)))
      (cons context (Emil:Form:DefConst
                     :symbol symbol
                     :init-value init-form
                     :documentation documentation
                     :type (Emil:Type:Basic :name 'symbol)))))

  (fn Transformer:transform-defvar (self _form symbol &optional init-value
                                         documentation context environment
                                         &rest _)
    (-let (((context . init-form)
            (Emil:Analyzer:infer self init-value context environment)))
      (cons context (Emil:Form:DefVar
                     :symbol symbol
                     :init-value init-form
                     :documentation documentation
                     :type (Emil:Type:Basic :name 'symbol)))))

  (fn Transformer:transform-function (self _form argument
                                           &optional context environment &rest _)
    (pcase-exhaustive argument
      ((and `(lambda ,argument-list . ,body)
            (guard (listp argument-list)))
       (-let* ((arity (func-arity argument))
               (min-arity (car arity))
               (rest? (eq 'many (cdr arity)))
               (variables
                (Emil:Util:lambda-variables argument-list))
               (variables-count (length variables))
               (arguments (Emil:Analyzer:generate-existentials
                           self variables-count))
               (variable-types
                (if (not rest?) arguments
                  (append (butlast arguments)
                          (list (Emil:Type:Compound
                                 :name 'List :arguments (last arguments))))))
               (returns (Emil:Analyzer:generate-existential self))
               (bindings (--map (Emil:Context:Binding
                                 :variable (car it) :type (cdr it))
                                (-zip-pair variables variable-types)))
               (marker (Emil:Context:Marker))
               (body-environment (Emil:Env:Alist :parent environment))
               (initial-context
                (Emil:Context:concat
                 (reverse bindings) marker returns
                 (reverse arguments) context))
               ((body-context . body-forms)
                (Emil:Analyzer:check-do
                 self body returns initial-context body-environment))
               (type (Emil:Type:Arrow* arguments returns min-arity rest?)))
         (Emil:Env:Alist:update-from body-environment body-context
                                     variables nil)
         (cons (Emil:Context:drop-until-after body-context marker)
               (Emil:Form:Function
                :value (Emil:Form:Lambda
                        :arguments argument-list
                        :body body-forms)
                :type type))))
      ((pred symbolp)
       (let ((type (or (Emil:Analyzer:lookup-function argument context environment)
                       (Emil:type-error "Unbound function: %s" argument))))
         (cons context
               (Emil:Form:Function
                :value argument
                :type type))))
      (value
       (Emil:type-error "Not a function: %s" value))))

  (fn Transformer:transform-if (self _form condition then else
                                     &optional context environment
                                     &rest _)
    (-let (((context . forms)
            (Emil:Analyzer:infer-do self `(,condition ,then ,@else) context environment)))
      (cons context
            (Emil:Form:If
             :condition (nth 0 forms)
             :then (nth 1 forms)
             :else (nthcdr 2 forms)
             :type (Emil:Type:Any)))))

  (fn Transformer:transform-interactive (_self form _descriptor _modes
                                               &optional context _environment
                                               &rest _)
    (cons context (Emil:Form:Interactive
                   :forms (cdr form)
                   :type (Emil:Type:Any))))

  (fn Transformer:transform-let (self _form bindings body &optional
                                      context environment &rest _)
    (setq bindings (--map (if (consp it) it (list it nil)) bindings))
    (-let* ((variables (--map (car it) bindings))
            ((bindings-context . binding-forms)
             (Emil:Util:map-reduce
              (-lambda (context (_variable value))
                (Emil:Analyzer:infer self value context environment))
              context
              bindings))
            (context-bindings
             (--map (Emil:Context:Binding
                     :variable (car it) :type (Struct:get (cdr it) :type))
                    (-zip-pair variables binding-forms)))
            (marker (Emil:Context:Marker))
            (body-environment (Emil:Env:Alist :parent environment))
            ((body-context . body-forms)
             (Emil:Analyzer:infer-do
              self body (Emil:Context:concat
                         (reverse context-bindings)
                         marker bindings-context)
              body-environment))
            (body-type (Emil:Analyzer:type-of-body body-forms)))
      (Emil:Env:Alist:update-from body-environment body-context
                                  variables nil)
      (cons (Emil:Context:drop-until-after body-context marker)
            (Emil:Form:Let
             :kind 'let
             :bindings (--map (Emil:Form:Binding :name (car it) :value (cdr it))
                              (-zip-pair variables binding-forms))
             :body body-forms
             :type (Emil:Context:resolve body-context body-type)))))

  (fn Transformer:transform-let* (self _form bindings body &optional
                                       context environment &rest _)
    (setq bindings (--map (if (consp it) it (list it nil)) bindings))
    (-let* ((marker (Emil:Context:Marker))
            (variables (--map (car it) bindings))
            (((bindings-context . body-environment) . binding-forms)
             (Emil:Analyzer:thread-let*-bindings
              self bindings (Emil:Context:concat marker context) environment))
            ((body-context . body-forms)
             (Emil:Analyzer:infer-do self body bindings-context body-environment))
            (body-type (Emil:Analyzer:type-of-body body-forms)))
      (--each (nreverse (--iterate (Struct:get it :parent)
                                   body-environment
                                   (length variables)))
        (Emil:Env:Alist:update-from
         it body-context (list (nth it-index variables)) nil t))
      (cons (Emil:Context:drop-until-after body-context marker)
            (Emil:Form:Let
             :kind 'let*
             :bindings (--map (Emil:Form:Binding :name (car it) :value (cdr it))
                              (-zip-pair variables binding-forms))
             :body body-forms
             :type (Emil:Context:resolve body-context body-type)))))

  (fn Transformer:transform-or (self _form conditions &optional context environment
                                     &rest _)
    (-let (((context . condition-forms)
            (Emil:Analyzer:infer-do self conditions context environment)))
      (cons context (Emil:Form:Or
                     :conditions condition-forms
                     :type (Emil:Type:Any)))))

  (fn Transformer:transform-prog1 (self _form first body
                                        &optional context environment
                                        &rest _)
    (-let* (((context . prog1-forms)
             (Emil:Analyzer:infer-do self (cons first body) context environment))
            (type (Struct:get (car  prog1-forms) :type)))
      (cons context (Emil:Form:Prog1
                     :first (car prog1-forms)
                     :body (cdr prog1-forms)
                     :type type))))

  (fn Transformer:transform-progn (self form _body &optional context environment
                                        &rest _)
    (Emil:Analyzer:infer-progn-like self form context environment))

  (fn Transformer:transform-quote (_self _form argument
                                         &optional context _environment &rest _)
    (cons context (Emil:Form:Quote
                   :value argument
                   :type (Emil:Type:Any))))

  (fn Transformer:transform-save-current-buffer (self form _body
                                                      &optional context environment
                                                      &rest _)
    (Emil:Analyzer:infer-progn-like self form context environment))

  (fn Transformer:transform-save-excursion (self form _body
                                                 &optional context environment
                                                 &rest _)
    (Emil:Analyzer:infer-progn-like self form context environment))

  (fn Transformer:transform-save-restriction (self form _body
                                                   &optional context environment
                                                   &rest _)
    (Emil:Analyzer:infer-progn-like self form context environment))

  (fn Transformer:transform-setq (self _form definitions
                                       &optional context environment
                                       &rest _)
    (-let* ((names (--filter (= 0 (% it-index 2)) definitions))
            (values (--filter (= 1 (% it-index 2)) definitions))
            ((context . value-forms)
             (Emil:Analyzer:infer-do self values context environment)))
      (cons context (Emil:Form:Setq
                     :bindings (--map (Emil:Form:Binding :name (pop names) :value it)
                                      value-forms)
                     :type (Emil:Type:Any)))))

  (fn Transformer:transform-unwind-protect (self _form body-form unwind-forms
                                                 &optional context environment
                                                 &rest _)
    (-let* (((context . unwind-protect-forms)
             (Emil:Analyzer:infer-do self (cons body-form unwind-forms) context environment))
            (type (Struct:get (car unwind-protect-forms) :type)))
      (cons context (Emil:Form:UnwindProtect
                     :body-form (car unwind-protect-forms)
                     :unwind-forms (cdr unwind-protect-forms)
                     :type type))))

  (fn Transformer:transform-while (self _form condition body
                                        &optional context environment &rest _)
    (-let* (((context . while-forms)
             (Emil:Analyzer:infer-do self (cons condition body) context environment)))
      (cons context (Emil:Form:While
                     :condition (car while-forms)
                     :body (cdr while-forms)
                     :type (Emil:Type:Null)))))

  (fn Transformer:transform-application (self _form function arguments
                                              &optional context environment
                                              &rest _)

    (-let* (((function-context . function-form)
             (Transformer:transform-function
              self `(function ,function) function context environment))
            ((context return-type . argument-forms)
             (Emil:Analyzer:infer-application
              self (Emil:Context:resolve
                    function-context (Struct:get function-form :type))
              (--map (Emil:Analyzer:macroexpand-maybe it environment)
                     arguments)
              function-context environment)))
      (cons context
            (Emil:Form:Application
             :function (Emil:Form:ApplicationFn
                        :value (Struct:get function-form :value)
                        :type (Struct:get function-form :type))
             :arguments argument-forms
             :type return-type))))

  (fn Transformer:transform-macro (self form macro arguments
                                        &optional context environment &rest _)
    (if (memq macro Emil:Annotation:macros)
        (Emil:Analyzer:transform-annotation
         self form macro arguments context environment)
      (Emil:Analyzer:infer
       self (macroexpand form (Emil:Env:macro-environment environment))
       context environment))))

(provide 'Emil/Transformer)
