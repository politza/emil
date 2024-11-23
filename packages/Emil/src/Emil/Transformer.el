;; -*- lexical-binding: t -*-

(require 'dash)
(require 'Transformer)
(require 'Emil/Analyzer)

(defmacro Emil:is (_type form)
  "Declare that FORM is of type TYPE.

Apart from that, this just expands to FORM.

\(fn TYPE FORM\)"
  (declare (indent 1))
  form)

(Struct:implement Emil:Analyzer
  (fn Emil:Analyzer:infer-do (self forms (context Emil:Context)
                                   (environment (Trait Emil:Env)))
    (Emil:Util:map-reduce
     (-lambda (context form)
       (Emil:Analyzer:infer self form context environment))
     context
     forms))
  
  (fn Emil:Analyzer:infer-progn-like (self form (context Emil:Context)
                                           (environment (Trait Emil:Env)))
    (-let* ((body (cdr (Transformer:Form:value form)))
            ((context . forms)
             (Emil:Analyzer:infer-do self body context environment)))
      (cons context (Emil:TypedForm:new
                     (cons (car (Transformer:Form:value form)) forms)
                     (Emil:Analyzer:type-of-body forms)
                     environment))))

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
      (Emil:Type:Null))))

(Trait:implement Transformer Emil:Analyzer
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
                  (or (Emil:Analyzer:lookup-variable symbol context environment)
                      (Emil:type-error "Unbound variable: %s" symbol))))))
      (cons context (Emil:TypedForm:new form type environment))))

  (fn Transformer:transform-and (self form _conditions &optional context environment
                                      &rest _)
    (-let (((context . typed-form)
            (Emil:Analyzer:infer-progn-like self form context environment)))
      (cons context (Emil:TypedForm* ,@typed-form :type (Emil:Type:Any)))))

  (fn Transformer:transform-catch (self form _tag _body &optional context environment
                                        &rest _)
    (-let (((context . typed-form)
            (Emil:Analyzer:infer-progn-like self form context environment)))
      (cons context (Emil:TypedForm* ,@typed-form :type (Emil:Type:Any)))))

  (fn Transformer:transform-cond (self form clauses &optional context environment
                                       &rest _)
    (-let (((context . forms)
            (Emil:Util:map-reduce
             (lambda (context clause)
               (Emil:Analyzer:infer-do self clause context environment))
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
            (Emil:Analyzer:infer self init-value context environment)))
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
            (Emil:Analyzer:infer self init-value context environment)))
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
               (arguments (Emil:Analyzer:generate-existentials
                           self (length variables)))
               (returns (Emil:Analyzer:generate-existential self))
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
                (Emil:Analyzer:check-do self body
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
       (let ((type (or (Emil:Analyzer:lookup-function argument context environment)
                       (Emil:type-error "Unbound function: %s" argument))))
         (cons context
               (Emil:TypedForm:new form type environment))))
      (value
       (Emil:type-error "Not a function: %s" value))))

  (fn Transformer:transform-if (self form condition then else
                                     &optional context environment
                                     &rest _)
    (-let (((context . forms)
            (Emil:Analyzer:infer-do self `(,condition ,then ,@else) context environment)))
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
                (Emil:Analyzer:infer self binding context environment))
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
            (Emil:TypedForm:new
             (cons (car form)
                   (cons (-zip-with #'list variables binding-forms)
                         body-forms))
             (Emil:Context:resolve body-context body-type)
             environment))))

  (fn Transformer:transform-or (self form _conditions &optional context environment
                                     &rest _)
    (-let (((context . typed-form)
            (Emil:Analyzer:infer-progn-like self form context environment)))
      (cons context (Emil:TypedForm* ,@typed-form :type (Emil:Type:Any)))))

  (fn Transformer:transform-prog1 (self form _first _body
                                        &optional context environment
                                        &rest _)
    (-let* (((context . typed-form)
             (Emil:Analyzer:infer-progn-like self form context environment))
            (type (Struct:get (nth 1 (Struct:get typed-form :form)) :type)))
      (cons context (Emil:TypedForm* ,@typed-form type))))

  (fn Transformer:transform-progn (self form _body &optional context environment
                                        &rest _)
    (Emil:Analyzer:infer-progn-like self form context environment))

  (fn Transformer:transform-quote (_self form _argument
                                         &optional context environment &rest _)
    (cons context (Emil:TypedForm:new form (Emil:Type:Any) environment)))

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
             (Emil:Analyzer:infer-progn-like self form context environment))
            (type (Struct:get (nth 1 (Struct:get typed-form :form)) :type)))
      (cons context (Emil:TypedForm* ,@typed-form type))))

  (fn Transformer:transform-while (self form _condition _body
                                        &optional context environment &rest _)
    (Emil:Analyzer:infer-progn-like self form context environment))

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
        (Emil:Analyzer:check self (nth 1 arguments) type context environment)))
     (t
      (Emil:Analyzer:infer
       self (macroexpand (Transformer:Form:unwrap form)) context environment)))))

(provide 'Emil/Transformer)
