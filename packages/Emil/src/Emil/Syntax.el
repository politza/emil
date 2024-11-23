;; -*- lexical-binding: t -*-

(require 'dash)
(require 'Struct)
(require 'Trait)
(require 'Emil/Env)
(require 'Emil/Type)
(require 'Emil/Form)
(require 'cl-lib)
(require 'elisp-mode)

(declare-function Emil:transform "Emil" (form &optional env))

(Struct:define Emil:Syntax env)

(defun Emil:Syntax:transform-form (self form &optional env)
  (cl-labels ((recurse (form &optional local-env)
                (Emil:Syntax:transform-form
                 self form
                 (if local-env (Emil:Env:Alist :variables local-env :parent env) env))))
    (pcase-exhaustive form
      ((Struct Emil:Form:Atom value)
       (cond
        ((not (symbolp value)) value)
        ((Emil:Syntax:resolve-variable self value env)
         (--reduce `(Struct:unsafe-get ,acc ,(Commons:symbol-to-keyword it))
                   (-map #'intern (split-string (symbol-name value) "[.]"))))
        (t value)))
      ((Struct Emil:Form:Application function arguments)
       (-let* ((function-value (Struct:get function :value))
               ((transformed-function . self-form)
                (Emil:Syntax:transform-function self function-value env))
               (transformed-arguments (-map #'recurse arguments)))
         (if self-form
             `(,transformed-function ,self-form ,@transformed-arguments)
           `(,transformed-function ,@transformed-arguments))))
      ((Struct Emil:Form:And conditions)
       `(and ,@(-map #'recurse conditions)))
      ((Struct Emil:Form:Catch tag body)
       `(catch ,(recurse tag) ,@(-map #'recurse body)))
      ((Struct Emil:Form:Cond clauses)
       `(cond ,@(-map
                 (-lambda (clause)
                   (cons (recurse (Struct:get clause :condition))
                         (-map #'recurse (Struct:get clause :body))))
                 clauses)))
      ((Struct Emil:Form:ConditionCase variable body-form handlers)
       (let ((env (list (cons variable (Emil:Type:Any)))))
         `(condition-case ,variable
              ,(recurse body-form)
            ,@(-map
               (-lambda (handler)
                 (cons (Struct:get handler :condition)
                       (--map (recurse it env) (Struct:get handler :body))))
               handlers))))
      ((Struct Emil:Form:DefConst symbol init-value documentation)
       `(defconst ,symbol ,(recurse init-value) ,documentation))
      ((Struct Emil:Form:DefVar symbol init-value documentation)
       `(defvar ,symbol ,(recurse init-value) ,documentation))
      ((Struct Emil:Form:Function value)
       (-let* (((transformed-function . self-form)
                (Emil:Syntax:transform-function self value env)))
         (if self-form
             `(function (lambda () (,transformed-function ,self-form)))
           `(function ,transformed-function))))
      ((Struct Emil:Form:If condition then else)
       `(if ,(recurse condition) ,(recurse then) ,@(-map #'recurse else)))
      ((Struct Emil:Form:Interactive forms)
       `(interactive ,@(-map #'recurse forms)))
      ((Struct Emil:Form:Let kind bindings body)
       (let ((env (--map (cons (Struct:get it :name)
                               (Struct:get (Struct:get it :value) :type))
                         bindings)))
         `(,kind ,(--map-indexed
                   (list (Struct:get it :name)
                         (recurse (Struct:get it :value)
                                  (if (eq kind 'let*) (-take it-index env))))
                   bindings)
                 ,@(--map (recurse it env) body))))
      ((Struct Emil:Form:Or conditions)
       `(or ,@(-map #'recurse conditions)))
      ((Struct Emil:Form:Prog1 first body)
       `(prog1 ,(recurse first) ,@(-map #'recurse body)))
      ((Struct Emil:Form:PrognLike kind body)
       `(,kind ,@(-map #'recurse body)))
      ((Struct Emil:Form:Quote value)
       `(quote ,value))
      ((Struct Emil:Form:Setq bindings)
       `(setq ,@(-flatten-n 1 (--map (list (Struct:get it :name)
                                           (recurse (Struct:get it :value)))
                                     bindings))))
      ((Struct Emil:Form:UnwindProtect body-form unwind-forms)
       `(unwind-protect ,(recurse body-form) ,@(-map #'recurse unwind-forms)))
      ((Struct Emil:Form:While condition body)
       `(while ,(recurse condition) ,@(-map #'recurse body))))))

(defun Emil:Syntax:transform-function (self function &optional env)
  (cond
   ((Emil:Form:Lambda? function)
    (let ((local-env (-zip-pair (Emil:Util:lambda-variables
                                 (Struct:get function :arguments))
                                (Emil:Type:Arrow:arguments
                                 (Struct:get function :type)))))
      (list `(lambda ,(Struct:get function :arguments)
               ,@(--map (Emil:Syntax:transform-form self it local-env)
                        (Struct:get function :body))))))
   (t
    (or (when-let (function-struct (Emil:Syntax:resolve-function self function env))
          (cons (Struct:get function-struct :qualified-name)
                (--reduce `(Struct:unsafe-get ,acc ,(Commons:symbol-to-keyword it))
                          (butlast (-map #'intern (split-string (symbol-name function) "[.]"))))))
        (list function)))))

(defun Emil:Syntax:resolve-expression (self expression &optional env)
  (when-let* ((components
               (-map #'intern (when (Emil:Syntax:dot-expression? expression)
                                (split-string (symbol-name expression) "[.]"))))
              (type (-reduce-from
                     (lambda (type name)
                       (if-let* ((struct (and (Emil:Type:Basic? type)
                                              (Struct:Type:get
                                               (Struct:get type :name))))
                                 (property (Struct:Type:get-property
                                            struct (Commons:symbol-to-keyword name)))
                                 (type (Struct:get property :type)))
                           (Emil:Type:read type)
                         (Emil:type-error "Property %s does not exist in type %s"
                                          name (Emil:Type:print type))))
                     (or (and env (Emil:Env:lookup-variable env (car components)))
                         (Emil:Env:lookup-variable (Struct:get self :env)
                                                   (car components) env)
                         (Emil:type-error "Variable %s not bound" (car components)))
                     (cdr (butlast components)))))
    (cons (car (last components)) type)))

(defun Emil:Syntax:dot-expression? (expression)
  (with-syntax-table emacs-lisp-mode-syntax-table
    (string-match-p
     "\\`\\(?:\\w\\|\\s_\\)+\\(?:[.]\\(?:\\w\\|\\s_\\)+\\)+\\'"
     (symbol-name expression))))

(defun Emil:Syntax:resolve-variable (self expression &optional env)
  (-when-let* (((property . type)
                (Emil:Syntax:resolve-expression self expression env))
               (struct (or (and (Emil:Type:Basic? type)
                                (Struct:Type:get (Struct:get type :name)))
                           (Emil:type-error "Property %s does not exist in type %s"
                                            property (Emil:Type:print type)))))
    (or (Struct:Type:get-property struct (Commons:symbol-to-keyword property))
        (Emil:type-error "Property %s does not exist in type %s"
                         property (Emil:Type:print type)))))

(defun Emil:Syntax:resolve-function (self expression &optional env)
  (-when-let* (((name . type)
                (Emil:Syntax:resolve-expression self expression env)))
    (let* ((functions (Emil:Syntax:type-functions type))
           (candidates
            (--filter (eq name (Struct:get it :name)) functions)))
      (pcase (length candidates)
        (0 (Emil:type-error "Method %s does not exist on type %s"
                            name (Emil:Type:print type)))
        (1 (car candidates))
        (_ (Emil:type-error
            "Multiple methods named %s exist for type %s: %s"
            name (Emil:Type:print type)
            (-map #'Struct:Function:type functions)))))))

(defun Emil:Syntax:type-functions (type)
  (pcase type
    ((Struct Emil:Type:Basic name)
     (append (Struct:functions name)
             (Trait:implemented-functions name)))
    ((pred Emil:Type:trait?)
     (Trait:functions
      (Struct:get (car (Struct:get type :arguments)) :name)))))

(defun Emil:Syntax:defined-functions (self type)
  (when (and (Struct:get self :defined-type)
             (equal type (Struct:get self :defined-type)))
    (Struct:get self :defined-functions)))

(defun Emil:Syntax:transform (function)
  (let ((env (Emil:Syntax
              :env
              (Emil:Env:Alist :variables (Emil:Syntax:Function:bindings function)))))
    (cdr (Emil:Syntax:transform-form
          env
          (Emil:transform `(progn ,@(Struct:get function :body)) env)))))

(defun Emil:Syntax:Function:bindings (fn)
  (let ((names (--map (Struct:get it :name) (Struct:get fn :arguments)))
        (types (Emil:Type:Arrow:arguments
                (Emil:Type:read-function (Struct:Function:type fn)))))
    (-zip-pair names types)))

(defun Emil:Syntax:expand-setf (self form &optional env)
  (if (and (= 2 (length form))
           (Emil:Syntax:resolve-variable self (car form) env))
      (let* ((components
              (-map #'intern (split-string (symbol-name (car form)) "[.]")))
             (accesor (--reduce `(Struct:unsafe-get
                                  ,acc ,(Commons:symbol-to-keyword it))
                                (butlast components))))
        `(Struct:set ,accesor ,(Commons:symbol-to-keyword
                                (car (last components)))
           ,(cadr form)))
    (macroexpand (cons 'setf form))))


(Trait:implement Emil:Env Emil:Syntax
  :disable-syntax t
  (fn Emil:Env:lookup-variable (self (name symbol)
                                     &optional locals)
    (or (when-let ((property (Emil:Syntax:resolve-variable self name locals))
                   (type (Struct:get property :type 'Any)))
          (Emil:Type:read type))
        (Emil:Env:lookup-variable (Struct:get self :env) name locals)))

  (fn Emil:Env:lookup-function (self (name symbol)
                                     &optional locals)
    (when-let (function (Emil:Syntax:resolve-function self name locals))
      (when (Struct:Function:method? function)
        (Emil:Type:read-function (Struct:Function:type function :as-method)))))

  (fn Emil:Env:macro-environment (self &optional locals)
    `((setf . ,(lambda (&rest form) (Emil:Syntax:expand-setf self form locals))))))

(provide 'Emil/Syntax)
