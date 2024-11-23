;; -*- lexical-binding: t -*-

(require 'Trait)
(require 'Struct)
(require 'Emil/Type)
(require 'Emil/Context)
(require 'Emil/Env)

(Trait:define Emil:TypedForm ()
  (fn Emil:TypedForm:environment (self -> (Trait Emil:Env))
    (Struct:get self :environment))

  (fn Emil:TypedForm:value (self))

  (fn Emil:TypedForm:position (self)
    (Struct:get self :position)))

(Trait:implement Emil:Env Emil:TypedForm
  (fn Emil:Env:lookup-variable (self variable &optional context)
    (Emil:Env:lookup-variable (Emil:TypedForm:environment self) variable context))

  (fn Emil:Env:lookup-function (self function &optional context)
    (Emil:Env:lookup-function (Emil:TypedForm:environment self) function context)))

(defun Emil:TypedForm:with-type (form type)
  (cl-check-type form (Trait Emil:TypedForm))
  (cl-check-type type (Trait Emil:Type))
  (let ((clone (copy-sequence form)))
    (Struct:unsafe-set clone :type type)
    clone))

(defun Emil:TypedForm? (object)
  (Trait:implements? 'Emil:TypedForm (Trait:type-of object)))

(Struct:define Emil:TypedForm:Basic
  (value)
  (type :type (Trait Emil:Type))
  (environment :type (Trait Emil:Env)))

(Struct:define Emil:TypedForm:Application
  (function :type Emil:TypedForm:Function)
  (arguments :type (List (Trait Emil:TypedForm)))
  (type :type (Trait Emil:Type))
  (environment :type (Trait Emil:Env)))

(Trait:implement Emil:TypedForm Emil:TypedForm:Application
  (fn Emil:TypedForm:value (self)
    `(,(nth 1 (Emil:TypedForm:value (Struct:get self :function)))
      ,@(Emil:TypedForm:value (Struct:get self :arguments)))))

(Struct:define Emil:TypedForm:Macro
  (form)
  (expansion :type (Trait Emil:TypedForm))
  (type :type (Trait Emil:Type))
  (environment :type (Trait Emil:Env)))

(Trait:implement Emil:TypedForm Emil:TypedForm:Macro
  (fn Emil:TypedForm:value (self)
    (Emil:TypedForm:value (Struct:get self :expansion))))

(Struct:define Emil:TypedForm:AnyForm
  (form)
  (type :type (Trait Emil:Type))
  (environment :type (Trait Emil:Env)))

(Trait:implement Emil:TypedForm Emil:TypedForm:AnyForm
  (fn Emil:TypedForm:value (self)
    (Struct:get self :form)))

(Trait:implement Emil:TypedForm Emil:TypedForm:Basic
  (fn Emil:TypedForm:value (self)
    (Struct:get self :value)))

(Struct:define Emil:TypedForm:And
  (conditions :type (List (Trait Emil:TypedForm)))
  (type :type (Trait Emil:Type))
  (environment :type (Trait Emil:Env)))

(Trait:implement Emil:TypedForm Emil:TypedForm:And
  (fn Emil:TypedForm:value (self)
    `(and
      ,@(-map #'Emil:TypedForm:value (Struct:get self :conditions)))))

(Struct:define Emil:TypedForm:Catch
  (tag :type (Trait Emil:TypedForm))
  (body :type (List (Trait Emil:TypedForm)))
  (type :type (Trait Emil:Type))
  (environment :type (Trait Emil:Env)))

(Trait:implement Emil:TypedForm Emil:TypedForm:Catch
  (fn Emil:TypedForm:value (self)
    `(catch
         ,(Emil:TypedForm:value (Struct:get self :tag))
       ,@(-map #'Emil:TypedForm:value (Struct:get self :body)))))

(Struct:define Emil:TypedForm:Cond
  (clauses :type (List Emil:TypedForm:Clause))
  (type :type (Trait Emil:Type))
  (environment :type (Trait Emil:Env)))

(Struct:define Emil:TypedForm:Clause
  (condition :type (Trait Emil:TypedForm))
  (body :type (List (Trait Emil:TypedForm))))

(Trait:implement Emil:TypedForm Emil:TypedForm:Cond
  (fn Emil:TypedForm:value (self)
    `(cond
      ,@(-map #'Emil:TypedForm:value (Struct:get self :clauses)))))

(Struct:define Emil:TypedForm:DefConst
  (symbol :type symbol)
  (init-value :type (Trait Emil:TypedForm))
  (documentation :type (or null string))
  (type :type (Trait Emil:Type))
  (environment :type (Trait Emil:Env)))

(Trait:implement Emil:TypedForm Emil:TypedForm:DefConst
  (fn Emil:TypedForm:value (self)
    `(defconst
       ,(Emil:TypedForm:value (Struct:get self :symbol))
       ,(Emil:TypedForm:value (Struct:get self :init-value))
       ,(Emil:TypedForm:value (Struct:get self :documentation)))))

(Struct:define Emil:TypedForm:DefVar
  (symbol :type symbol)
  (init-value :type (Trait Emil:TypedForm))
  (documentation :type (or null string))
  (type :type (Trait Emil:Type))
  (environment :type (Trait Emil:Env)))

(Trait:implement Emil:TypedForm Emil:TypedForm:DefVar
  (fn Emil:TypedForm:value (self)
    `(defvar
       ,(Emil:TypedForm:value (Struct:get self :symbol))
       ,(Emil:TypedForm:value (Struct:get self :init-value))
       ,(Emil:TypedForm:value (Struct:get self :documentation)))))

(Struct:define Emil:TypedForm:Function
  (value :type (or symbol (Emil:TypedForm:Lambda)))
  (type :type (Trait Emil:Type))
  (environment :type (Trait Emil:Env)))

(Trait:implement Emil:TypedForm Emil:TypedForm:Function
  (fn Emil:TypedForm:value (self)
    (let ((value (Struct:get self :value)))
      (list 'function
            (if (not (Emil:TypedForm:Lambda? value))
                value
              `(lambda ,(Struct:get value :arguments)
                 (-map #'Emil:TypedForm:value
                       (Struct:get value :body))))))))

(Struct:define Emil:TypedForm:Lambda
  (arguments :type list)
  (body :type (List (Trait Emil:TypedForm))))

(Struct:define Emil:TypedForm:If
  (condition :type (Trait Emil:TypedForm))
  (then :type (Trait Emil:TypedForm))
  (else :type (List (Trait Emil:TypedForm)))
  (type :type (Trait Emil:Type))
  (environment :type (Trait Emil:Env)))

(Trait:implement Emil:TypedForm Emil:TypedForm:If
  (fn Emil:TypedForm:value (self)
    `(if
       ,(Emil:TypedForm:value (Struct:get self :condition))
       ,(Emil:TypedForm:value (Struct:get self :then))
       ,(Emil:TypedForm:value (Struct:get self :else)))))

(Struct:define Emil:TypedForm:Interactive
  (forms)
  (type :type (Trait Emil:Type))
  (environment :type (Trait Emil:Env)))

(Trait:implement Emil:TypedForm Emil:TypedForm:Interactive
  (fn Emil:TypedForm:value (self)
    `(interactive ,@(Struct:get self :forms))))

(Struct:define Emil:TypedForm:Let
  (kind :type (member let let*))
  (bindings :type (List Emil:TypedForm:Binding))
  (body :type (List (Trait Emil:TypedForm)))
  (type :type (Trait Emil:Type))
  (environment :type (Trait Emil:Env)))

(Trait:implement Emil:TypedForm Emil:TypedForm:Let
  (fn Emil:TypedForm:value (self)
    `(let ,@(--map (list (Struct:get it :name)
                         (Emil:TypedForm:value (Struct:get it :name)))
                   (Struct:get self :bindings))
       ,@(-map #'Emil:TypedForm:value (Struct:get self :body)))))

(Struct:define Emil:TypedForm:Binding
  (name :type symbol)
  (value :type (Trait Emil:TypedForm)))

(Struct:define Emil:TypedForm:Or
  (conditions :type (List (Trait Emil:TypedForm)))
  (type :type (Trait Emil:Type))
  (environment :type (Trait Emil:Env)))

(Trait:implement Emil:TypedForm Emil:TypedForm:Or
  (fn Emil:TypedForm:value (self)
    `(or
      ,@(-map #'Emil:TypedForm:value (Struct:get self :conditions)))))

(Struct:define Emil:TypedForm:Prog1
  (first :type (Trait Emil:TypedForm))
  (body :type (List (Trait Emil:TypedForm)))
  (type :type (Trait Emil:Type))
  (environment :type (Trait Emil:Env)))

(Trait:implement Emil:TypedForm Emil:TypedForm:Prog1
  (fn Emil:TypedForm:value (self)
    `(prog1
         (Emil:TypedForm:value (Struct:get self :first))
       ,@(-map #'Emil:TypedForm:value (Struct:get self :body)))))

(Struct:define Emil:TypedForm:PrognLike
  (kind :type (member progn save-current-buffer save-excursion save-restriction))
  (body :type (List (Trait Emil:TypedForm)))
  (type :type (Trait Emil:Type))
  (environment :type (Trait Emil:Env)))

(Trait:implement Emil:TypedForm Emil:TypedForm:PrognLike
  (fn Emil:TypedForm:value (self)
    `(,(Struct:get self :kind)
      ,@(-map #'Emil:TypedForm:value (Struct:get self :body)))))

(Struct:define Emil:TypedForm:Quote
  (value)
  (type :type (Trait Emil:Type))
  (environment :type (Trait Emil:Env)))

(Trait:implement Emil:TypedForm Emil:TypedForm:Quote
  (fn Emil:TypedForm:value (self)
    `(quote ,(Struct:get self :value))))

(Struct:define Emil:TypedForm:Setq
  (bindings :type (List Emil:TypedForm:Binding))
  (type :type (Trait Emil:Type))
  (environment :type (Trait Emil:Env)))

(Trait:implement Emil:TypedForm Emil:TypedForm:Setq
  (fn Emil:TypedForm:value (self)
    `(setq ,@(--mapcat (list (Struct:get it :name)
                             (Emil:TypedForm:value (Struct:get it :name)))
                       (Struct:get self :bindings)))))

(Struct:define Emil:TypedForm:UnwindProtect
  (body-form :type (Trait Emil:TypedForm))
  (unwind-forms :type (List (Trait Emil:TypedForm)))
  (type :type (Trait Emil:Type))
  (environment :type (Trait Emil:Env)))

(Trait:implement Emil:TypedForm Emil:TypedForm:UnwindProtect
  (fn Emil:TypedForm:value (self)
    `(unwind-protect
         ,(Emil:TypedForm:value (Struct:get self :body-form))
       ,@(-map #'Emil:TypedForm:value (Struct:get self :unwind-forms)))))

(Struct:define Emil:TypedForm:While
  (condition :type (Trait Emil:TypedForm))
  (body :type (List (Trait Emil:TypedForm)))
  (type :type (Trait Emil:Type))
  (environment :type (Trait Emil:Env)))

(Trait:implement Emil:TypedForm Emil:TypedForm:While
  (fn Emil:TypedForm:value (self)
    `(while ,(Emil:TypedForm:value (Struct:get self :condition))
       ,@(-map #'Emil:TypedForm:value (Struct:get self :body)))))

(provide 'Emil/TypedForm)
