;; -*- lexical-binding: t -*-

(require 'Trait)
(require 'Struct)
(require 'Emil/Type)
(require 'Emil/Context)
(require 'Emil/Env)

;; Shut up cl-type-check's compiler warnings.
(cl-deftype Emil:TypedForm nil t)

(Trait:define Emil:TypedForm ()
  :disable-syntax t
  (fn Emil:TypedForm:environment (self -> (Trait Emil:Env))
    (Struct:get self :environment))

  (fn Emil:TypedForm:value (self)
    (Emil:TypedForm:map self #'identity))

  (fn Emil:TypedForm:map (self fn))

  (fn Emil:TypedForm:position (self)
    (Struct:get self :position)))

(Trait:implement Emil:Env Emil:TypedForm
  :disable-syntax t
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

(Struct:define Emil:TypedForm:Atom
  (value)
  (type :type (Trait Emil:Type))
  (environment :type (Trait Emil:Env)))

(Trait:implement Emil:TypedForm Emil:TypedForm:Atom
  :disable-syntax t
  (fn Emil:TypedForm:map (self _fn)
    (Struct:get self :value)))

(Struct:define Emil:TypedForm:Application
  (function :type (or Emil:TypedForm:Function Emil:TypedForm:Lambda))
  (arguments :type (List (Trait Emil:TypedForm)))
  (type :type (Trait Emil:Type))
  (environment :type (Trait Emil:Env)))

(Trait:implement Emil:TypedForm Emil:TypedForm:Application
  :disable-syntax t
  (fn Emil:TypedForm:map (self fn)
    `(,(nth 1 (funcall fn (Struct:get self :function fn)))
      ,@(-map fn (Struct:get self :arguments fn)))))

(Struct:define Emil:TypedForm:Invalid
  (form)
  (type :type (Trait Emil:Type))
  (environment :type (Trait Emil:Env)))

(Trait:implement Emil:TypedForm Emil:TypedForm:Invalid
  :disable-syntax t
  (fn Emil:TypedForm:map (self _fn)
    (Struct:get self :form)))

(Struct:define Emil:TypedForm:And
  (conditions :type (List (Trait Emil:TypedForm)))
  (type :type (Trait Emil:Type))
  (environment :type (Trait Emil:Env)))

(Trait:implement Emil:TypedForm Emil:TypedForm:And
  :disable-syntax t
  (fn Emil:TypedForm:map (self fn)
    `(and
      ,@(-map fn (Struct:get self :conditions fn)))))

(Struct:define Emil:TypedForm:Catch
  (tag :type (Trait Emil:TypedForm))
  (body :type (List (Trait Emil:TypedForm)))
  (type :type (Trait Emil:Type))
  (environment :type (Trait Emil:Env)))

(Trait:implement Emil:TypedForm Emil:TypedForm:Catch
  :disable-syntax t
  (fn Emil:TypedForm:map (self fn)
    `(catch
         ,(funcall fn (Struct:get self :tag fn))
       ,@(-map fn (Struct:get self :body fn)))))

(Struct:define Emil:TypedForm:Cond
  (clauses :type (List Emil:TypedForm:Clause))
  (type :type (Trait Emil:Type))
  (environment :type (Trait Emil:Env)))

(Struct:define Emil:TypedForm:Clause
  (condition :type (Trait Emil:TypedForm))
  (body :type (List (Trait Emil:TypedForm))))

(Trait:implement Emil:TypedForm Emil:TypedForm:Cond
  :disable-syntax t
  (fn Emil:TypedForm:map (self fn)
    `(cond
      ,@(-map fn (Struct:get self :clauses fn)))))

(Struct:define Emil:TypedForm:DefConst
  (symbol :type symbol)
  (init-value :type (Trait Emil:TypedForm))
  (documentation :type (or null string))
  (type :type (Trait Emil:Type))
  (environment :type (Trait Emil:Env)))

(Trait:implement Emil:TypedForm Emil:TypedForm:DefConst
  :disable-syntax t
  (fn Emil:TypedForm:map (self fn)
    `(defconst
       ,(funcall fn (Struct:get self :symbol fn))
       ,(funcall fn (Struct:get self :init-value fn))
       ,(funcall fn (Struct:get self :documentation fn)))))

(Struct:define Emil:TypedForm:DefVar
  (symbol :type symbol)
  (init-value :type (Trait Emil:TypedForm))
  (documentation :type (or null string))
  (type :type (Trait Emil:Type))
  (environment :type (Trait Emil:Env)))

(Trait:implement Emil:TypedForm Emil:TypedForm:DefVar
  :disable-syntax t
  (fn Emil:TypedForm:map (self fn)
    `(defvar
       ,(funcall fn (Struct:get self :symbol fn))
       ,(funcall fn (Struct:get self :init-value fn))
       ,(funcall fn (Struct:get self :documentation fn)))))

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
  :disable-syntax t
  (fn Emil:TypedForm:map (self fn)
    `(if
       ,(funcall fn (Struct:get self :condition fn))
       ,(funcall fn (Struct:get self :then fn))
       ,(funcall fn (Struct:get self :else fn)))))

(Struct:define Emil:TypedForm:Interactive
  (forms)
  (type :type (Trait Emil:Type))
  (environment :type (Trait Emil:Env)))

(Trait:implement Emil:TypedForm Emil:TypedForm:Interactive
  :disable-syntax t
  (fn Emil:TypedForm:map (self _fn)
    `(interactive ,@(Struct:get self :forms))))

(Struct:define Emil:TypedForm:Let
  (kind :type (member let let*))
  (bindings :type (List Emil:TypedForm:Binding))
  (body :type (List (Trait Emil:TypedForm)))
  (type :type (Trait Emil:Type))
  (environment :type (Trait Emil:Env)))

(Trait:implement Emil:TypedForm Emil:TypedForm:Let
  :disable-syntax t
  (fn Emil:TypedForm:map (self fn)
    `(let ,@(--map (list (Struct:get it :name)
                         (funcall fn (Struct:get it :name fn)))
                   (Struct:get self :bindings))
       ,@(-map fn (Struct:get self :body fn)))))

(Struct:define Emil:TypedForm:Binding
  (name :type symbol)
  (value :type (Trait Emil:TypedForm)))

(Struct:define Emil:TypedForm:Or
  (conditions :type (List (Trait Emil:TypedForm)))
  (type :type (Trait Emil:Type))
  (environment :type (Trait Emil:Env)))

(Trait:implement Emil:TypedForm Emil:TypedForm:Or
  :disable-syntax t
  (fn Emil:TypedForm:map (self fn)
    `(or
      ,@(-map fn (Struct:get self :conditions fn)))))

(Struct:define Emil:TypedForm:Prog1
  (first :type (Trait Emil:TypedForm))
  (body :type (List (Trait Emil:TypedForm)))
  (type :type (Trait Emil:Type))
  (environment :type (Trait Emil:Env)))

(Trait:implement Emil:TypedForm Emil:TypedForm:Prog1
  :disable-syntax t
  (fn Emil:TypedForm:map (self fn)
    `(prog1
         (funcall fn (Struct:get self :first fn))
       ,@(-map fn (Struct:get self :body fn)))))

(Struct:define Emil:TypedForm:PrognLike
  (kind :type (member progn save-current-buffer save-excursion save-restriction))
  (body :type (List (Trait Emil:TypedForm)))
  (type :type (Trait Emil:Type))
  (environment :type (Trait Emil:Env)))

(Trait:implement Emil:TypedForm Emil:TypedForm:PrognLike
  :disable-syntax t
  (fn Emil:TypedForm:map (self fn)
    `(,(Struct:get self :kind)
      ,@(-map fn (Struct:get self :body fn)))))

(Struct:define Emil:TypedForm:Quote
  (value)
  (type :type (Trait Emil:Type))
  (environment :type (Trait Emil:Env)))

(Trait:implement Emil:TypedForm Emil:TypedForm:Quote
  :disable-syntax t
  (fn Emil:TypedForm:map (self _fn)
    `(quote ,(Struct:get self :value))))

(Struct:define Emil:TypedForm:Setq
  (bindings :type (List Emil:TypedForm:Binding))
  (type :type (Trait Emil:Type))
  (environment :type (Trait Emil:Env)))

(Trait:implement Emil:TypedForm Emil:TypedForm:Setq
  :disable-syntax t
  (fn Emil:TypedForm:map (self fn)
    `(setq ,@(--mapcat (list (Struct:get it :name)
                             (funcall fn (Struct:get it :name fn)))
                       (Struct:get self :bindings)))))

(Struct:define Emil:TypedForm:UnwindProtect
  (body-form :type (Trait Emil:TypedForm))
  (unwind-forms :type (List (Trait Emil:TypedForm)))
  (type :type (Trait Emil:Type))
  (environment :type (Trait Emil:Env)))

(Trait:implement Emil:TypedForm Emil:TypedForm:UnwindProtect
  :disable-syntax t
  (fn Emil:TypedForm:map (self fn)
    `(unwind-protect
         ,(funcall fn (Struct:get self :body-form fn))
       ,@(-map fn (Struct:get self :unwind-forms fn)))))

(Struct:define Emil:TypedForm:While
  (condition :type (Trait Emil:TypedForm))
  (body :type (List (Trait Emil:TypedForm)))
  (type :type (Trait Emil:Type))
  (environment :type (Trait Emil:Env)))

(Trait:implement Emil:TypedForm Emil:TypedForm:While
  :disable-syntax t
  (fn Emil:TypedForm:map (self fn)
    `(while ,(funcall fn (Struct:get self :condition fn))
       ,@(-map fn (Struct:get self :body fn)))))

(provide 'Emil/TypedForm)
