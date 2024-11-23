;; -*- lexical-binding: t -*-

(require 'dash)
(require 'Struct)
(require 'Trait)
(require 'Emil/Env)
(require 'Emil/Type)
(require 'Emil/Form)
(require 'cl-lib)

(declare-function Emil:transform* "Emil" (form &optional environment))

(Struct:define Emil:Syntax:Env
  environment
  (emitted-types :default (make-hash-table :test 'eq)))

(Struct:implement Emil:Syntax:Env
  :disable-syntax t
  (fn Emil:Syntax:transform-function (self function)
    (let* ((bindings (Emil:Syntax:Function:bindings function))
           (env (Emil:Env:Hierarchy
                 :environments
                 (list (Emil:Env:Alist :variables bindings) self))))
      (Emil:Form:map
       (Emil:transform* `(progn ,@(Struct:get function :body)) env)
       (-partial #'Emil:Syntax:transform-form self))))

  (fn Emil:Syntax:transform-form (self form)
    (pcase form
      ((Struct Emil:Form:Atom value type)
       (if (and (symbolp value)
                (gethash type (Struct:get self :emitted-types)))
           value
         value))
      ((Struct Emil:Form:Function value type)
       value)
      ((Struct Emil:Form:Application function arguments type)
       (if (and (symbolp (Struct:get function :value))
                (gethash type (Struct:get self :emitted-types)))
           value
         value))
      (_ (Emil:Form:map form (-partial #'Emil:Syntax:transform self)))))

  (fn Emil:Syntax:transform-variable (self symbol context)
    )

  (fn Emil:Syntax:-resolve-type (self symbol context kind)
    (when-let* ((components (-map #'intern (-some-> (symbol-name symbol)
                                             (string-match-p "[.]")
                                             (split-string  "[.]"))))
                (type (Emil:Env:lookup-function
                       (Struct:get self :environment)
                       )))
      ))
  )

(Trait:implement Emil:Env Emil:Syntax:Env
  :disable-syntax t
  (fn Emil:Env:lookup-variable (self (variable symbol)
                                     &optional (context Emil:Context))
    )

  (fn Emil:Env:lookup-function (self (function symbol)
                                     &optional (context Emil:Context))
    ))

(defun Emil:Syntax:create-transformer ()
  (let ((env (Emil:Syntax:Env :environment (Emil:Env:Global))))
    (lambda (function)
      (Emil:Syntax:transform-function env function))))

(defun Emil:Syntax:Function:type (fn)
  (Emil:Type:read-function (Struct:Function:type fn)))

(defun Emil:Syntax:Function:bindings (fn)
  (let ((names (--map (Struct:get it :name) (Struct:get fn :arguments)))
        (types (Emil:Type:Arrow:arguments
                (Emil:Type:read-function (Struct:Function:type fn)))))
    (-zip-pair names types)))

(provide 'Emil/Syntax)
