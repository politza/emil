;; -*- lexical-binding: t -*-

(require 'Trait)
(require 'Struct)
(require 'Emil/Type)
(require 'Emil/Context)
(require 'Emil/Env)

(Trait:define Emil:Form ()
  :disable-syntax t)

(defun Emil:Form:with-type (form type)
  (cl-check-type form (Trait Emil:Form))
  (cl-check-type type (Trait Emil:Type))
  (let ((clone (copy-sequence form)))
    (Struct:unsafe-set clone :type type)
    clone))

(Struct:define Emil:Form:Invalid
  (form)
  (type :type (Trait Emil:Type)))

(Trait:implement Emil:Form Emil:Form:Invalid
  :disable-syntax t)

(Struct:define Emil:Form:Atom
  (value)
  (type :type (Trait Emil:Type)))

(Trait:implement Emil:Form Emil:Form:Atom
  :disable-syntax t)

(Struct:define Emil:Form:Application
  (function :type Emil:Form:ApplicationFn)
  (arguments :type (List (Trait Emil:Form)))
  (type :type (Trait Emil:Type)))

(Struct:define Emil:Form:ApplicationFn
  (value :type (or symbol Emil:Form:Lambda))
  (type :type (Trait Emil:Type)))

(Trait:implement Emil:Form Emil:Form:Application
  :disable-syntax t)

(Struct:define Emil:Form:And
  (conditions :type (List (Trait Emil:Form)))
  (type :type (Trait Emil:Type)))

(Trait:implement Emil:Form Emil:Form:And
  :disable-syntax t)

(Struct:define Emil:Form:Catch
  (tag :type (Trait Emil:Form))
  (body :type (List (Trait Emil:Form)))
  (type :type (Trait Emil:Type)))

(Trait:implement Emil:Form Emil:Form:Catch
  :disable-syntax t)

(Struct:define Emil:Form:Cond
  (clauses :type (List Emil:Form:Clause))
  (type :type (Trait Emil:Type)))

(Trait:implement Emil:Form Emil:Form:Cond
  :disable-syntax t)

(Struct:define Emil:Form:Clause
  (condition :type (Trait Emil:Form))
  (body :type (List (Trait Emil:Form))))

(Struct:define Emil:Form:DefConst
  (symbol :type symbol)
  (init-value :type (Trait Emil:Form))
  (documentation :type (or null string))
  (type :type (Trait Emil:Type)))

(Trait:implement Emil:Form Emil:Form:DefConst
  :disable-syntax t)

(Struct:define Emil:Form:DefVar
  (symbol :type symbol)
  (init-value :type (Trait Emil:Form))
  (documentation :type (or null string))
  (type :type (Trait Emil:Type)))

(Trait:implement Emil:Form Emil:Form:DefVar
  :disable-syntax t)

(Struct:define Emil:Form:Function
  (value :type (or symbol Emil:Form:Lambda))
  (type :type (Trait Emil:Type)))

(Trait:implement Emil:Form Emil:Form:Function
  :disable-syntax t)

(Struct:define Emil:Form:Lambda
  (arguments :type list)
  (body :type (List (Trait Emil:Form))))

(Struct:define Emil:Form:If
  (condition :type (Trait Emil:Form))
  (then :type (Trait Emil:Form))
  (else :type (List (Trait Emil:Form)))
  (type :type (Trait Emil:Type)))

(Trait:implement Emil:Form Emil:Form:If
  :disable-syntax t)

(Struct:define Emil:Form:Interactive
  (forms)
  (type :type (Trait Emil:Type)))

(Trait:implement Emil:Form Emil:Form:Interactive
  :disable-syntax t)

(Struct:define Emil:Form:Let
  (kind :type (member let let*))
  (bindings :type (List Emil:Form:Binding))
  (body :type (List (Trait Emil:Form)))
  (type :type (Trait Emil:Type)))

(Trait:implement Emil:Form Emil:Form:Let
  :disable-syntax t)

(Struct:define Emil:Form:Binding
  (name :type symbol)
  (value :type (Trait Emil:Form)))

(Struct:define Emil:Form:Or
  (conditions :type (List (Trait Emil:Form)))
  (type :type (Trait Emil:Type)))

(Trait:implement Emil:Form Emil:Form:Or
  :disable-syntax t)

(Struct:define Emil:Form:Prog1
  (first :type (Trait Emil:Form))
  (body :type (List (Trait Emil:Form)))
  (type :type (Trait Emil:Type)))

(Trait:implement Emil:Form Emil:Form:Prog1
  :disable-syntax t)

(Struct:define Emil:Form:PrognLike
  (kind :type (member progn save-current-buffer save-excursion save-restriction))
  (body :type (List (Trait Emil:Form)))
  (type :type (Trait Emil:Type)))

(Trait:implement Emil:Form Emil:Form:PrognLike
  :disable-syntax t)

(Struct:define Emil:Form:Quote
  (value)
  (type :type (Trait Emil:Type)))

(Trait:implement Emil:Form Emil:Form:Quote
  :disable-syntax t)

(Struct:define Emil:Form:Setq
  (bindings :type (List Emil:Form:Binding))
  (type :type (Trait Emil:Type)))

(Trait:implement Emil:Form Emil:Form:Setq
  :disable-syntax t)

(Struct:define Emil:Form:UnwindProtect
  (body-form :type (Trait Emil:Form))
  (unwind-forms :type (List (Trait Emil:Form)))
  (type :type (Trait Emil:Type)))

(Trait:implement Emil:Form Emil:Form:UnwindProtect
  :disable-syntax t)

(Struct:define Emil:Form:While
  (condition :type (Trait Emil:Form))
  (body :type (List (Trait Emil:Form)))
  (type :type (Trait Emil:Type)))

(Trait:implement Emil:Form Emil:Form:While
  :disable-syntax t)

(provide 'Emil/Form)
