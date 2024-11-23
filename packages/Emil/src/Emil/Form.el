;; -*- lexical-binding: t -*-

;; Copyright (C) 2024  Andreas Politz

;; Author: Andreas Politz

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

(require 'Trait)
(require 'Struct)
(require 'Emil/Type)
(require 'Emil/Context)
(require 'Emil/Env)

(Trait:define Emil:Form ()
  :disable-syntax t
  (fn Emil:Form:position (self -> integer)
    nil)

  (fn Emil:Form:each-child (self fn &optional env)))

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
  :disable-syntax t
  (fn Emil:Form:position (self)
    (let ((form (Struct:get self :form)))
      (when (symbol-with-pos-p form)
        (symbol-with-pos-pos form))))

  (fn Emil:Form:each-child (self _fn &optional _env) nil))

(Struct:define Emil:Form:Atom
  (value)
  (type :type (Trait Emil:Type)))

(Trait:implement Emil:Form Emil:Form:Atom
  :disable-syntax t
  (fn Emil:Form:position (self)
    (let ((value (Struct:get self :value)))
      (when (symbol-with-pos-p value)
        (symbol-with-pos-pos value))))

  (fn Emil:Form:each-child (self _fn &optional _env) nil))

(Struct:define Emil:Form:Application
  (function :type Emil:Form:Function)
  (arguments :type (List (Trait Emil:Form)))
  (type :type (Trait Emil:Type)))

(Trait:implement Emil:Form Emil:Form:Application
  :disable-syntax t
  (fn Emil:Form:each-child (self fn &optional env)
    (or (funcall fn (Struct:get self :function) env)
        (--some (funcall fn it env)
                (Struct:get self :arguments)))))

(Struct:define Emil:Form:And
  (conditions :type (List (Trait Emil:Form)))
  (type :type (Trait Emil:Type)))

(Trait:implement Emil:Form Emil:Form:And
  :disable-syntax t
  (fn Emil:Form:each-child (self fn &optional env)
    (--some (funcall fn it env)
            (Struct:get self :conditions))))

(Struct:define Emil:Form:Catch
  (tag :type (Trait Emil:Form))
  (body :type (List (Trait Emil:Form)))
  (type :type (Trait Emil:Type)))

(Trait:implement Emil:Form Emil:Form:Catch
  :disable-syntax t
  (fn Emil:Form:each-child (self fn &optional env)
    (or (funcall fn (Struct:get self :tag) env)
        (--some (funcall fn it env)
                (Struct:get self :body)))))

(Struct:define Emil:Form:Cond
  (clauses :type (List (List (Trait Emil:Form))))
  (type :type (Trait Emil:Type)))

(Trait:implement Emil:Form Emil:Form:Cond
  :disable-syntax t
  (fn Emil:Form:each-child (self fn &optional env)
    (--some (--some (funcall fn it env) it)
            (Struct:get self :clauses))))

(Struct:define Emil:Form:ConditionCase
  (variable :type symbol)
  (body-form :type (Trait Emil:Form))
  (handlers :type (List Emil:Form:ConditionCaseHandler))
  (type :type (Trait Emil:Type)))

(Trait:implement Emil:Form Emil:Form:ConditionCase
  :disable-syntax t
  (fn Emil:Form:each-child (self fn &optional env)
    (or (funcall fn (Struct:get self :body-form) env)
        (let ((env (if-let (variable (Struct:get self :variable))
                       (cons (cons variable (Emil:Type:Any))
                             env)
                     env)))
          (--some (--some (funcall fn it env)
                          (Struct:get it :body))
                  (Struct:get self :handlers))))))

(Struct:define Emil:Form:ConditionCaseHandler
  (condition :type (or symbol (List symbol)))
  (body :type (List (Trait Emil:Form))))

(Struct:define Emil:Form:DefConst
  (symbol :type symbol)
  (init-value :type (Trait Emil:Form))
  (documentation :type (or null string))
  (type :type (Trait Emil:Type)))

(Trait:implement Emil:Form Emil:Form:DefConst
  :disable-syntax t
  (fn Emil:Form:each-child (self fn &optional env)
    (funcall fn (Struct:get self :init-value) env)))

(Struct:define Emil:Form:DefVar
  (symbol :type symbol)
  (init-value :type (Trait Emil:Form))
  (documentation :type (or null string))
  (type :type (Trait Emil:Type)))

(Trait:implement Emil:Form Emil:Form:DefVar
  :disable-syntax t
  (fn Emil:Form:each-child (self fn &optional env)
    (funcall fn (Struct:get self :init-value) env)))

(Struct:define Emil:Form:Function
  (value :type (or Emil:Form:Atom Emil:Form:Lambda))
  (type :type (Trait Emil:Type)))

(Trait:implement Emil:Form Emil:Form:Function
  :disable-syntax t
  (fn Emil:Form:each-child (self fn &optional env)
    (funcall fn (Struct:get self :value) env)))

(Struct:define Emil:Form:Lambda
  (arguments :type list)
  (body :type (List (Trait Emil:Form)))
  (type :type (Trait Emil:Type)))

(Trait:implement Emil:Form Emil:Form:Lambda
  :disable-syntax t
  (fn Emil:Form:each-child (self fn &optional env)
    (let* ((arguments (Emil:Util:lambda-variables (Struct:get self :arguments)))
           (types (if (Emil:Type:Arrow? (Struct:get self :type))
                      (Emil:Type:Arrow:arguments (Struct:get self :type))
                    (-repeat (length arguments) (Emil:Type:Any))))
           (env (append (-zip-pair arguments types) env)))
      (--some (funcall fn it env)
              (Struct:get self :body)))))

(Struct:define Emil:Form:If
  (condition :type (Trait Emil:Form))
  (then :type (Trait Emil:Form))
  (else :type (List (Trait Emil:Form)))
  (type :type (Trait Emil:Type)))

(Trait:implement Emil:Form Emil:Form:If
  :disable-syntax t
  (fn Emil:Form:each-child (self fn &optional env)
    (or (funcall fn (Struct:get self :condition) env)
        (funcall fn (Struct:get self :then) env)
        (--some (funcall fn it env)
                (Struct:get self :else)))))

(Struct:define Emil:Form:Interactive
  (forms)
  (type :type (Trait Emil:Type)))

(Trait:implement Emil:Form Emil:Form:Interactive
  :disable-syntax t
  (fn Emil:Form:each-child (self _fn &optional _env) nil))

(Struct:define Emil:Form:Let
  (kind :type (member let let*))
  (bindings :type (List Emil:Form:Binding))
  (body :type (List (Trait Emil:Form)))
  (type :type (Trait Emil:Type)))

(Trait:implement Emil:Form Emil:Form:Let
  :disable-syntax t
  (fn Emil:Form:each-child (self fn &optional env)
    (let ((local-env (--map (cons (Struct:get it :name)
                                  (Struct:get (Struct:get it :value) :type))
                            (Struct:get self :bindings)))
          (sequential? (eq 'let* (Struct:get self :kind)))
          (index -1))
      (or (--some
           (funcall fn (Struct:get it :value)
                    (append (when sequential?
                              (reverse (-take (cl-incf index) local-env)))
                            env))
           (Struct:get self :bindings))
          (--some (funcall fn it (append (reverse local-env) env))
                  (Struct:get self :body))))))

(Struct:define Emil:Form:Binding
  (name :type symbol)
  (value :type (Trait Emil:Form)))

(Struct:define Emil:Form:Or
  (conditions :type (List (Trait Emil:Form)))
  (type :type (Trait Emil:Type)))

(Trait:implement Emil:Form Emil:Form:Or
  :disable-syntax t
  (fn Emil:Form:each-child (self fn &optional env)
    (--some (funcall fn it env)
            (Struct:get self :conditions))))

(Struct:define Emil:Form:Prog1
  (first :type (Trait Emil:Form))
  (body :type (List (Trait Emil:Form)))
  (type :type (Trait Emil:Type)))

(Trait:implement Emil:Form Emil:Form:Prog1
  :disable-syntax t
  (fn Emil:Form:each-child (self fn &optional env)
    (or (funcall fn (Struct:get self :first) env)
        (--some (funcall fn it env)
                (Struct:get self :body)))))

(Struct:define Emil:Form:PrognLike
  (kind :type (member progn save-current-buffer save-excursion save-restriction))
  (body :type (List (Trait Emil:Form)))
  (type :type (Trait Emil:Type)))

(Trait:implement Emil:Form Emil:Form:PrognLike
  :disable-syntax t
  (fn Emil:Form:each-child (self fn &optional env)
    (--some (funcall fn it env)
            (Struct:get self :body))))

(Struct:define Emil:Form:Quote
  (value)
  (type :type (Trait Emil:Type)))

(Trait:implement Emil:Form Emil:Form:Quote
  :disable-syntax t
  (fn Emil:Form:each-child (self _fn &optional _env)
    nil))

(Struct:define Emil:Form:Setq
  (bindings :type (List Emil:Form:Binding))
  (type :type (Trait Emil:Type)))

(Trait:implement Emil:Form Emil:Form:Setq
  :disable-syntax t
  (fn Emil:Form:each-child (self fn &optional env)
    (--some (funcall fn (Struct:get it :value) env)
            (Struct:get self :bindings))))

(Struct:define Emil:Form:UnwindProtect
  (body-form :type (Trait Emil:Form))
  (unwind-forms :type (List (Trait Emil:Form)))
  (type :type (Trait Emil:Type)))

(Trait:implement Emil:Form Emil:Form:UnwindProtect
  :disable-syntax t
  (fn Emil:Form:each-child (self fn &optional env)
    (or (funcall fn (Struct:get self :body-form) env)
        (--some (funcall fn it env)
                (Struct:get self :unwind-forms)))))

(Struct:define Emil:Form:While
  (condition :type (Trait Emil:Form))
  (body :type (List (Trait Emil:Form)))
  (type :type (Trait Emil:Type)))

(Trait:implement Emil:Form Emil:Form:While
  :disable-syntax t
  (fn Emil:Form:each-child (self fn &optional env)
    (or (funcall fn (Struct:get self :condition) env)
        (--some (funcall fn it env)
                (Struct:get self :body)))))

(provide 'Emil/Form)
