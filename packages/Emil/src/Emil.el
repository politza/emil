;;; Emil.el --- Emacs mit Inferenz Logik.             -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Andreas Politz

;; Author: Andreas Politz <mail@andreas-politz.de>
;; Keywords: extensions
;; Version: 1.0.0beta1
;; Package-Requires: ((emacs "29.1") (dash "2.19.1") (Struct "1.0.0beta1") (Commons "1.0.0beta1") (Transformer "1.0.0beta1"))

(require 'Emil/Type)

(require 'dash)
(require 'pcase)
(require 'Struct)
(require 'Trait)
(require 'Transformer)
(require 'Emil/Type)
(require 'Emil/Context)

(eval-when-compile
  (pcase-defmacro list (&rest elements)
    (cons '\` (list (--map (list '\, it) elements))))

  (pcase-defmacro cons (head tail)
    (cons '\` (list (cons (list '\, head) (list '\, tail))))))

(defvar Emil:environment nil)

(defvar Emil:prepared-environment nil)

(Struct:define Emil:State
  (infer-transformer
   :type Emil:InferTransformer)
  (check-transformer
   :type Emil:CheckTransformer)
  (generator
   :type Emil:ExistentialsGenerator :default (Emil:ExistentialsGenerator))
  (messages :type list))

(Struct:define Emil:Message
  (type
   "The type of this message. Either one of `:info', `:warning' or `:error'"
   :type (member :info :warning :error))
  (content
   "The content of the message."
   :type string)
  (form
   "An optional form associated with this message."))

(Struct:define Emil:InferTransformer)

(Struct:define Emil:CheckTransformer)

(Trait:implement Transformer Emil:InferTransformer)

(Trait:implement Transformer Emil:CheckTransformer)

(Struct:define Emil:ExistentialsGenerator
  "Generator for instances of type `Emil:Type:Existential'."
  (counter :type number :default -1 :mutable t))

(defun Emil:ExistentialsGenerator:next (self)
  (Emil:Type:Existential
   :name (->> (Struct:update self :counter #'1+)
              (format "t%d")
              (intern))))

(Struct:implement Emil:State
  (fn Emil:infer (self form)
    (Transformer:transform-form
     (Struct:get self :infer-transformer)
     form
     self))

  (fn Emil:check (self form type)
    (Transformer:transform-form
     (Struct:get self :check-transformer)
     form
     type
     self))

  (fn Emil:generate-existential (self)
    "Returns a new `Emil:Type:Existential'."
    (Emil:ExistentialsGenerator:next (Struct:get self :generator)))

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
        top (Emil:Context:SolvedVarInst* variable type) bottom))
      ;; InstLReach
      ((and (Struct Emil:Type:Existential)
            (let `(,top ,center ,bottom)
              (Emil:Context:double-hole context type variable)))
       (Emil:Context:concat top (Emil:Context:SolvedVarInst
                                 :variable type :type variable)
                            center variable bottom))
      ;; InstLArr
      ((and (Struct Emil:Type:Arrow)
            (let `(,top ,bottom)
              (Emil:Context:hole context variable)))
       (let* ((instance (Emil:instantiate-arrow
                         type (Struct:get self :generator)))
              (solved-var-inst
               (Emil:Context:SolvedVarInst* variable :type instance))
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
        top (Emil:Context:SolvedVarInst* variable type) bottom))
      ;; InstRReach
      ((and (Struct Emil:Type:Existential)
            (let `(,top ,center ,bottom)
              (Emil:Context:double-hole context type variable)))
       (Emil:Context:concat top (Emil:Context:SolvedVarInst
                                 :variable type :type variable)
                            center variable bottom))
      ;; InstRArr
      ((and (Struct Emil:Type:Arrow)
            (let `(,top ,bottom)
              (Emil:Context:hole context variable)))
       (let* ((instance (Emil:instantiate-arrow
                         type (Struct:get self :generator)))
              (solved-var-inst
               (Emil:Context:SolvedVarInst* variable :type instance))
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
    (pcase-exhaustive (cons left right)
      ;; Var / Exvar
      ((and (or (cons (Struct Emil:Type:Variable)
                      (Struct Emil:Type:Variable))
                (cons (Struct Emil:Type:Existential)
                      (Struct Emil:Type:Existential)))
            (guard (equal left right)))
       nil)
      ;; Arrow
      ((cons (Struct Emil:Type:Arrow
                     :arguments left-arguments :returns left-returns)
             (Struct Emil:Type:Arrow
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
      ((cons (Struct Emil:Type:Forall parameters type) _)
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
      ((cons _ (Struct Emil:Type:Forall parameters type))
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
      ((and (cons (Struct Emil:Type:Existential name) _)
            (guard (not (member name (Emil:Type:free-variables right)))))
       (Emil:instantiate-left self context left right))
      ;; Instantiate R
      ((and (cons _ (Struct Emil:Type:Existential name))
            (guard (not (member name (Emil:Type:free-variables left)))))
       (Emil:instantiate-right self context left right))
      ;; Unit
      ((or (cons (Struct Emil:Type:Never) _)
           (cons _ (Struct Emil:Type:Any))
           (and (cons (Struct Emil:Type:Null) _)
                (guard (not (Emil:Type:Never? right))))
           (and (cons (Struct Emil:Type:Void) _)
                (guard (not (Emil:Type:Never? right))))
           (and (cons (Struct Emil:Type:Basic :name left-name)
                      (Struct Emil:Type:Basic :name right-name))
                (guard (pcase (cons left-name right-name)
                         ((cons 'integer 'number) t)
                         ((cons 'float 'number) t)
                         ((and _ (guard (equal left-name right-name)))
                          t)))))
       nil))))

(provide 'Emil)
;;; Emil.el ends here
