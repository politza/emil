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
   :type Emil:Type:Generator :default (Emil:Type:Generator))
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

  (fn Emil:generate-variable (self)
    (Emil:Type:Generator:next (Struct:get self :generator)))

  (fn Emil:instantiate-left (self (context Emil:Context)
                                  (variable Emil:Type:VarInst)
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
      ((and (Struct Emil:Type:VarInst)
            (let `(,top ,center ,bottom)
              (Emil:Context:double-hole context type variable)))
       (Emil:Context:concat top (Emil:Context:SolvedVarInst
                                 :variable type :type variable)
                            center variable right))
      ;; InstLArr
      ((and (Struct Emil:Type:Fn)
            (let `(,top ,bottom)
              (Emil:Context:hole context variable)))
       (let* ((instance (Emil:Type:Fn:instantiate
                         type (Struct:get self :generator)))
              (solved-var-inst
               (Emil:Context:SolvedVarInst* variable :type instance))
              (initial-context
               (Emil:Context:concat
                top solved-var-inst
                (Emil:Type:Fn:returns instance)
                (reverse (Emil:Type:Fn:arguments instance))
                bottom))
              (intermediate-context
               (--reduce-from
                (Emil:instantiate-right (car it) (cdr it) acc)
                initial-context
                (-zip-pair (Emil:Type:Fn:arguments type)
                           (Emil:Type:Fn:arguments instance)))))
         (Emil:instantiate-left
          (Struct:get instance :returns)
          (Emil:Context:resolve result-context (Struct:get type :returns))
          intermediate-context)))
      ;; InstLAllR
      ((and (Struct Emil:Type:Forall parameters type)
            (guard (Emil:Context:member? context variable)))
       (let* ((marker (Emil:Context:Marker))
              (initial-context (Emil:Context:concat
                                (Emil:Context :entries (reverse parameters))
                                marker context))
              (intermediate-context (Emil:instantiate-left
                                     initial-context variable type)))
         (Emil:Context:drop-until-after initial-context marker)))))

  (fn Emil:instantiate-right (self (context Emil:Context)
                                   (type (Trait Emil:Type))
                                   (variable Emil:Type:VarInst))
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
      ((and (Struct Emil:Type:VarInst)
            (let `(,top ,center ,bottom)
              (Emil:Context:double-hole context type variable)))
       (Emil:Context:concat top (Emil:Context:SolvedVarInst
                                 :variable type :type variable)
                            center variable right))
      ;; InstRArr
      ((and (Struct Emil:Type:Fn)
            (let `(,top ,bottom)
              (Emil:Context:hole context variable)))
       (let* ((instance (Emil:Type:instantiate
                         type (Struct:get self :generator)))
              (solved-var-inst
               (Emil:Context:SolvedVarInst* variable :type instance))
              (initial-context
               (Emil:Context:concat
                top solved-var-inst
                (Emil:Type:Fn:returns instance)
                (reverse (Emil:Type:Fn:arguments instance))
                bottom))
              (intermediate-context
               (--reduce-from
                (Emil:instantiate-left (car it) (cdr it) acc)
                initial-context
                (-zip-pair (Emil:Type:Fn:arguments instance)
                           (Emil:Type:Fn:arguments type)))))
         (Emil:instantiate-right
          (Emil:Context:resolve
           intermediate-context (Struct:get type :returns))
          (Struct:get instance :returns)
          intermediate-context)))
      ;; InstRAllR
      ((and (Struct Emil:Type:Forall)
            (guard (Emil:Context:member? context variable)))
       (let ((parameters (Emil:Type:Forall:parameters type))
             (instances (-map (lambda (_)
                                (Emil:generate-variable self))
                               parameters))
             (marker (Emil:Context:Marker))
             (initial-context
              (Emil:Context:concat
               (Emil:Context :entries (reverse instances)) marker context))
             (intermediate-context
              (Emil:instantiate-right
               initial-context
               (--zip-with
                (Emil:Type:substitute acc (car it) (cdr it))
                (Emil:Type:Forall:type type)
                (-zip-pair parameters instances))
               variable)))
         (Emil:Context:drop-until-after intermediate-context marker)))))

  (fn Emil:subtype (self (context Emil:Context)
                         (left (Trait Emil:Type))
                         (right (Trait Emil:Type)))
    (pcase-exhaustive (cons left right)
      ((and (or (cons (Struct Emil:Type:Var)
                      (Struct Emil:Type:Var))
                (cons (Struct Emil:Type:VarInst)
                      (Struct Emil:Type:VarInst)))
            (guard (equal left right)))
       nil)
      ((cons (Struct Emil:Type:Fn
                     :arguments left-arguments :returns left-returns)
             (Struct Emil:Type:Fn
                     :arguments right-arguments :returns right-returns))
       (-let (((left-min . left-max) (Emil:Type:Fn:arity left))
              ((right-min . right-max) (Emil:Type:Fn:arity right)))
         (unless (and (<= left-min right-min)
                      (>= left-max right-max))
           (error "Function is not arity-compatible"))
         ;; FIXME: This does not work with &rest arguments.
         (Emil:subtype
          self (--reduce-from
                (Emil:subtype self acc (car it) (cdr it))
                context
                (-zip-pair right-arguments left-arguments))
          left-returns right-returns))))))

(provide 'Emil)
;;; Emil.el ends here
