;;; Emil.el --- Emacs mit Inferenz Logik.             -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Andreas Politz

;; Author: Andreas Politz <mail@andreas-politz.de>
;; Keywords: extensions
;; Version: 1.0.0beta1
;; Package-Requires: ((emacs "29.1") (dash "2.19.1") (Struct "1.0.0beta1") (Commons "1.0.0beta1") (Transformer "1.0.0beta1"))

(require 'Emil/Type)

(require 'dash)
(require 'Struct)
(require 'Trait)
(require 'Transformer)
(require 'Emil/Type)
(require 'Emil/Context)

(defvar Emil:environment nil)

(defvar Emil:prepared-environment nil)

(Struct:define Emil:State
  (infer-transformer
   :type Emil:InferTransformer)
  (check-transformer
   :type Emil:CheckTransformer)
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

  (fn Emil:generate-var-inst (self))

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
       (let* ((instance (Emil:Type:instantiate type))
              (solved-var-inst
               (Emil:Context:SolvedVarInst* variable :type instance))
              (initial-context
               (Emil:Context:concat
                top solved-var-inst
                (Emil:Type:parameters instance) bottom))
              (result-context
               (--reduce-from
                (Emil:instantiate-right (car it) (cdr it) acc)
                initial-context
                (butlast (-zip-pair (Emil:Type:parameters type)
                                    (Emil:Type:parameters instance))))))
         (Emil:instantiate-left
          (Struct:get instance :result-type)
          (Emil:Context:resolve result-context (Struct:get type :result-type))
          result-context)))
      ;; InstLAllR
      ((and (Struct Emil:Type:Forall)
            (guard (Emil:Context:member? context variable)))
       (let ((parameters (Emil:Type:parameters type)))
         (Emil:Context:drop-until-after
          (Emil:instantiate-left
           (Emil:Context:concat
            (Emil:Context :entries (cdr (reverse parameters)))
            context)
           variable (-last parameters))
          (car parameters)))))))



(provide 'Emil)
;;; Emil.el ends here
