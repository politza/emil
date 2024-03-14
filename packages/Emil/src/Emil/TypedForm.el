;; -*- lexical-binding: t -*-

(require 'Transformer)
(require 'Emil/Type)
(require 'Emil/Context)

(Struct:define Emil:TypedForm
  (value
   "The value of this form.")
  (type
   "The type of this form."
   :type (Trait Emil:Type))
  (environment
   "The environment pertaining to this form."
   :type (Trait Emil:Env)))

(Trait:implement Transformer:Form Emil:TypedForm
  (fn Transformer:Form:value (self)
    (Struct:get self :value)))

(Trait:implement Emil:Env Emil:TypedForm
  (fn Emil:Env:lookup-variable (self variable &optional context)
    (Emil:Env:lookup-variable (Struct:get self :environment) variable context))

  (fn Emil:Env:lookup-function (self function &optional context)
    (Emil:Env:lookup-function (Struct:get self :environment) function context)))

(provide 'Emil/TypedForm)
