;; -*- lexical-binding: t -*-

(require 'Transformer)
(require 'Emil/Type)
(require 'Emil/Context)

(Struct:define Emil:TypedForm
  (form
   "The encapsulated form.")
  (type "The inferred type of this form."
        :type (Trait Emil:Type))
  (context "The context pertaining to this form."
           :type (Trait Emil:Context)))

(Trait:implement Transformer:Form Emil:TypedForm
  (fn Transformer:Form:value (self)
    (Transformer:Form:value (Struct:get self :form)))
  
  (fn Transformer:Form:source (self)
    (Transformer:Form:source (Struct:get self :form))))

(Trait:implement Emil:Env Emil:TypedForm
  (fn Emil:Env:lookup-variable (self variable &optional _context)
    (Emil:Env:lookup-variable
     (Struct:get self :context) variable))

  (fn Emil:Env:lookup-function (self function &optional _context)
    (Emil:Env:lookup-function
     (Struct:get self :context) function)))

(provide 'Emil/TypedForm)
