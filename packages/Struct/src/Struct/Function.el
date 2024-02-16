;; -*- lexical-binding: t -*-

(require 'Struct)

(Struct:define Struct:Function
  "Defines a declared function."
  (name
   "The name of the function."
   :type symbol)
  (qualified-name
   "The qualified name of the function.

This is the name under which the function is exported into the global
namespace."
   :type symbol)
  (arguments 
   "The list of declared arguments of this function."
   :type list)
  (return-type
   "The return-type of this function."
   :type nil)
  (documentation
   "The documentation for this function."
   :type (or null string))
  (body-forms
   "The forms defining this function."
   :type list))

(provide 'Struct/Function)
