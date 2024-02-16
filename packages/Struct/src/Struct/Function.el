;; -*- lexical-binding: t -*-

(require 'Struct)

(Struct:define Struct:Function
  "Defines a function for use with structs and traits."
  (name
   "The name of the function."
   :type symbol)
  (qualified-name
   "The qualified name of the function.

This is the name under which the function is exported into the global
namespace."
   :type symbol)
  (arguments 
   "The list of arguments declared by this function."
   :type list)
  (return-type
   "The type this function returns."
   :type nil)
  (documentation
   "The documentation for this function."
   :type (or null string))
  (definition
   "The definition of this function.

If nil, this function is abstract."
   :type (or null function)))

(provide 'Struct/Function)
