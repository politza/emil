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

(require 'Commons)
(require 'dash)
(require 'Struct)
(require 'Trait)
(require 'Emil/Type)
(require 'Emil/Context)

(Trait:define Emil:Env ()
  :disable-syntax t
  (fn Emil:Env:lookup-variable (self (variable symbol)
                                     &optional (locals (Trait Emil:Env)))
    "Looks up VARIABLE in this environment and returns its type.

Optional LOCALS contains bindings pertaining to the current
type-inference state.

Returns `nil', if VARIABLE is not bound in this environment.")

  (fn Emil:Env:lookup-function (self (function symbol)
                                     &optional (locals (Trait Emil:Env)))
    "Looks up FUNCTION in this environment and returns its type.

See `Emil:Env:lookup-variable' for the LOCALS argument.

Returns `nil', if FUNCTION is not bound in this environment.")

  (fn Emil:Env:macro-environment (self &optional (locals (Trait Emil:Env)))
    "Returns the environment for use in `macroexpand'.

The returned value should be an association-list of macro-names and
their implementing functions."
    (ignore self locals)
    nil))

(Struct:define Emil:Env:Alist
  "Defines an environment represented via association lists."
  (variables
   "An association list mapping variable names to their `Emil:Type'."
   :type list :mutable t)
  (functions
   "An association list mapping function names to their `Emil:Type:Arrow'."
   :type list :mutable t)
  (macros
   "An association list mapping macro names to their definitions."
   :type list :default nil)
  (parent
   "An optional parent environment.

It is consulted, if a variable or function is not present in this
environment."
   :type (or null (Trait Emil:Env))))

(Trait:implement Emil:Env Emil:Env:Alist
  :disable-syntax t
  (fn Emil:Env:lookup-variable (self variable &optional locals)
    (or (cdr (assq variable (Struct:get self :variables)))
        (and (Struct:get self :parent)
             (Emil:Env:lookup-variable (Struct:get self :parent)
                                       variable locals))))

  (fn Emil:Env:lookup-function (self function &optional locals)
    (or (cdr (assq function (Struct:get self :functions)))
        (and (Struct:get self :parent)
             (Emil:Env:lookup-function (Struct:get self :parent)
                                       function locals))))

  (fn Emil:Env:macro-environment (self &optional locals)
    (append (Struct:get self :macros)
            (when-let (parent (Struct:get self :parent))
              (Emil:Env:macro-environment parent locals)))))

(defun Emil:Env:Alist:read (variables functions &optional parent)
  "Reads an environment from VARIABLES and FUNCTIONS.

Both arguments should be association lists, mapping symbols to the
printed representation of types. PARENT is an optional parent
environment, which is consulted, if a lookup in this environment
fails.

Returns a struct `Emil:Env:Alist' containing the read representation
of these lists as per `Emil:Type:read'.

Signals an error, if types could not be read; or an element of
FUNCTIONS does not contain a function-type; or the lists are otherwise
malformed."
  (unless (and (listp variables)
               (listp functions)
               (--every? (and (symbolp (car it))
                              (not (Commons:constant-symbol? (car it))))
                         (append variables functions)))
    (error "Arguments should map names to types: %s"
           (append variables functions)))

  (Emil:Env:Alist
   :variables (--map (cons (car it) (Emil:Type:read (cdr it)))
                     variables)
   :functions (--map (cons (car it) (Emil:Type:read-function (cdr it)))
                     functions)
   :parent parent))

(Struct:define Emil:Env:Hierarchy
  "Defines a hierarchy of environments via an ordered list.

Lookups start with the first environment and proceeds with the other
ones until some environment returns a non-`nil' value."

  (environments
   "The list of environments representing this hierarchy."
   :type list))

(Trait:implement Emil:Env Emil:Env:Hierarchy
  :disable-syntax t
  (fn Emil:Env:lookup-variable (self variable &optional locals)
    (--some (Emil:Env:lookup-variable it variable locals)
            (Struct:get self :environments)))

  (fn Emil:Env:lookup-function (self function &optional locals)
    (--some (Emil:Env:lookup-function it function locals)
            (Struct:get self :environments)))

  (fn Emil:Env:macro-environment (self &optional locals)
    (--mapcat (Emil:Env:macro-environment it locals)
              (Struct:get self :environments))))

(defun Emil:Env:empty ()
  "Returns an empty environment."
  (Emil:Env:Alist))

(Trait:implement Emil:Env Emil:Context
  :disable-syntax t
  (fn Emil:Env:lookup-variable (self variable &optional _locals)
    "Looks up VARIABLE in the current, local environment.

Argument LOCALS is ignored.

Returns `nil', if VARIABLE is not present in this environment."
    (-some->> (Emil:Context:lookup-variable self variable)
      (Emil:Context:resolve self)))

  (fn Emil:Env:lookup-function (_self _function &optional _locals)
    "Looks up FUNCTION in the current, local environment.

Argument LOCALS is ignored.

Returns `nil', if FUNCTION is not present in this environment."
    nil))

(defconst Emil:Env:function-type 'Emil:Env:function-type)

(defconst Emil:Env:variable-type 'Emil:Env:variable-type)

(Struct:define Emil:Env:Global)

(Trait:implement Emil:Env Emil:Env:Global
  :disable-syntax t
  (fn Emil:Env:lookup-variable (_self variable &optional _locals)
    (when-let (type (get variable Emil:Env:variable-type))
      (if (Trait:implements? (Trait:type-of type) 'Emil:Type)
          type
        (Emil:Type:read type))))

  (fn Emil:Env:lookup-function (_self function &optional _locals)
    (when-let (type (get function Emil:Env:function-type))
      (if (Trait:implements? (Trait:type-of type) 'Emil:Type)
          type
        (Emil:Type:read-function type)))))

(defun Emil:Env:declare-function (symbol type)
  (put symbol Emil:Env:function-type
       (if type (Emil:Type:read-function type))))

(defun Emil:Env:declare-variable (symbol type)
  (put symbol Emil:Env:variable-type
       (if type (Emil:Type:read type))))

(defun Emil:Env:declare-alias (symbol definition)
  (put symbol Emil:Env:function-type
       (and definition
            (or (get definition Emil:Env:function-type)
                (Emil:error "Type of definition `%s' for alias `%s' is not declared" definition symbol)))))

(defmacro Emil:Env:declare-functions (&rest declarations)
  "Declare function types given via DECLARATIONS.

DECLARATIONS should be a list of pairs \(FUNCTION . TYPE\), where
FUNCTION is a symbol naming the function and TYPE its type in a form
readable by `Emil:Type:read-function'."
  (declare (indent 0))
  `(progn
     ,@(-map (-lambda ((symbol . type))
               `(Emil:Env:declare-function ',symbol ',type))
             declarations)))

(defmacro Emil:Env:declare-variables (&rest declarations)
  "Declare variable types given via DECLARATIONS.

DECLARATIONS should be a list of pairs \(VARIABLE . TYPE\), where
VARIABLE is a symbol naming the variable and TYPE its type in a form
readable by `Emil:Type:read'."
  (declare (indent 0))
  `(progn
     ,@(-map (-lambda ((symbol . type))
               `(Emil:Env:declare-variable ',symbol ',type))
             declarations)))

(defmacro Emil:Env:declare-aliases (&rest declarations)
  "Declare types of function aliases via DECLARATIONS."
  (declare (indent 0))
  `(progn
     ,@(-map (-lambda ((symbol . definition))
               `(Emil:Env:declare-alias ',symbol ',definition))
             declarations)))

(Struct:define Emil:Env:Fallback)

(Trait:implement Emil:Env Emil:Env:Fallback
  :disable-syntax t
  (fn Emil:Env:lookup-variable (_self _variable &optional _locals)
    (Emil:Type:Any))

  (fn Emil:Env:lookup-function (_self _function &optional _locals)
    (Emil:Type:read-function '(-> (&rest Any) Any))))

(provide 'Emil/Env)
