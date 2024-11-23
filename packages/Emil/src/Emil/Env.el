;; -*- lexical-binding: t -*-

(require 'Commons)
(require 'dash)
(require 'Struct)
(require 'Trait)
(require 'Emil/Type)
(require 'Emil/Context)

(Trait:define Emil:Env ()
  (fn Emil:Env:lookup-variable (self (variable symbol)
                                     &optional (context Emil:Context))
    "Looks up VARIABLE in this environment and returns its type.

Optional CONTEXT contains information pertaining to the current
type-inference state. Since it also implements `Emil:Env', it can also
be used to lookup local variables and functions.

Returns `nil', if VARIABLE is not bound in this environment.")

  (fn Emil:Env:lookup-function (self (function symbol)
                                     &optional (context Emil:Context))
    "Looks up FUNCTION in this environment and returns its type.

See `Emil:Env:lookup-variable' for the CONTEXT argument.

Returns `nil', if FUNCTION is not bound in this environment.")

  (fn Emil:Env:macro-environment (self &optional (context Emil:Context))
    "Returns the environment for use in `macroexpand'.

Returns an association-list of macro-names and their implementing
functions."
    (ignore self context)
    nil)

  (fn Emil:Env:add-function (self (function symbol)
                                  (type (Trait Emil:Type))
                                  &optional (context Emil:Context))
    "Adds FUNCTION with given TYPE to this environment.

See `Emil:Env:lookup-variable' for the CONTEXT argument."
    (ignore self context)
    nil)

  (fn Emil:Env:add-variable (self (variable symbol)
                                  (type (Trait Emil:Type))
                                  &optional (context Emil:Context))
    "Adds VARIABLE with given TYPE to this environment.

See `Emil:Env:lookup-variable' for the CONTEXT argument."
    (ignore self context)
    nil)

  (fn Emil:Env:add-macro (self (macro symbol)
                               (definition function)
                               &optional (context Emil:Context))
    "Adds MACRO with given DEFINITION to this environment.

See `Emil:Env:lookup-variable' for the CONTEXT argument."
    (ignore self context)
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
   :type list :mutable t :default nil)
  (parent
   "An optional parent environment.

It is consulted, if a variable or function is not present in this
environment."
   :type (or null (Trait Emil:Env))))

(Trait:implement Emil:Env Emil:Env:Alist
  (fn Emil:Env:lookup-variable (self variable &optional context)
    (or (cdr (assq variable (Struct:get self :variables)))
        (and (Struct:get self :parent)
             (Emil:Env:lookup-variable (Struct:get self :parent)
                                       variable context))))

  (fn Emil:Env:add-variable (self variable type &optional _context)
    (Struct:update self :variables (-partial #'cons (cons variable type))))

  (fn Emil:Env:lookup-function (self function &optional context)
    (or (cdr (assq function (Struct:get self :functions)))
        (and (Struct:get self :parent)
             (Emil:Env:lookup-function (Struct:get self :parent)
                                       function context))))

  (fn Emil:Env:add-function (self function type &optional _context)
    (Struct:update self :functions (-partial #'cons (cons function type))))

  (fn Emil:Env:macro-environment (self &optional _context)
    (Struct:get self :macros))

  (fn Emil:Env:add-macro (self macro definition &optional _context)
    (Struct:update self :macros (-partial #'cons (cons macro definition)))))

(Struct:implement Emil:Env:Alist
  (fn Emil:Env:Alist:update-variable (self variable (type (Trait Emil:Type))
                                           &optional require-exists?)
    "Update VARIABLE's type to TYPE.

Adds VARIABLE to this environment, if it is currently not
present."
    (let ((elt (assq variable (Struct:get self :variables))))
      (cond
       (elt (setcdr elt type))
       (require-exists?
        (error "Variable is not bound in this environment: %s" variable))
       (t (Struct:update
            self :variables (-partial #'cons (cons variable type)))))))

  (fn Emil:Env:Alist:update-function (self function (type (Trait Emil:Type))
                                           &optional require-exists?)
    "Update FUNCTION's type to TYPE.

Adds FUNCTION to this environment, if it is currently not
present."
    (unless (Emil:Type:function? type)
      (error "Argument should be a function type: %s" type))
    (let ((elt (assq function (Struct:get self :functions))))
      (cond
       (elt (setcdr elt type))
       (require-exists?
        (error "Function is not bound in this environment: %s" function))
       (t (Struct:update
            self :functions (-partial #'cons (cons function type)))))))

  (fn Emil:Env:Alist:update-from (self (environment (Trait Emil:Env))
                                       variables functions
                                       &optional require-exists?)
    (--each variables
      (Emil:Env:Alist:update-variable
       self it (Emil:Env:lookup-variable environment it) require-exists?))
    (--each functions
      (Emil:Env:Alist:update-function
       self it (Emil:Env:lookup-function environment it) require-exists?))))

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

Lookups start with the first environment and proceed with the other
ones until some environment returns a non-`nil' value."

  (environments
   "The list of environments representing this hierarchy."
   :type list))

(Trait:implement Emil:Env Emil:Env:Hierarchy
  (fn Emil:Env:lookup-variable (self variable &optional context)
    (--some (Emil:Env:lookup-variable it variable context)
            (Struct:get self :environments)))

  (fn Emil:Env:lookup-function (self function &optional context)
    (--some (Emil:Env:lookup-function it function context)
            (Struct:get self :environments))))

(defun Emil:Env:empty ()
  "Returns an empty environment."
  (Emil:Env:Alist))

(Trait:implement Emil:Env Emil:Context
  (fn Emil:Env:lookup-variable (self variable &optional _context)
    "Looks up VARIABLE in the current, local environment.

Argument CONTEXT is ignored.

Returns `nil', if VARIABLE is not present in this environment."
    (-some->> (Emil:Context:lookup-variable self variable)
      (Emil:Context:resolve self)))

  (fn Emil:Env:lookup-function (_self _function &optional _context)
    "Looks up FUNCTION in the current, local environment.

Argument CONTEXT is ignored.

Returns `nil', if FUNCTION is not present in this environment."
    nil))

(provide 'Emil/Env)
