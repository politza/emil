;; -*- lexical-binding: t -*-

(require 'Commons)

(Trait:define Emil:Env ()
  (fn Emil:Env:lookup-variable (self (variable symbol)
                                     &optional (context (Trait Emil:Context)))
    "Looks up VARIABLE in this environment and returns its type.

Optional CONTEXT contains information pertaining to the current
type-inference state. Since it also implements `Emil:Env', it can also
be used to lookup local variables and functions.

Returns `nil', if VARIABLE is not bound in this environment.")

  (fn Emil:Env:lookup-function (self (function symbol)
                                     &optional (context (Trait Emil:Context)))
    "Looks up FUNCTION in this environment and returns its type.

See `Emil:Env:lookup-variable' for the CONTEXT argument.

Returns `nil', if FUNCTION is not bound in this environment."))

(Struct:define Emil:Env:Alist
  "Defines an environment represented via association lists."
  (variables
   "An association list mapping variable names to their `Emil:Type'."
   :type list)
  (functions
   "An association list mapping function names to their `Emil:Type:Arrow'."
   :type list))

(Trait:implement Emil:Env Emil:Env:Alist
  (fn Emil:Env:lookup-variable (self variable &optional _context)
    (cdr (assq variable (Struct:get self :variables))))

  (fn Emil:Env:lookup-function (self function &optional _context)
    (cdr (assq function (Struct:get self :functions)))))

(defun Emil:Env:Alist:read (variables functions)
  "Reads an environment from VARIABLES and FUNCTIONS.

Both arguments should be association lists, mapping symbols to the
printed representation of types.

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
                     functions)))

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
  (Emil:Env:Alist :variables nil :functions nil))

(provide 'Emil/Env)
