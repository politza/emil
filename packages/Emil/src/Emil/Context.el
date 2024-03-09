;; -*- lexical-binding: t -*-

(require 'Emil/Type)
(require 'Commons)
(require 'dash)
(require 'Struct)
(require 'Struct/Pcase)

(Struct:define Emil:Context
  "Provides a context for type-inference.

The entries of the context are organized as a stack, i.e. entries are
added and removed at/from the left and it should be read bottom to
top."
  (entries :type list))

(Struct:define Emil:Context:Binding
  "Maps a variable to some type.

The term variable refers to a programmer's variable, i.e. not a
type-variable. Note, that type may be incomplete, i.e. it may refer to
yet to be resolved type-variables."
  (variable :type symbol)
  (type :type (Trait Emil:Type)))

(Struct:define Emil:Context:Marker
  "Defines a marker establishing a before and after context.

The marker contains an arbitrary token value, in order to
differentiate this marker from other markers. Though, in the current
implementation it is always a value of type `Emil:Type:VarInst'."
  (token))

(Struct:define Emil:Context:SolvedVarInst
  "Maps an instance of a variable-type to some type.

During type-inference, instances of type-variables of polymorphic
types are resolved to other types. This struct keeps track of these
assignments."
  (variable :type Emil:Type:VarInst)
  (type :type (Trait Emil:Type)))

(Struct:implement Emil:Context
  (fn Emil:Context:hole (self (variable Emil:Type:VarInst))
    "Splits this context at VARIABLE.

Returns a list of 2 new contexts \(TOP BOTTOM\) representing the
entries above resp. below VARIABLE, which itself is not included
in neither. Remember that the context read bottom to top.

Returns `nil', if VARIABLE is not a member of this context."
    (when-let (tail (member variable (Struct:get self :entries)))
      (list (Emil:Context :entries
                          (--take-while (not (equal variable it))
                                        (Struct:get self :entries)))
            (Emil:Context :entries (cdr tail)))))

  (fn Emil:Context:double-hole (self (variable Emil:Type:VarInst)
                                     (other Emil:Type:VarInst))
    "Splits this context at VARIABLE and OTHER.

This works similar to `Emil:Context:hole', except that the entries are
split twice. Returns a triple of 3 new contexts \(TOP CENTER BOTTOM\),
given that the entries of this context look like \(TOP (VARIABLE) CENTER
(OTHER) BOTTOM\) appended together.

Returns `nil', if either VARIABLE or OTHER is not a member of this
context; or if OTHER appears in front of VARIABLE."
    (-when-let* ((top (Emil:Context:hole self variable))
                 (bottom (Emil:Context:hole (nth 1 top) other)))
      (cons (car top) bottom)))

  (fn Emil:Context:lookup-binding (self (variable symbol) -> (Trait Emil:Type))
    "Lookup VARIABLE in this context.

Returns variable's type; or `nil', if VARIABLE is not bound in this
context."
    (-some-> (--find (pcase it
                       ((Struct Emil:Context:Binding :variable other)
                        (equal variable other)))
                     (Struct:get self :entries))
      (Struct:get :type)))

  (fn Emil:Context:lookup-solved (self (variable Emil:Type:VarInst)
                                       -> (Trait Emil:Type))
    "Lookup the instantiated type VARIABLE in this context.

Returns variable's type; or `nil', if VARIABLE is not bound in this
context."
    (-some-> (--find (pcase it
                       ((Struct Emil:Context:SolvedVarInst :variable other)
                        (equal variable other)))
                     (Struct:get self :entries))
      (Struct:get :type)))

  (fn Emil:Context:drop-until-after (self entry)
    "Remove entries up-to and including the given ENTRY.

Returns an empty context, if ENTRY is not present in this one."
    (Emil:Context
     :entries
     (cdr (--drop-while (not (equal it entry))
                        (Struct:get self :entries)))))

  (fn Emil:Context:well-formed? (self (type (Trait Emil:Type)))
    "Returns non-nil, if TYPE is well-formed given this context."
    (let ((entries (Struct:get self :entries)))
      (pcase-exhaustive type
        ((or (Struct Emil:Type:Any)
             (Struct Emil:Type:Never)
             (Struct Emil:Type:Null)
             (Struct Emil:Type:Void)
             (Struct Emil:Type:Basic))
         t)
        ((Struct Emil:Type:Var)
         (not (null (member type entries))))
        ((Struct Emil:Type:VarInst)
         (not (null (or (member type entries)
                        (Emil:Context:lookup-solved self type)))))
        ((Struct Emil:Type:Fn argument-types rest-type return-type)
         (and (--every? (Emil:Context:well-formed? self it)
                        argument-types)
              (Emil:Context:well-formed? self return-type)
              (or (null rest-type)
                  (Emil:Context:well-formed? self rest-type))))
        ((Struct Emil:Type:Forall variables type)
         (Emil:Context:well-formed?
          (Emil:Context :entries (append variables entries nil))
          type)))))

  (fn Emil:Context:resolve (self (type (Trait Emil:Type)))
    "Resolves the given TYPE against this context.

This will substitute any solved instantiated type-variables with their
resolved type. This applies recursively; both within the TYPE, as well
as with regards to its unresolved type-variables."
    (pcase-exhaustive type
      ((or (Struct Emil:Type:Any)
           (Struct Emil:Type:Never)
           (Struct Emil:Type:Null)
           (Struct Emil:Type:Void)
           (Struct Emil:Type:Basic)
           (Struct Emil:Type:Var))
       type)
      ((Struct Emil:Type:VarInst)
       (if-let (resolved (Emil:Context:lookup-solved self type))
           (Emil:Context:resolve self resolved)
         type))
      ((Struct Emil:Type:Fn argument-types rest-type return-type)
       (let ((argument-types
              (--map (Emil:Context:resolve self it) argument-types))
             (rest-type (if rest-type (Emil:Context:resolve self rest-type)))
             (return-type (Emil:Context:resolve self return-type)))
         (Emil:Type:Fn* ,@type argument-types rest-type return-type)))
      ((Struct Emil:Type:Forall variables type)
       (Emil:Type:Forall*
        variables
        :type (Emil:Context:resolve self type)))))

  (fn Emil:Context:concat (self &rest contexts-and-entries)
    "Concat this context with the provided CONTEXTS-AND-ENTRIES.

Each argument may either be a context or a single entry."
    (Emil:Context
     :entries (--mapcat (pcase it
                          ((Struct Emil:Context entries)
                           entries)
                          (_ (list it)))
                        (cons self contexts-and-entries))))

  (fn Emil:Context:member? (self entry)
    (member entry (Struct:get self :entries))))

(Trait:define Emil:Environment ()
  (fn Emil:Environment:lookup (self (variable symbol) &optional related)
    "Looks up VARIABLE in an environment and returns its type.

Optional argument RELATED is a related environment, which may provide
additional bindings pertaining to this one. Typically a local
environment used by a global one.

Returns `nil', if VARIABLE is not bound in this environment."))

(Trait:implement Emil:Environment Emil:Context
  (fn Emil:Environment:lookup (self variable &optional _related)
    "Looks up VARIABLE in the current, local environment.

Argument RELATED is ignored.

Returns `nil', if VARIABLE is not present in this environment."
    (-some->> (Emil:Context:lookup-binding self variable)
      (Emil:Context:resolve self))))

(provide 'Emil/Context)
