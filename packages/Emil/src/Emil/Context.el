;; -*- lexical-binding: t -*-

(require 'Emil/Type)
(require 'Commons)
(require 'dash)
(require 'Struct)
(require 'Struct/Pcase)
(require 'Trait)

(Struct:define Emil:Context
  "Provides a context for type-inference.

The entries of the context are organized as a stack, i.e. entries are
added and removed at/from the left and it should be read bottom to
top."
  (entries :type list))

(Struct:define Emil:Context:Binding
  "Maps a variable to some type.

Variable here refers to a programmer's variable, i.e. not a
type-variable. Note, that type may be incomplete, i.e. it may refer to
yet to be resolved type-variables."
  (variable :type symbol)
  (type :type (Trait Emil:Type)))

(defvar Emil:Context:Marker:counter 0)

(Struct:define Emil:Context:Marker
  "Defines a marker establishing a before and after context.

The marker contains an arbitrary token value. The actual value does
not matter, just that it is different from the values of all other
markers. By default it is initialized with the current value of a
monotonically increasing counter stored in the variable
`Emil:Context:Marker:counter'."
  (token :default (cl-incf Emil:Context:Marker:counter)))

(Struct:define Emil:Context:Solution
  "Maps a `Emil:Context:Existential' variable to some type.

During type-inference, instances of type-variables of polymorphic
types are resolved to other types. This struct keeps track of these
assignments."
  (variable :type Emil:Type:Existential)
  (type :type (Trait Emil:Type)))

(Struct:implement Emil:Context
  :disable-syntax t
  (fn Emil:Context:hole (self (variable Emil:Type:Existential))
    "Splits this context at VARIABLE.

Returns a cons of 2 new contexts \(TOP . BOTTOM\) representing the
entries above resp. below VARIABLE, which itself is not included in
neither.

Returns `nil', if VARIABLE is not a member of this context."
    (when-let (tail (member variable (Struct:get self :entries)))
      (cons (Emil:Context :entries
                          (--take-while (not (equal variable it))
                                        (Struct:get self :entries)))
            (Emil:Context :entries (cdr tail)))))

  (fn Emil:Context:double-hole (self (variable Emil:Type:Existential)
                                     (other Emil:Type:Existential))
    "Splits this context at VARIABLE and OTHER.

This works similar to `Emil:Context:hole', except that the entries are
split twice. Returns a triple of 3 new contexts \(TOP CENTER BOTTOM\),
given that the entries of this context look like \(TOP (VARIABLE) CENTER
(OTHER) BOTTOM\) appended together.

Returns `nil', if either VARIABLE or OTHER is not a member of this
context; or if OTHER appears in front of VARIABLE."
    (-when-let* ((top (Emil:Context:hole self variable))
                 (bottom (Emil:Context:hole (cdr top) other)))
      (cons (car top) (list (car bottom) (cdr bottom)))))

  (fn Emil:Context:lookup-variable (self (variable symbol) -> (Trait Emil:Type))
    "Lookup VARIABLE in this context.

Returns variable's type; or `nil', if VARIABLE is not bound in this
context."
    (-some-> (--find (pcase it
                       ((Struct Emil:Context:Binding :variable other)
                        (equal variable other)))
                     (Struct:get self :entries))
      (Struct:get :type)))

  (fn Emil:Context:lookup-function (_self _function)
    "Lookup FUNCTION in this context.

Currently, this always returns nil."
    nil)

  (fn Emil:Context:lookup-solved (self (variable Emil:Type:Existential)
                                       -> (Trait Emil:Type))
    "Lookup the instantiated type VARIABLE in this context.

Returns variable's type; or `nil', if VARIABLE is not bound in this
context."
    (-some-> (--find (pcase it
                       ((Struct Emil:Context:Solution :variable other)
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
        ((Struct Emil:Type:Variable)
         (not (null (member type entries))))
        ((Struct Emil:Type:Existential)
         (not (null (or (member type entries)
                        (Emil:Context:lookup-solved self type)))))
        ((Struct Emil:Type:Arrow arguments returns)
         (and (--every? (Emil:Context:well-formed? self it)
                        arguments)
              (Emil:Context:well-formed? self returns)))
        ((Struct Emil:Type:Forall parameters type)
         (Emil:Context:well-formed?
          (Emil:Context :entries (append parameters entries nil))
          type))
        ((Struct Emil:Type:Compound arguments)
         (--every? (Emil:Context:well-formed? self it)
                   arguments)))))

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
           (Struct Emil:Type:Variable))
       type)
      ((Struct Emil:Type:Existential)
       (if-let (resolved (Emil:Context:lookup-solved self type))
           (Emil:Context:resolve self resolved)
         type))
      ((Struct Emil:Type:Arrow arguments returns)
       (let ((arguments
              (--map (Emil:Context:resolve self it) arguments))
             (returns (Emil:Context:resolve self returns)))
         (Emil:Type:Arrow* ,@type arguments returns)))
      ((Struct Emil:Type:Forall parameters type)
       (Emil:Type:Forall*
        parameters
        :type (Emil:Context:resolve self type)))
      ((Struct Emil:Type:Compound arguments)
       (let ((arguments
              (--map (Emil:Context:resolve self it) arguments)))
         (Emil:Type:Compound* ,@type arguments)))))

  (fn Emil:Context:valid-entry? (object)
    (or (Trait:implements? (Trait:type-of object) 'Emil:Type)
        (Emil:Context:Solution? object)
        (Emil:Context:Marker? object)
        (Emil:Context:Binding? object)))

  (fn Emil:Context:concat (&rest contexts-and-entries)
    "Concat all arguments CONTEXTS-AND-ENTRIES.

Each argument may either be a context, an entry or a, possibly
empty, list of entries. Returns a context with all arguments
concatenated."
    (Emil:Context
     :entries
     (--mapcat (pcase it
                 ((Struct Emil:Context entries)
                  entries)
                 ((pred Emil:Context:valid-entry?)
                  (list it))
                 ((and (pred listp)
                       (guard (-every? #'Emil:Context:valid-entry? it)))
                  it)
                 (_
                  (error "Attempted to concat an invalid context entry: %s" it)))
               contexts-and-entries)))

  (fn Emil:Context:member? (self entry)
    (member entry (Struct:get self :entries))))

(provide 'Emil/Context)
