;; -*- lexical-binding: t -*-

(require 'Emil/Type)
(require 'Commons)
(require 'dash)
(require 'Struct)
(require 'Struct/Pcase)

(Struct:define Emil:Context
  "Provides a context for type-inference.

The entries of the context are organized as a stack, i.e. entries are
added and removed from the left."
  (entries :type list))

(Struct:define Emil:Context:Binding
  "Maps a variable to some type.

The term variable refers to a programmer's variable, i.e. not a
type-variable. Note, that type may be incomplete: It may refer to yet
to be solved type-variables."
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

Returns a list of 2 new contexts \(LEFT-CONTEXT RIGHT-CONTEXT\)
representing the entries left resp. right of VARIABLE, which itself is
not included in neither.

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
split twice. Returns a triple of 3 new contexts \(LEFT MID RIGHT\),
given that the entries of this context look like \(LEFT (VARIABLE) MID
(OTHER) RIGHT\) appended together.

Returns `nil', if either VARIABLE or OTHER is not a member of this
context; or if OTHER appears in front of VARIABLE."
    (-when-let* ((left (Emil:Context:hole self variable))
                 (right (Emil:Context:hole (nth 1 left) other)))
      (cons (car left) right)))

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
                        (equal (Struct:get variable :name)
                               (Struct:get other :name))))
                     (Struct:get self :entries))
      (Struct:get :type)))

  (fn Emil:Context:drop-until-after (self entry)
    "Remove entries up-to and including the given ENTRY.

Returns an empty context, if ENTRY is not present in this one."
    (Emil:Context
     :entries
     (cdr (--drop-while (not (equal it entry))
                        (Struct:get self :entries)))))

  (fn Emil:Context:well-formed-type? (self (type (Trait Emil:Type)))
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
         (and (--every? (Emil:Context:well-formed-type? self it)
                        argument-types)
              (Emil:Context:well-formed-type? self return-type)
              (or (null rest-type)
                  (Emil:Context:well-formed-type? self rest-type))))
        ((Struct Emil:Type:Forall variables type)
         (Emil:Context:well-formed-type?
          (Emil:Context :entries (append variables entries nil))
          type)))))

  (fn Emil:Context:resolve (self (type (Trait Emil:Type)))
    "Resolves the given TYPE against this context.

This will substitute any solved instantiated type-variables with their
resolved type. This applies recursively; both to the TYPE and its
unresolved type-variables."
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
        :type (Emil:Context:resolve self type))))))

(provide 'Emil/Context)
