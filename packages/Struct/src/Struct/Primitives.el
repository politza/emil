;; -*- lexical-binding: t -*-

(require 'Commons)
(require 'dash)
(require 'cl-macs)

(defconst Struct:Type:definition-symbol 'Struct:Type:definition-symbol
  "Symbol used to attach struct type-information to other symbols.")

(define-symbol-prop 'Struct:Property Struct:Type:definition-symbol
  `(Struct:MetaType
    :name Struct:Property
    :properties
    ((Struct:Property
      :name name
      :keyword :name
      :default-value nil
      :documentation "The name of this propertỵ."
      :mutable nil
      :type symbol)
     (Struct:Property
      :name keyword
      :keyword :keyword
      :default-value (Commons:symbol-to-keyword name)
      :documentation "The name of this propertỵ as a keyword."
      :mutable nil
      :type keyword)
     (Struct:Property
      :name default-value
      :keyword :default-value
      :default-value nil
      :documentation "The default value of this propertỵ."
      :mutable nil
      :type nil)
     (Struct:Property
      :name documentation
      :keyword :documentation
      :default-value nil
      :documentation "The documentation of this propertỵ."
      :mutable nil
      :type (or null string))
     (Struct:Property
      :name mutable
      :keyword :mutable
      :default-value nil
      :documentation "Whether this property is mutable after its construction.
 Defaults to `nil'."
      :mutable nil
      :type boolean)
     (Struct:Property
      :name type
      :keyword :type
      :default-value nil
      :documentation "The type of this property."
      :mutable nil
      :type nil))))

(define-symbol-prop 'Struct:Type Struct:Type:definition-symbol
  '(Struct:MetaType
    :name Struct:Type
    :properties
    ((Struct:Property
      :name name
      :keyword :name
      :default-value nil
      :documentation "The name of this struct-type."
      :mutable nil
      :type symbol)
     (Struct:Property
      :name documentation
      :keyword :documentation
      :default-value nil
      :documentation "The documentation of this struct-type."
      :mutable nil
      :type (or null string))
     (Struct:Property
      :name properties
      :keyword :properties
      :default-value nil
      :documentation "The properties of this struct-type."
      :mutable nil
      :type list)
     (Struct:Property
      :name mutable
      :keyword :mutable
      :default-value t
      :documentation "Whether this type is mutable after its construction.
Defaults to `t'."
      :mutable nil
      :type boolean))))

(defsubst Struct:unsafe-get (struct property &optional default)
  "Returns STRUCT's current value of PROPERTY.

Returns DEFAULT if value is nil."
  (or (plist-get (cdr struct) property)
      default))

(defsubst Struct:unsafe-set (struct property value)
  "Sets STRUCT's PROPERTY to VALUE and returns it."
  (setcdr struct
          (plist-put (cdr struct) property value))
  value)

(defsubst Struct:unsafe-properties (struct)
  "Returns STRUCT's properties."
  (cdr struct))

(defun Struct:Type (&rest property-list)
  "Constructs a new struct-type."
  (Struct:construct 'Struct:Type property-list))

(defun Struct:Type? (struct)
  "Return `t', if STRUCT is a struct type. "
  (eq 'Struct:Type (car-safe struct)))

(cl-deftype Struct:Type ()
  `(satisfies Struct:Type?))

(defun Struct:Name? (name)
  (not (null (Struct:Type:get name))))

(cl-deftype Struct:Name ()
  `(satisfies Struct:Name?))

(defun Struct:Type:get (name &optional ensure)
  "Returns the `Struct:Type' definition of NAME.

Returns nil, if NAME does not name a struct-type; unless ENSURE is
non-nil, in which case a `wrong-type-argument' is signaled."
  (or (get name Struct:Type:definition-symbol)
      (and ensure
           (signal 'wrong-type-argument (list 'Struct:Name name)))))

(defun Struct:Type:property-names (properties)
  (--map (Commons:symbol-to-keyword it)
         (--map (Struct:unsafe-get it :name)
                properties)))

(defun Struct:Property (&rest property-list)
  "Creates a new struct-property."
  (Struct:construct 'Struct:Property property-list))

(defun Struct:Property? (struct)
  "Return `t', if STRUCT is a `Struct:Property' type. "
  (eq 'Struct:Property (car-safe struct)))

(cl-deftype Struct:Property ()
  `(satisfies Struct:Property?))

(defun Struct:Property:find (struct property)
  "Returns the `Struct:Property' of STRUCT for PROPERTY.

PROPERTY should have the keyword form.

Returns nil, if PROPERTY does not name a property of struct."
  (--find (eq property (Struct:unsafe-get it :keyword))
          (Struct:unsafe-get struct :properties)))

(defun Struct:Property:check-type (struct value)
  (when-let ((type (Struct:unsafe-get struct :type)))
    (or (cl-typep value type)
        (signal 'wrong-type-argument (list type value))))
  value)

(defun Struct:construct (name properties)
  "Constructs a struct of type NAME using PROPERTIES"
  (let ((type (Struct:Type:get name :ensure)))
    (cons name (->> properties
                    (Struct:-initial-properties type)
                    (Struct:-construct-properties type)))))

(defun Struct:-empty-properties (type)
  (--splice (prog1 t (ignore it))
            (list it nil)
            (Struct:Type:property-names
             (Struct:unsafe-get type :properties))))

(defun Struct:-initial-properties (type provided-properties)
  (let* ((initial-properties (Struct:-empty-properties type))
         (property-count (/ (length initial-properties) 2)))
    (while provided-properties
      (let ((keyword (pop provided-properties))
            (value (pop provided-properties)))
        (setq initial-properties (plist-put initial-properties keyword value))))
    (unless (= property-count (/ (length initial-properties) 2))
      (error "Undeclared properties set: %s"
             (nthcdr (* 2 property-count) initial-properties)))
    initial-properties))

(defun Struct:-construct-properties (type properties)
  (let ((declared-properties (Struct:unsafe-get type :properties))
        (environment nil)
        (properties-head properties))
    (while declared-properties
      (let ((property (pop declared-properties))
            (value (nth 1 properties-head)))
        (unless value
          (when-let (default-value (Struct:unsafe-get property :default-value))
            (setq value (eval default-value environment))))
        (push (cons (Struct:unsafe-get property :name) value)
              environment)
        (setcar (cdr properties-head)
                (Struct:Property:check-type property value))
        (setq properties-head (nthcdr 2 properties-head))))
    properties))

(defun Struct:get (struct property &optional default)
  "Returns STRUCT's current value of PROPERTY.

Throws an error if PROPERTY is not a member of STRUCT.

Returns DEFAULT, if value is `nil'."
  (unless (Struct:Property:find
           (Struct:Type:get (car struct) :ensure)
           property)
    (error "Property is not a member of struct: %s" property))
  (or (Struct:unsafe-get struct property)
      default))

(defun Struct:set (struct property value)
  "Sets STRUCT's PROPERTY to VALUE.

Throws an error if
- PROPERTY is not a member of STRUCT, or
- PROPERTY is not mutable, or
- PROPERTY has an associated type and VALUE does not match it."
  (let* ((struct-type (Struct:Type:get (car struct) :ensure))
         (property-type (Struct:Property:find struct-type property)))
    (unless property-type
      (error "Property is not a member of struct: %s" property))
    (unless (and (Struct:unsafe-get struct-type :mutable)
                 (Struct:unsafe-get property-type :mutable))
      (error "Attempted to set immutable property: %s" property))
    (Struct:Property:check-type property-type value))
  (Struct:unsafe-set struct property value))

(defun Struct:update (struct property fn)
  (Struct:set struct property
              (funcall fn (Struct:get struct property))))

(defun Struct:properties (struct)
  "Returns STRUCT's properties and values as a property-list.

This function returns a new property-list everytime its called."
  (copy-sequence (Struct:unsafe-properties struct)))

(provide 'Struct/Primitives)
