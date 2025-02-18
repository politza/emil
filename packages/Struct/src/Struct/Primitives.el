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
(require 'cl-macs)

(defconst Struct:Type:definition-symbol 'Struct:Type:definition-symbol
  "Symbol used to attach struct type-information to other symbols.")

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

(define-symbol-prop 'Struct:Property Struct:Type:definition-symbol
  `(Struct:MetaType
    :name Struct:Property
    :properties
    ,(--map
      (cons (Commons:symbol-to-keyword (Struct:unsafe-get it :name)) it)
      '((Struct:Property
         :name name
         :default nil
         :documentation "The name of this propertỵ."
         :mutable nil
         :type symbol)
        (Struct:Property
         :name default
         :default nil
         :documentation "The default value of this propertỵ."
         :mutable nil
         :type nil)
        (Struct:Property
         :name documentation
         :default nil
         :documentation "The documentation of this propertỵ."
         :mutable nil
         :type (or null string))
        (Struct:Property
         :name mutable
         :default nil
         :documentation "Whether this property can be changed."
         :mutable nil
         :type boolean)
        (Struct:Property
         :name type
         :default nil
         :documentation "The type of this property."
         :mutable nil
         :type nil)
        (Struct:Property
         :name metadata
         :default nil
         :documentation "Contains artibrary metadata for this property."
         :mutable nil
         :type list)))))

(define-symbol-prop 'Struct:Type Struct:Type:definition-symbol
  `(Struct:MetaType
    :name Struct:Type
    :properties
    ,(--map
      (cons (Commons:symbol-to-keyword (Struct:unsafe-get it :name)) it)
      '((Struct:Property
         :name name
         :default nil
         :documentation "The name of this struct-type."
         :mutable nil
         :type symbol)
        (Struct:Property
         :name documentation
         :default nil
         :documentation "The documentation of this struct-type."
         :mutable nil
         :type (or null string))
        (Struct:Property
         :name properties
         :default nil
         :documentation "The properties of this struct-type.

This is an association-list mapping the keyword-names to their
corresponding property."
         :mutable nil
         :type list)
        (Struct:Property
         :name functions
         :default nil
         :documentation "Implemented functions associated with this struct-type.

This association-list maps qualified names to their declaration."
         :mutable t
         :type list)
        (Struct:Property
         :name metadata
         :default nil
         :documentation "Contains arbitrary metadata for this struct-type."
         :mutable nil
         :type list)))))

(defun Struct:Type (&rest property-list)
  "Constructs a new struct-type."
  (Struct:construct 'Struct:Type property-list))

(defun Struct:Type? (struct)
  "Return `t', if STRUCT is a struct type. "
  (eq 'Struct:Type (car-safe struct)))

(cl-deftype Struct:Type ()
  `(satisfies Struct:Type?))

(defun Struct:Type:define (name type)
  (cl-check-type type Struct:Type)
  (put name Struct:Type:definition-symbol (copy-sequence type)))

(defun Struct:Type:undefine (name)
  (when (memq name '(Struct:Type Struct:Property))
    (error "Attempted to undefine primitive type: %s" name))
  (put name Struct:Type:definition-symbol nil))

(defun Struct:Name? (name)
  (not (null (Struct:Type:get name))))

(defsubst Struct:name (value)
  "Returns the struct-name of the given VALUE.

Returns nil, if VALUE is not a struct type."
  (and (Struct:Type:get (car-safe value))
       (car value)))

(cl-deftype Struct:Name ()
  `(satisfies Struct:Name?))

(defun Struct:Type:get (name &optional ensure)
  "Returns the `Struct:Type' definition of NAME.

Returns nil, if NAME does not name a struct-type; unless ENSURE is
non-nil, in which case a `wrong-type-argument' is signaled."
  (or (and (symbolp name)
           (get name Struct:Type:definition-symbol))
      (and ensure
           (signal 'wrong-type-argument (list 'Struct:Name name)))))

(defun Struct:Property (&rest property-list)
  "Creates a new struct-property."
  (Struct:construct 'Struct:Property property-list))

(defun Struct:Property? (struct)
  "Return `t', if STRUCT is a `Struct:Property' type. "
  (eq 'Struct:Property (car-safe struct)))

(cl-deftype Struct:Property ()
  `(satisfies Struct:Property?))

(defun Struct:Type:get-property (struct property)
  "Returns the `Struct:Property' of STRUCT for PROPERTY.

Returns nil, if PROPERTY does not name a property of struct."
  (cdr (assq property (Struct:unsafe-get struct :properties))))

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

(defun Struct:-initial-properties (type provided-properties)
  (let* ((initial-properties (Struct:-empty-properties type))
         (property-count (/ (length initial-properties) 2)))
    (while provided-properties
      (let ((keyword (pop provided-properties))
            (value (pop provided-properties)))
        (setq initial-properties (plist-put initial-properties keyword value))))
    (unless (= property-count (/ (length initial-properties) 2))
      (error "Undeclared properties set while constructing `%s': %s"
             (Struct:unsafe-get type :name)
             (nthcdr (* 2 property-count) initial-properties)))
    initial-properties))

(defun Struct:-empty-properties (type)
  (let ((properties nil))
    (dolist (declared-property (Struct:unsafe-get type :properties))
      (push (car declared-property) properties)
      (push nil properties))
    (nreverse properties)))

(defun Struct:-construct-properties (type properties)
  (let ((declared-properties
         (Struct:unsafe-get type :properties))
        (environment nil)
        (properties-head properties))
    (while declared-properties
      (let ((property (cdr (pop declared-properties)))
            (value (nth 1 properties-head)))
        (unless value
          (when-let (default (Struct:unsafe-get property :default))
            (setq value (eval default environment))))
        (push (cons (Struct:unsafe-get property :name) value)
              environment)
        (setcar (cdr properties-head)
                (Struct:Property:check-type property value))
        (setq properties-head (nthcdr 2 properties-head))))
    properties))

(defun Struct? (object)
  "Return non-nil, if OBJECT is a struct type."
  (Struct:Type:get (car-safe object)))

(defun Struct:get (struct property &optional default)
  "Returns STRUCT's current value of PROPERTY.

Throws an error if PROPERTY is not a member of STRUCT.

Returns DEFAULT, if value is `nil'."
  (unless (Struct:Type:get-property
           (Struct:Type:get (car struct) :ensure)
           property)
    (error "Property is not a member of struct `%s': %s"
           (car-safe struct)
           property))
  (or (Struct:unsafe-get struct property)
      default))

(defun Struct:set (struct property value)
  "Sets STRUCT's PROPERTY to VALUE.

Throws an error if
- PROPERTY is not a member of STRUCT, or
- PROPERTY is immutable, or
- PROPERTY has an associated type and VALUE does not match it."
  (declare (indent 2))
  (let* ((struct-type (Struct:Type:get (car struct) :ensure))
         (property-type (Struct:Type:get-property struct-type property)))
    (unless property-type
      (error "Property is not a member of struct: %s" property))
    (unless (Struct:unsafe-get property-type :mutable)
      (error "Attempted to set immutable property: %s" property))
    (Struct:Property:check-type property-type value))
  (Struct:unsafe-set struct property value))

(defun Struct:update (struct property fn)
  "Sets STRUCT's PROPERTY using update-function FN.

Returns the updated value."
  (Struct:set struct property
              (funcall fn (Struct:get struct property))))

(defun Struct:properties (struct)
  "Returns STRUCT's properties and values as a property-list.

This function returns a new property-list everytime it's invoked."
  (copy-sequence (Struct:unsafe-properties struct)))

(defun Struct:metadata (object)
  "Returns the metadata associated with OBJECT.

Object should either be a struct value or a symbol denoting a struct-type."
  (let ((name (if (symbolp object) object (car-safe object))))
    (Struct:get (Struct:Type:get name :ensure) :metadata)))

(defun Struct:property-metadata (object property)
  "Returns the metadata associated with OBJECT's PROPERTY.

OBJECT should either be a struct value or a symbol denoting a struct-type.

PROPERTY should be a keyword denoting the property for which
metadata should be retrieved."
  (let* ((name (if (symbolp object) object (car-safe object)))
         (type (Struct:Type:get name :ensure))
         (definition (alist-get property (Struct:get type :properties))))
    (unless definition
      (error "Property `%s' is not a member of struct `%s'" property name))
    (Struct:get definition :metadata)))

(provide 'Struct/Primitives)
