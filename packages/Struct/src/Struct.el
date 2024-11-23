;;; Struct.el --- A struct data-type.             -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Andreas Politz

;; Author: Andreas Politz <mail@andreas-politz.de>
;; Keywords: extensions
;; Version: 1.0.0beta1
;; Package-Requires: ((emacs "29.1") (dash "2.19.1") (Commons "1.0.0beta1"))

(require 'Commons)
(require 'cl-macs)
(require 'dash)

(defconst Struct:Type:definition-symbol 'Struct:Type:definition-symbol
  "Symbol used to attach struct type-information to other symbols.")

(defconst Struct:syntax-highlight-symbol 'Struct:syntax-highlight-symbol
  "Symbol to attach font-lock information to other symbols.")

(defvar Struct:enable-syntax-highlighting t
  "Whether to highlight struct types with face `font-lock-type-face'.")

;; These `defsubst' definitions need to be defined before they are
;; used.
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
  (Struct:-construct 'Struct:Type property-list))

(defmacro Struct:Type* (&rest property-list)
  "Constructs a new struct-type."
  (declare (no-font-lock-keyword t)
           (debug t))
  (list 'Struct:-construct
        'Struct:Type (Struct:-expand-syntax property-list)))

(define-symbol-prop 'Struct:Type Struct:Type:definition-symbol
     '(Struct:MetaType
       :name Struct:Type
       :properties
       ((Struct:Property
         :name name
         :keyword :name
         :default-value nil
         :documentation "The name of this struct-type."
         :required t
         :mutable nil
         :type symbol)
        (Struct:Property
         :name documentation
         :keyword :documentation
         :default-value nil
         :documentation "The documentation of this struct-type."
         :required nil
         :mutable nil
         :type (or null string))
        (Struct:Property
         :name properties
         :keyword :properties
         :default-value nil
         :documentation "The properties of this struct-type."
         :required nil
         :mutable nil
         :type list)
        (Struct:Property
         :name mutable
         :keyword :mutable
         :default-value t
         :documentation "Whether this type is mutable after its construction.
Defaults to `t'."
         :required nil
         :mutable nil
         :type boolean))))

(defun Struct:Type:get (name &optional ensure)
  "Returns the `Struct:Type' definition of NAME.

Returns nil, if NAME does not name a struct-type; unless ENSURE is
non-nil, in which case a `wrong-type-argument' is signaled."
  (or (get name Struct:Type:definition-symbol)
      (and ensure
           (signal 'wrong-type-argument (list 'Struct:Type name)))))

(defun Struct:Type? (name-or-type-struct)
  "Return `t', if NAME-OR-TYPE-STRUCT is a struct-type.

NAME-OR-TYPE-STRUCT can be either a symbol naming a struct-type or
the type itself."
  (let ((type-struct (if (symbolp name-or-type-struct)
                         (Struct:Type:get name-or-type-struct)
                       name-or-type-struct)))
    (eq 'Struct:Type (car-safe type-struct))))

(defmacro Struct:Property* (&rest property-list)
  "Creates a new struct-property."
  (declare (no-font-lock-keyword t)
           (debug t))
  (list 'Struct:-construct
        'Struct:Property (Struct:-expand-syntax property-list)))

(defun Struct:Property (&rest property-list)
  "Creates a new struct-property."
  (Struct:-construct 'Struct:Property property-list))

(define-symbol-prop 'Struct:Property Struct:Type:definition-symbol
     `(Struct:MetaType
       :name Struct:Property
       :properties
       ((Struct:Property
         :name name
         :keyword :name
         :default-value nil
         :documentation "The name of this propertỵ."
         :required t
         :mutable nil
         :type symbol)
        (Struct:Property
         :name keyword
         :keyword :keyword
         :default-value (Commons:symbol-to-keyword name)
         :documentation "The name of this propertỵ as a keyword."
         :required t
         :mutable nil
         :type keyword)
        (Struct:Property
         :name default-value
         :keyword :default-value
         :default-value nil
         :documentation "The default value of this propertỵ."
         :required nil
         :mutable nil
         :type nil)
        (Struct:Property
         :name documentation
         :keyword :documentation
         :default-value nil
         :documentation "The documentation of this propertỵ."
         :required nil
         :mutable nil
         :type (or null string))
        (Struct:Property
         :name required
         :keyword :required
         :default-value nil
         :documentation "Whether this property can have a `nil' valuẹ."
         :required nil
         :mutable nil
         :type boolean)
        (Struct:Property
         :name mutable
         :keyword :mutable
         :default-value nil
         :documentation "Whether this property is mutable after its construction.
 Defaults to `nil'."
         :required nil
         :mutable nil
         :type boolean)
        (Struct:Property
         :name type
         :keyword :type
         :default-value nil
         :documentation "The type of this property."
         :required nil
         :mutable nil
         :type nil))))

(defun Struct:-construct (name arguments)
  (let ((type (Struct:Type:get name :ensure)))
    (cons name (->> arguments
                    (Struct:-initial-property-list type)
                    (Struct:-construct-property-list type)))))

(defun Struct:-initial-property-list (type arguments)
  (let* ((property-list (Struct:-empty-property-list type))
         (property-count (/ (length property-list) 2)))
    (while arguments
      (let ((keyword (pop arguments))
            (value (pop arguments)))
        (setq property-list (plist-put property-list keyword value))))
    (unless (= property-count (/ (length property-list) 2))
      (error "Undeclared properties set: %s"
             (nthcdr (* 2 property-count) property-list)))
    property-list))

(defun Struct:-property-keywords (properties)
  (--map (Commons:symbol-to-keyword it)
         (--map (Struct:unsafe-get it :name)
                properties)))

(defun Struct:-empty-property-list (type)
  (--splice (prog1 t (ignore it))
            (list it nil)
            (Struct:-property-keywords
             (Struct:unsafe-get type :properties))))

(defun Struct:-construct-property-list (type property-list)
  (let ((properties (Struct:unsafe-get type :properties))
        (environment nil)
        (property-list-head property-list))
    (while properties
      (let ((property (pop properties))
            (value (nth 1 property-list-head)))
        (unless value
          (when-let (default-value (Struct:unsafe-get property :default-value))
            (setq value (eval default-value environment))))
        (unless (or value (not (Struct:unsafe-get property :required)))
          (error "Required property not provided: %s"
                 (Struct:unsafe-get property :name)))
        (push (cons (Struct:unsafe-get property :name) value)
              environment)
        (setcar (cdr property-list-head)
                (Struct:-check-property-type property value))
        (setq property-list-head (nthcdr 2 property-list-head))))
    property-list))

(defun Struct:-check-property-type (property-type value)
  (when-let ((type (Struct:unsafe-get property-type :type)))
    (or (and (null value)
             (not (Struct:unsafe-get property-type :required)))
        (cl-typep value type)
        (signal 'wrong-type-argument (list type value))))
  value)

;;;###autoload
(defmacro Struct:define (name &optional documentation &rest declarations)
  "Defines a new struct-type named NAMẸ.

DOCUMENTATION is an optional string describing the type.

STRUCT-META-PROPERTY is a keyword/value pair of which an arbitrary
number may be provided.  The supported properties are listed in the
documentation of `Struct:Type'.  All of which can be used, with the
exception of `:name', `:documentation' and `:properties'.

The remaining forms, if present, determine the properties of the
defined struct.

Each such form may be just the property's name, or a list consisting
of its name, an optional documentation string and an arbitrary number
of meta-properties.  See `Struct:Property' for a list of supported
meta-properties.  All of which mentioned there can be used.

Defining a struct also defines 2 functions, a macro and a new cl-type,
which are:

1. A type-constructor having the name of the struct. It accepts
keyword/value pairs for defining the properties of the struct-value.

2. A type-predicate having the name of the struct with a question-mark
appended.

3. Another type-constructor with the name of type with an asterisk
appended. It works like the first constructor, except that it is
defined as a macro and is therefore able to support shorthand- as well
as splice-syntax.

4. A cl-type having the name of the struct, which may be used with
`cl-check-type' and other cl-type features.

(fn NAME [DOCUMENTATION]? [STRUCT-META-PROPERTY]* [PROPERTY-NAME |
(PROPERTY-NAME [DOCUMENTATION]? [PROPERTY-META-PROPERTY]*)]*)"
  (declare (indent 1) (doc-string 2))
  (unless (symbolp name)
    (error "Expected a symbol: %s" name))
  (unless (or (stringp documentation)
              (null documentation))
    (push documentation declarations)
    (setq documentation nil))
  (-let* (((struct-declarations property-declarations)
           (Commons:split-property-list-start declarations))
          (struct-property-list
           (append struct-declarations
                   (list :name name :documentation documentation
                         :properties
                         (-map #'Struct:-construct-property
                               property-declarations))))
          (type (apply #'Struct:Type struct-property-list))
          (macro-name (intern (concat (symbol-name name) "*")))
          (predicate-name (intern (concat (symbol-name name) "?"))))
    `(progn
       (defun ,name (&rest property-list)
         ,(Struct:-doc-constructor type)
         (Struct:-construct ',name property-list))
       (defmacro ,macro-name (&rest property-list)
         ,(Struct:-doc-constructor type)
         (declare (no-font-lock-keyword t)
                  (debug t))
         (list 'Struct:-construct
               '',name (Struct:-expand-syntax property-list)))
       (defun ,predicate-name (object &optional ensure)
         ,(Struct:-doc-predicate type)
         (or (eq (car-safe object) ',name)
             (and ensure
                  (signal 'wrong-type-argument (list ',predicate-name object)))))
       (cl-deftype ,name ()
         (list 'satisfies (function ,predicate-name)))
       ;; Use `copy-sequence', in case type is mutated afterwards.
       (define-symbol-prop ',name Struct:Type:definition-symbol
                           (copy-sequence ',type))
       (when Struct:enable-syntax-highlighting
         (Struct:-add-syntax-highlighting ',name))
       ',name)))

(defun Struct:-construct-property (declaration)
  (cond
   ((symbolp declaration)
    (Struct:Property :name declaration))
   ((consp declaration)
    (-let* ((((positional &as name documentation)
              property-list)
             (Commons:split-property-list-end declaration))
            (positional-property-list
             (cl-case (length positional)
               (0 nil)
               (1 (list :name name))
               (2 (list :name name :documentation documentation))
               (t (error "Invalid property declaration: %s" declaration)))))
      (apply #'Struct:Property (append positional-property-list
                                       property-list))))
   (t
    (error "Invalid property declaration: %s" declaration))))

(defconst Struct:-doc-predicate
  "Returns `t', if OBJECT is of struct-type `%s'.

Otherwise returns nil, unless ENSURE is non-nil, in which a
`wrong-type-argument' is signaled.")

(defconst Struct:-doc-constructor-first-line
  "Constructs a value of struct-type `%s'.")

(defconst Struct:-doc-constructor-properties-line
  "Struct `%s' has the following properties:")

(defconst Struct:-doc-constructor-syntax-info
  "The macro-version (with a star appended) of this constructor
supports shorthand-syntax, i.e. keyword arguments may be omitted, when
using an identifier eponymous with the property's name. And also
spread-syntax via the `,@' operator, with other struct values as
arguments.

See also `%s'.")

(defconst Struct:-doc-constructor-functions-info
  "The following functions are associated with this struct:")

(defun Struct:-doc-constructor (type)
  (with-output-to-string
    (princ (format Struct:-doc-constructor-first-line
                   (Struct:unsafe-get type :name)))
    (terpri nil t)
    (terpri)
    (when (Struct:unsafe-get type :documentation)
      (princ (Struct:unsafe-get type :documentation))
      (terpri nil t)
      (terpri))
    (princ (format Struct:-doc-constructor-properties-line
                   (Struct:unsafe-get type :name)))
    (terpri nil t)
    (terpri)
    (--each (Struct:unsafe-get type :properties)
      (-let* (((&plist :name :documentation :default-value
                       :mutable :required :type)
               (Struct:unsafe-properties it)))
        (princ (format "- %s" name))
        (when type
          (princ (format " :: %s" type)))
        (princ "	(")
        (when required
          (princ "required, "))
        (when mutable
          (princ "mutable, "))
        (princ (format "default: %s)" default-value))
        (terpri)
        (when documentation
          (princ (string-trim documentation))
          (terpri nil t))
        (terpri)))
    (terpri nil t)
    (princ (format Struct:-doc-constructor-syntax-info
                   (Struct:unsafe-get type :name)))
    (terpri nil t)
    (terpri)
    (princ (Struct:-doc-constructor-signature type))))

(defun Struct:-doc-constructor-signature (type)
  (--> (--map
        (-let* (((&plist :keyword :required :default-value)
                 (Struct:unsafe-properties it))
                (optional (or default-value (not required))))
          (format "%s%s" keyword (if optional "?" "")))
        (Struct:unsafe-get type :properties))
       (mapconcat #'identity it " ")
       (format "\(fn (&plist %s))" it)))

(defun Struct:-doc-predicate (type)
  (with-output-to-string
    (princ (format Struct:-doc-predicate (Struct:unsafe-get type :name)))
    (terpri)))

(defun Struct:-expand-syntax (arguments)
  "Expands shorthand and spread-syntax in ARGUMENTS."
  (let ((expanded-property-list nil))
    (while arguments
      (let ((argument (pop arguments)))
        (cond
         ((and (keywordp argument) arguments)
          ;; Keyword-value pair.
          (push `(list ,argument ,(pop arguments))
                expanded-property-list))
         ((and (eq '\,@ (car-safe argument))
               (= 2 (length argument)))
          ;; Spread syntax.
          (push `(Struct:properties ,(nth 1 argument))
                expanded-property-list))
         ((and (symbolp argument) (not (keywordp argument)))
          ;; Shorthand syntax.
          (push `(list ,(Commons:symbol-to-keyword argument)
                       ,argument)
                expanded-property-list))
         (t
          ;; Probably an error later on.
          (push `(list ,argument)
                expanded-property-list)))))
    `(apply (function append)
            (list ,@(nreverse expanded-property-list)))))

(defun Struct:-add-syntax-highlighting (name)
  (let ((keywords `((,(format "\\_<%s\\*?\\_>"
                              (regexp-quote (symbol-name name)))
                     0 'font-lock-type-face))))
    (put name Struct:syntax-highlight-symbol keywords)
    (font-lock-add-keywords 'emacs-lisp-mode keywords)))

(defun Struct:-remove-syntax-highlighting (name &optional undefine)
  (when-let (keywords (get name Struct:syntax-highlight-symbol))
    (font-lock-remove-keywords 'emacs-lisp-mode keywords)
    (when undefine
      (put name Struct:syntax-highlight-symbol nil))))

;;;###autoload
(defun Struct:undefine (name)
  "Undefine struct NAME.

This function attempts to undo all side-effects of a
corresponding `Struct:define' call.

It does nothing, if NAME does not name a defined struct-type."
  (when (memq name '(Struct:Type Struct:Property))
    (error "Attempted to undefine a builtin struct-type: %s" name))
  (when-let (type (Struct:Type:get name))
    (put name Struct:Type:definition-symbol nil)
    (Struct:-remove-syntax-highlighting name :undefine)
    (fmakunbound name)
    (fmakunbound (intern (concat (symbol-name name) "*")))
    (fmakunbound (intern (concat (symbol-name name) "?")))))

;;;###autoload
(defun Struct:member? (struct property)
  "Returns non-nil, if STRUCT has a keyword-property PROPERTY.

Returned value is actually the `Struct:Property' type of the
given PROPERTY."
  (--find (eq property (Struct:unsafe-get it :keyword))
          (Struct:unsafe-get
           (Struct:Type:get (car struct) :ensure)
           :properties)))

;;;###autoload
(defun Struct:get (struct property &optional default)
  "Returns STRUCT's current value of PROPERTY.

Throws an error if PROPERTY is not a member of STRUCT.

Returns DEFAULT, if value is `nil'."
  (unless (Struct:member? struct property)
    (error "Property is not a member of struct: %s" property))
  (or (Struct:unsafe-get struct property)
      default))

;;;###autoload
(defun Struct:set (struct property value)
  "Sets STRUCT's PROPERTY to VALUE.

Throws an error if
- PROPERTY is not a member of STRUCT, or
- VALUE is `nil' and PROPERTY is required, or
- PROPERTY is not mutable, or
- PROPERTY has an associated type and VALUE does not match it."
  (let ((property-type (Struct:member? struct property))
        (struct-type (Struct:Type:get (car struct) :ensure)))
    (unless property-type
      (error "Property is not a member of struct: %s" property))
    (when (and (null value)
               (Struct:unsafe-get property-type :required))
      (error "Attempted to set required property to `nil': %s" property))
    (unless (and (Struct:unsafe-get struct-type :mutable)
                 (Struct:unsafe-get property-type :mutable))
      (error "Attempted to set immutable property: %s" property))
    (Struct:-check-property-type property-type value))
  (Struct:unsafe-set struct property value))

;;;###autoload
(defun Struct:update (struct property fn)
  (Struct:set struct property
              (funcall fn (Struct:get struct property))))

;;;###autoload
(defmacro Struct:update- (struct property form)
  `(Struct:update ,struct ,property (lambda (it) ,form)))

;;;###autoload
(defun Struct:properties (struct)
  "Returns STRUCT's properties and values as a property-list.

This function returns a new property-list everytime its called."
  (copy-sequence (Struct:unsafe-properties struct)))


(defmacro Struct:lambda (arguments &rest body)
  "Defines a function with some struct related features."
  (declare (indent 1))
  (Struct:lambda-check-arguments arguments)

  `(lambda ,(Struct:lambda-normalize-arguments arguments)
     ,@(Struct:-lambda-emit-type-checks arguments)
     ,(Struct:-lambda-emit-handle-struct-rest arguments)
     ,@body))

(defmacro Struct:defun (name arguments
                             &optional documentation declare
                             &rest body)
  "Defines a function NAME with some struct related features."
  (declare (indent defun) (doc-string 3))
  (unless (or (stringp documentation)
              (null documentation))
    (push documentation body)
    (setq documentation nil))
  (unless (or (eq (car-safe declare) 'declare)
              (null declare))
    (push declare body)
    (setq declare nil))
  (Struct:lambda-check-arguments arguments)

  `(defun ,name ,(Struct:lambda-normalize-arguments arguments)
     ,(format "%s\n\n%s" (or documentation "") (cons 'fn arguments))
     ,declare
     ,@(Struct:-lambda-emit-type-checks arguments)
     ,(Struct:-lambda-emit-handle-struct-rest arguments)
     ,@body))

(defun Struct:lambda-check-arguments (arguments)
  "Check that arguments can be used with `Struct:lambda'.

Throws an error, if ARGUMENTS are incompatible with that macro."
  (when (and (memq '&rest arguments)
             (memq '&struct arguments))
    (error "&rest and &struct are mutually exclusive: %s" arguments))
  (when-let (struct-rest (memq '&struct arguments))
    (pcase (length struct-rest)
      (1 (error "&struct is missing an argument: %s" arguments))
      (2 nil)
      (_ (error "&struct argument must be last: %s" arguments)))
    (let ((argument (cadr struct-rest)))
      (unless (and (consp argument)
                   (= (length argument))
                   (-every? #'symbolp argument))
        (error "&struct argument should specify a struct-type: %s"
               arguments))))
  (when-let (rest-rest (memq '&rest arguments))
    (pcase (length rest-rest)
      (1 (error "&rest is missing an argument: %s" arguments))
      (2 nil)
      (_ (error "&rest argument must be last: %s" arguments)))
    (let ((argument (cadr rest-rest)))
      (when (consp argument)
        (error "Providing a type for &rest arguments is not supported: %s"
               arguments))))
  (--each arguments
    (unless (or (symbolp it)
                (and (consp it)
                     (= (length it) 2)))
      (error "Argument should be a symbol or have the form (argument type): %s"
             arguments))))

(defun Struct:lambda-normalize-arguments (arguments)
  (--map (cond
          ((eq it '&struct) '&rest)
          ((symbolp it) it)
          ((consp it) (car it))
          (t (error "Invalid argument: %s" it)))
         arguments))

(defun Struct:-lambda-emit-type-checks (arguments)
  (when (or (memq '&struct arguments)
            (memq '&rest arguments))
    (setq arguments (butlast arguments 2)))
  (-let (((non-optional (_ . optional))
          (--split-with (not (eq '&optional it)) arguments)))
    (append
     (--map `(cl-check-type ,(car it) ,(cadr it))
            (-filter #'consp non-optional))
     (--map `(cl-check-type ,(car it) (or null ,(cadr it)))
            (-filter #'consp optional)))))

(defun Struct:-lambda-emit-handle-struct-rest (arguments)
  (-when-let ((struct type)
              (cadr (memq '&struct arguments)))
    `(setq ,struct (Struct:-lambda-handle-struct-rest ',type ,struct))))

(defun Struct:-lambda-handle-struct-rest(type rest)
  (if (and (consp rest)
           (= 1 (length rest))
           (eq type (car-safe (car rest))))
      (car rest)
    (Struct:Type:get type :ensure)
    (apply type rest)))

(define-symbol-prop 'Struct:Type 'function-documentation
     (Struct:-doc-constructor (Struct:Type:get 'Struct:Type :ensure)))

(define-symbol-prop 'Struct:Property 'function-documentation
     (Struct:-doc-constructor (Struct:Type:get 'Struct:Property :ensure)))

(provide 'Struct)
;;; Struct.el ends here
