;;; Struct.el --- A struct data-type.             -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Andreas Politz

;; Author: Andreas Politz <mail@andreas-politz.de>
;; Keywords: extensions
;; Version: 1.0.0beta1
;; Package-Requires: ((emacs "29.1") (dash "2.19.1") (Commons "1.0.0beta1"))

(require 'Commons)
(require 'cl-macs)
(eval-and-compile (require 'dash))

(defconst Struct:Type:symbol 'Struct:Type
  "Symbol used to attach struct type-information to other symbols.")

(defconst Struct:syntax-highlight-symbol 'Struct:syntax-highlight
  "Symbol to attach font-lock information to other symbols.")

(defvar Struct:enable-syntax-highlighting t
  "Whether to highlight struct types with face `font-lock-type-face'.")

;; These `defsubst' definition need to be defined before they are
;; used, or else the compiler will complain.
;;;###autoload
(defsubst Struct:unsafe-get (struct property &optional default)
  "Returns STRUCT's current value of PROPERTY.

Returns DEFAULT if value is nil."
  (or (plist-get (cdr struct) property)
      default))

;;;###autoload
(defsubst Struct:unsafe-set (struct property value)
  "Sets STRUCT's PROPERTY to VALUE."
  (setcdr struct
          (plist-put (cdr struct) property value))
  value)

;;;###autoload
(defsubst Struct:unsafe-properties (struct)
  "Returns STRUCT's properties."
  (cdr struct))

(defun Struct:Type (&rest property-list)
  "Constructs a new struct-type"
  (Struct::construct 'Struct:Type property-list))

(put 'Struct:Type Struct:Type:symbol
     '(Struct:MetaType
       :name Struct:Type
       :properties
       ((Struct:Property
         :name name
         :keyword :name
         :default-value nil
         :documentation "The name of this struct-type."
         :required t
         :read-only t
         :type symbol)
        (Struct:Property
         :name documentation
         :keyword :documentation
         :default-value nil
         :documentation "The documentation of this struct-type."
         :required nil
         :read-only t
         :type (or null string))
        (Struct:Property
         :name properties
         :keyword :properties
         :default-value nil
         :documentation "The properties of this struct-type."
         :required nil
         :read-only t
         :type list)
        (Struct:Property
         :name read-only
         :keyword :read-only
         :default-value nil
         :documentation "Whether this type is immutable after its construction."
         :required nil
         :read-only t
         :type boolean)
        (Struct:Property
         :name methods
         :keyword :methods
         :default-value nil
         :documentation "A list of method-names associated with this struct-type."
         :required nil
         :read-only nil
         :type list)
        (Struct:Property
         :name disable-type-checks
         :keyword :disable-type-checks
         :default-value nil
         :documentation "Wether checking property-types should be disabled."
         :required nil
         :read-only t
         :type boolean))))

(defun Struct:Type:get (name &optional no-error)
  "Returns the `Struct:Type' of NAME.

Signals a `wrong-type-argument', if NAME is not a
struct-type. Unless NO-ERROR is non-nil, in which case `nil' is
returned."
  (or (get name Struct:Type:symbol)
      (and (not no-error)
           (signal 'wrong-type-argument (list 'Struct:Type name)))))

(defun Struct:Type? (name-or-type-struct)
  "Return `t', if NAME-OR-TYPE-STRUCT is a struct-type.

NAME-OR-TYPE-STRUCT can be either a symbol naming a struct-type or
the type itself."
  (let ((type-struct (if (symbolp name-or-type-struct)
                         (Struct:Type:get name-or-type-struct :no-error)
                       name-or-type-struct)))
    (eq 'Struct:Type (car-safe type-struct))))

(defun Struct:Property (&rest property-list)
  (Struct::construct 'Struct:Property property-list))

(put 'Struct:Property Struct:Type:symbol
     `(Struct:MetaType
       :name Struct:Property
       :properties
       ((Struct:Property
         :name name
         :keyword :name
         :default-value nil
         :documentation "The name of this propertỵ."
         :required t
         :read-only t
         :type symbol)
        (Struct:Property
         :name keyword
         :keyword :keyword
         :default-value (Commons:symbol-to-keyword name)
         :documentation "The name of this propertỵ as a keyword."
         :required t
         :read-only t
         :type keyword)
        (Struct:Property
         :name default-value
         :keyword :default-value
         :default-value nil
         :documentation "The default value of this propertỵ."
         :required nil
         :read-only t
         :type nil)
        (Struct:Property
         :name documentation
         :keyword :documentation
         :default-value nil
         :documentation "The documentation of this propertỵ."
         :required nil
         :read-only t
         :type (or null string))
        (Struct:Property
         :name required
         :keyword :required
         :default-value nil
         :documentation "Whether this property can have a `nil' valuẹ."
         :required nil
         :read-only t
         :type boolean)
        (Struct:Property
         :name read-only
         :keyword :read-only
         :default-value nil
         :documentation "Whether this property is immutable after its construction."
         :required nil
         :read-only t
         :type boolean)
        (Struct:Property
         :name type
         :keyword :type
         :default-value nil
         :documentation "The type of this property."
         :required nil
         :read-only t
         :type nil))))

(defun Struct::construct (name arguments)
  (let ((type (Struct:Type:get name)))
    (cons name (->> arguments
                    (Struct::initial-property-list type)
                    (Struct::construct-property-list type)))))

(defun Struct::initial-property-list (type arguments)
  (let* ((property-list (Struct::empty-property-list type))
         (property-count (/ (length property-list) 2)))
    (while arguments
      (let ((keyword (pop arguments))
            (value (pop arguments)))
        (setq property-list (plist-put property-list keyword value))))
    (unless (= property-count (/ (length property-list) 2))
      (error "Undeclared properties set: %s"
             (nthcdr (* 2 property-count) property-list)))
    property-list))

(defun Struct::property-keywords (properties)
  (--map (Commons:symbol-to-keyword it)
         (--map (Struct:unsafe-get it :name)
                properties)))

(defun Struct::empty-property-list (type)
  (--splice (prog1 t (ignore it))
            (list it nil)
            (Struct::property-keywords
             (Struct:unsafe-get type :properties))))

(defun Struct::construct-property-list (type property-list)
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
                (Struct::check-type type property value))
        (setq property-list-head (nthcdr 2 property-list-head))))
    property-list))

(defun Struct::check-type (struct-type property-type value)
  (unless (Struct:unsafe-get struct-type :disable-type-checks)
    (when-let (type (Struct:unsafe-get property-type :type))
      (or (cl-typep value type)
          (signal 'wrong-type-argument (list type value)))))
  value)

;;;###autoload
(defmacro Struct:define (name &optional documentation &rest declarations)
  "Defines a new struct NAMẸ."
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
                         (-map #'Struct::construct-property
                               property-declarations))))
          (type (apply #'Struct:Type struct-property-list))
          (starred-name (intern (concat (symbol-name name) "*")))
          (predicate-name (intern (concat (symbol-name name) "?")))
          (alternative-predicate-name
           (intern (concat (symbol-name name) "-p"))))
    `(prog1
         (defmacro ,name (&rest property-list)
           ,(Struct::doc-constructor type)
           (declare (no-font-lock-keyword t)
                    (debug t))
           (list 'Struct::construct
                 '',name (Struct::expand-syntax property-list)))
       (defun ,starred-name (&rest property-list)
         ,(Struct::doc-function-constructor type)
         (Struct::construct ',name property-list))
       (defsubst ,predicate-name (object)
         ,(Struct::doc-predicate type 'object)
         (eq (car-safe object) ',name))
       (defalias ',alternative-predicate-name ',predicate-name)
       ;; Use `copy-sequence', in case type is mutated afterwards.
       (put ',name Struct:Type:symbol (copy-sequence ',type))
       (when Struct:enable-syntax-highlighting
         (Struct::syntax-highlight-add ',name))
       ',name)))

(defun Struct::construct-property (declaration)
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

(defconst Struct::doc-predicate
  "Returns `t', if %s is of struct-type `%s'.")

(defconst Struct::doc-function-constructor
  "Constructs a value of struct-type `%s'.\n\nSee also `%s'.")

(defconst Struct::doc-constructor-first-line
  "Constructs a value of struct-type `%s'.")

(defconst Struct::doc-constructor-properties-line
  "Struct `%s' has the following properties:")

(defconst Struct::doc-constructor-syntax-info
  "This macro supports shorthand-syntax, i.e. keyword arguments may be 
omitted, when using an identifier eponymous with the property's 
name. And also spread-syntax via the `,@' operator, with other struct
values as arguments.

See also `%s*'.")

(defconst Struct::doc-constructor-functions-info
  "The following functions are associated with this struct:")

(defconst Struct::doc-constructor-methods-info
  "The following methods can be used with this struct:")

(defun Struct::doc-function-constructor (type)
  (with-output-to-string
    (princ (format Struct::doc-function-constructor
                   (Struct:unsafe-get type :name)
                   (format "%s*" (Struct:unsafe-get type :name))))
    (terpri)))

(defun Struct::doc-constructor (type)
  (with-output-to-string
    (princ (format Struct::doc-constructor-first-line
                   (Struct:unsafe-get type :name)))
    (terpri nil t)
    (terpri)
    (when (Struct:unsafe-get type :documentation)
      (princ (Struct:unsafe-get type :documentation))
      (terpri nil t)
      (terpri))
    (princ (format Struct::doc-constructor-properties-line
                   (Struct:unsafe-get type :name)))
    (terpri nil t)
    (terpri)
    (--each (Struct:unsafe-get type :properties)
      (-let* (((&plist :name :documentation :default-value
                       :read-only :required :type)
               (Struct:unsafe-properties it)))
        (princ (format "- %s" name))
        (when type
          (princ (format " :: %s" type)))
        (princ "	(")
        (when required
          (princ "required, "))
        (when read-only
          (princ "read-only, "))
        (princ (format "default: %s)" default-value))
        (terpri)
        (when documentation
          (princ (string-trim documentation))
          (terpri nil t))
        (terpri)))
    (unless (memq (Struct:unsafe-get type :name)
                  '(Struct:Type Struct:Property))
      (terpri)
      (princ (format Struct::doc-constructor-syntax-info
                     (Struct:unsafe-get type :name))))
    (terpri nil t)
    (terpri)
    (princ (Struct::doc-constructor-signature type))))

(defun Struct::doc-constructor-signature (type)
  (--> (--map
        (-let* (((&plist :keyword :required :default-value)
                 (Struct:unsafe-properties it))
                (optional (or default-value (not required))))
          (format "%s%s" keyword (if optional "?" "")))
        (Struct:unsafe-get type :properties))
       (mapconcat #'identity it " ")
       (format "\(fn (&plist %s))" it)))

(defun Struct::doc-predicate (type argument-name)
  (with-output-to-string
    (princ (format Struct::doc-predicate
                   (upcase (symbol-name argument-name))
                   (Struct:unsafe-get type :name)))
    (terpri)))

(defun Struct::expand-syntax (arguments)
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

(defun Struct::syntax-highlight-add (name)
  (let ((keywords `((,(format "\\_<%s\\*?\\_>" name)
                     0 'font-lock-type-face))))
    (put name Struct:syntax-highlight-symbol keywords)
    (font-lock-add-keywords 'emacs-lisp-mode keywords)))

(defun Struct::syntax-highlight-remove (name &optional undefine)
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
  (when-let (type (Struct:Type:get name :no-error))
    (put name Struct:Type:symbol nil)
    (Struct::syntax-highlight-remove name :undefine)
    (fmakunbound name)
    (fmakunbound (intern (concat (symbol-name name) "*")))
    (fmakunbound (intern (concat (symbol-name name) "?")))
    (fmakunbound (intern (concat (symbol-name name) "-p")))))

;;;###autoload
(defun Struct:member? (struct property)
  "Returns non-nil, if STRUCT has a keyword-property PROPERTY.

Returned value is actually the `Struct:Property' type of the
given PROPERTY."
  (--find (eq property (Struct:unsafe-get it :keyword))
          (Struct:unsafe-get
           (Struct:Type:get (car struct))
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
- PROPERTY is read-only, or
- PROPERTY has an associated type and VALUE does not match it."
  (let ((property-type (Struct:member? struct property))
        (struct-type (Struct:Type:get (car struct))))
    (unless property-type
      (error "Property is not a member of struct: %s" property))
    (when (and (null value)
               (Struct:unsafe-get property-type :required))
      (error "Attempted to set required property to `nil': %s" property))
    (when (or (Struct:unsafe-get struct-type :read-only)
              (Struct:unsafe-get property-type :read-only))
      (error "Attempted to set read-only property: %s" property))
    (Struct::check-type struct-type property-type value))
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

(put 'Struct:Type 'function-documentation
     (Struct::doc-constructor (Struct:Type:get 'Struct:Type)))

(put 'Struct:Property 'function-documentation
     (Struct::doc-constructor (Struct:Type:get 'Struct:Property)))

(defmacro Struct:defmethod (name arguments &rest body)
  "Defines a method NAME for a struct-type.

A method always accepts at least one argument, which must be
non-nil and of type STRUCT-NAME.  This condition is asserted by
adding a corresponding test to the beginning of BODY.

This adds NAME to the method property of the type-definition of
the struct and updates the documentation of its type-constructor,
such that it will contain a reference to this method.

Otherwise this behaves like `defun', which see."
  (declare (indent defun))
  (let* ((struct-type (make-symbol "struct-type"))
         (documentation (if (stringp (car-safe body)) (pop body)))
         (declare (if (eq 'declare (car-safe (car-safe body))) (pop body)))
         (self-argument (car-safe arguments))
         (struct-name (car-safe (cdr-safe self-argument)))
         (type-predicate (intern (format "%s?" struct-name)))
         (type-predicate-form
          `(or (,type-predicate ,self-argument)
               (signal 'wrong-type-argument (list ',struct-name ,self-argument))))
         (body (cons type-predicate-form body)))
    (unless (and self-argument
                 (symbolp self-argument)
                 (not (memq self-argument '(&optional &rest))))
      (error "A method must have at least one non-optional self-argument"))
    (unless (and struct-name
                 (symbolp struct-name))
      (error "First argument must have the form (self Type)"))
    `(let ((,struct-type (Struct:Type:get ',struct-name)))
       (defun ,name ,arguments ,documentation ,declare ,@body)
       (unless (memq ',name (Struct:get ,struct-type :methods))
         (Struct:update- ,struct-type :methods (push ',name it))))))

(provide 'Struct)
;;; Struct.el ends here
