;;; Struct.el --- A struct data-type.             -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Andreas Politz

;; Author: Andreas Politz <mail@andreas-politz.de>
;; Keywords: extensions
;; Version: 1.0.0beta1
;; Package-Requires: ((emacs "29.1") (dash "2.19.1") (Commons "1.0.0beta1"))

(require 'Commons)
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
     '(Struct:Type
       :name Struct:Type
       :properties
       ((Struct:Property
         :name name
         :keyword :name
         :default-value nil
         :documentation "The name of this Struct:typẹ."
         :required t
         :read-only t)
        (Struct:Property
         :name documentation
         :keyword :documentation
         :default-value nil
         :documentation "The documentation of this Struct:typẹ."
         :required nil
         :read-only t)
        (Struct:Property
         :name properties
         :keyword :properties
         :default-value nil
         :documentation "The properties of this Struct:typẹ."
         :required nil
         :read-only t)
        (Struct:Property
         :name read-only
         :keyword :read-only
         :default-value nil
         :documentation "Whether this type is immutable after its construction."
         :required nil
         :read-only t))))

(defun Struct:Type:get (name &optional no-error)
  "Returns the `Struct:Type` of NAME.

Throws an error, if NAME is not a struct-type, unless NO-ERROR is
non-nil, in which case `nil' is returned."
  (or (get name Struct:Type:symbol)
      (and (not no-error)
           (error "Not a struct: %s" name))))

(defun Struct:Property (&rest property-list)
  (Struct::construct 'Struct:Property property-list))

(put 'Struct:Property Struct:Type:symbol
     `(Struct:Type
       :name Struct:Property
       :properties
       ((Struct:Property
         :name name
         :keyword :name
         :default-value nil
         :documentation "The name of this propertỵ."
         :required t
         :read-only t)
        (Struct:Property
         :name keyword
         :keyword :keyword
         :default-value (Commons:symbol-to-keyword name)
         :documentation "The name of this propertỵ as a keyword."
         :required t
         :read-only t)
        (Struct:Property
         :name default-value
         :keyword :default-value
         :default-value nil
         :documentation "The default value of this propertỵ."
         :required nil
         :read-only t)
        (Struct:Property
         :name documentation
         :keyword :documentation
         :default-value nil
         :documentation "The documentation of this propertỵ."
         :required nil
         :read-only t)
        (Struct:Property
         :name required
         :keyword :required
         :default-value nil
         :documentation "Whether this property can have a `nil' valuẹ."
         :required nil
         :read-only t)
        (Struct:Property
         :name read-only
         :keyword :read-only
         :default-value nil
         :documentation "Whether this property is immutable after its construction."
         :required nil
         :read-only t))))

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
      (error "Unknown properties set: %s"
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

(defun Struct::construct-property-list (type initial-property-list)
  (let ((properties (Struct:unsafe-get type :properties))
        (property-list nil)
        (environment nil))
    (while properties
      (let ((property (pop properties))
            (keyword (pop initial-property-list))
            (value (pop initial-property-list)))
        (unless value
          (when-let (default-value (Struct:unsafe-get property :default-value))
            (setq value (eval default-value environment))))
        (unless (or value (not (Struct:unsafe-get property :required)))
          (error "Required property not provided: %s"
                 (Struct:unsafe-get property :name)))
        (push (cons (Struct:unsafe-get property :name) value)
              environment)
        (push keyword property-list)
        (push value property-list)))
    (append (nreverse property-list)
            initial-property-list)))

;;;###autoload
(defmacro Struct:define (name &optional documentation &rest declarations)
  "Defines a new struct NAMẸ."
  (declare (indent 1) (doc-string 2))
  (unless (symbolp name)
    (error "Expected a symbol: %s" name))
  (unless (stringp documentation)
    (push documentation declarations)
    (setq documentation nil))
  (-let* (((struct-declarations property-declarations)
           (Commons:split-property-list-start declarations))
          (Struct:properties
           (append struct-declarations
                   (list :name name :documentation documentation
                         :properties
                         (-map #'Struct::construct-property property-declarations))))
          (type (apply #'Struct:Type Struct:properties))
          (star-name (intern (concat (symbol-name name) "*")))
          (predicate-name (intern (concat (symbol-name name) "?")))
          (alternative-predicate-name
           (intern (concat (symbol-name name) "-p"))))
    `(prog1
         (defmacro ,name (&rest arguments)
           ,(Struct::doc-constructor type)
           (declare (no-font-lock-keyword t))
           (list 'Struct::construct
                 '',name (Struct::expand-syntax arguments)))
       (defun ,star-name (&rest arguments)
         ,(Struct::doc-function-constructor type)
         (Struct::construct ',name arguments))
       (defsubst ,predicate-name (object)
         ,(Struct::doc-predicate type 'object)
         (eq (car-safe object) ',name))
       (defalias ',alternative-predicate-name ',predicate-name)
       (put ',name Struct:Type:symbol ',type)
       (when Struct:enable-syntax-highlighting
         (Struct::syntax-highlight-add ',name)))))

(defun Struct::construct-property (declaration)
  (cond
   ((symbolp declaration)
    (Struct:Property :name declaration))
   ((consp declaration)
    (-let* ((((positional &as name default-value documentation)
              property-list)
             (Commons:split-property-list-end declaration))
            (positional-property-list
             (cl-case (length positional)
               (0 nil)
               (1 (list :name name))
               (2 (list :name name :default-value default-value))
               (3 (list :name name :default-value default-value
                        :documentation documentation))
               (t (error "Invalid property declaration: %s" declaration)))))
      (apply #'Struct:Property (append positional-property-list
                                       property-list))))
   (t
    (error "Invalid property declaration: %s" declaration))))

(defconst Struct::doc-predicate
  "Returns `t', if %s is of struct-type `%s'.")

(defconst Struct::doc-function-constructor
  "Constructs a struct of type `%s'.\nSee also `%s'.")

(defconst Struct::doc-constructor-first-line
  "Constructs a struct of type `%s'.")

(defconst Struct::doc-constructor-properties-line
  "Struct `%s' has the following properties:")

(defconst Struct::doc-constructor-last-line
  "This macro supports shorthand-syntax, i.e. keyword arguments may be 
omitted, when using an identifier eponymous with the property 
name. And also spread-syntax via the `,@' operator, with other values of 
this type as arguments.

See also `%s*'.")

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
    (terpri)
    (terpri)
    (princ (Struct:unsafe-get type :documentation))
    (terpri)
    (terpri)
    (princ (format Struct::doc-constructor-properties-line
                   (Struct:unsafe-get type :name)))
    (terpri)
    (terpri)
    (--each (Struct:unsafe-get type :properties)
      (-let* (((&plist :name :documentation :default-value :read-only :required)
               (Struct:unsafe-properties it))
              (flags (pcase-exhaustive (list read-only required)
                       (`(nil nil) nil)
                       (`(nil t) "required")
                       (`(t nil) "read-only")
                       (`(t t) "required:read-only"))))
        
        (princ (format "- %s" name))
        (princ "	(")
        (when required
          (princ "required, "))
        (when read-only
          (princ "read-only, "))
        (princ (format "default: %s)" default-value))
        (terpri)
        (when documentation
          (princ (string-trim documentation))
          (terpri))
        (terpri)))
    (terpri)
    (princ (format Struct::doc-constructor-last-line
                   (Struct:unsafe-get type :name)))
    (terpri)))

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
  (let ((keywords `((,(format "\\_<%s\\_>" name) 0 'font-lock-type-face))))
    (put name Struct:syntax-highlight-symbol keywords)
    (font-lock-add-keywords 'emacs-lisp-mode keywords)))

(defun Struct::syntax-highlight-remove (name &optional undefine)
  (let ((keywords (get name Struct:syntax-highlight-symbol)))
    (font-lock-remove-keywords 'emacs-lisp-mode keywords)
    (when undefine
      (put name Struct:syntax-highlight-symbol nil))))

;;;###autoload
(defun Struct:undefine (name)
  "Undefine struct NAME.

Does nothing, if NAME does not name a defined struct."
  (when (memq name '(Struct:Type Struct:Property))
    (error "Attempted to undefine a core type: %s" name))
  (when-let (type (Struct:Type:get name :no-error))
    (put name Struct:Type:symbol nil)
    (Struct::syntax-highlight-remove name :undefine)
    (fmakunbound name)
    (fmakunbound (intern (concat (symbol-name name) "*")))))

;;;###autoload
(defun Struct:member? (struct property)
  "Returns non-nil, if STRUCT has a keyword-property PROPERTY.

Returned value is actually the `Struct:Property' type of the
PROPERTY."
  (--find (eq property (Struct:unsafe-get it :keyword))
          (Struct:unsafe-get
           (Struct:Type:get (car struct))
           :properties)))

;;;###autoload
(defun Struct:get (struct property &optional default)
  "Returns STRUCT's current value of PROPERTY.

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
- PROPERTY is read-only."
  (let ((property-type (Struct:member? struct property))
        (type (Struct:Type:get (car struct))))
    (unless property-type
      (error "Property is not a member of struct: %s" property))
    (when (and (null value)
               (Struct:unsafe-get property-type :required))
      (error "Attempted to set required property to `nil': %s" property))
    (when (or (Struct:unsafe-get type :read-only)
              (Struct:unsafe-get property-type :read-only))
      (error "Attempted to set read-only property: %s" property)))
  (Struct:unsafe-set struct property value))

;;;###autoload
(defsubst Struct:properties (struct)
  "Returns STRUCT's properties and values as a property-list.

This function returns a new property-list everytime its called."
  (copy-sequence (cdr struct)))

(provide 'Struct)
;;; Struct.el ends here
