;; -*- lexical-binding: t -*-

(require 'Lang/Commons)
(eval-and-compile (require 'dash))

(defconst Struct:type-symbol 'Struct:Type
  "Symbol used to attach struct type-information to other symbols.")

(defconst Struct:syntax-highlight-symbol 'Struct:syntax-highlight
  "Symbol to attach font-lock information to other symbols.")

(defvar Struct:enable-syntax-highlighting t
  "Whether to highlight struct types with face `font-lock-type-face'.")

(defun Struct:get-type (name &optional no-error)
  (or (get name Struct:type-symbol)
      (and (not no-error)
           (error "Not a struct: %s" name))))

(defun Struct:Type (&rest property-list)
  (Struct:construct 'Struct:Type property-list))

(put 'Struct:Type Struct:type-symbol
     '(Struct:Type
       :name Struct:Type
       :properties
       ((Struct:Property
         :name name
         :keyword :name
         :default-value nil
         :documentation "The name of this struct-typẹ."
         :required t
         :read-only t)
        (Struct:Property
         :name documentation
         :keyword :documentation
         :default-value nil
         :documentation "The documentation of this struct-typẹ."
         :required nil
         :read-only t)
        (Struct:Property
         :name properties
         :keyword :properties
         :default-value nil
         :documentation "The properties of this struct-typẹ."
         :required nil
         :read-only t)
        (Struct:Property
         :name read-only
         :keyword :read-only
         :default-value nil
         :documentation "Whether this type is immutable after its construction."
         :required nil
         :read-only t))))

(defun Struct:Property (&rest property-list)
  (Struct:construct 'Struct:Property property-list))

(put 'Struct:Property Struct:type-symbol
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

(defun Struct:construct (name arguments)
  (let ((type (Struct:get-type name)))
    (cons name (->> arguments
                    (Struct:initial-property-list type)
                    (Struct:construct-property-list type)))))

(defun Struct:initial-property-list (type arguments)
  (let* ((property-list (Struct:empty-property-list type))
         (property-count (/ (length property-list) 2)))
    (while arguments
      (let ((keyword (pop arguments))
            (value (pop arguments)))
        (setq property-list (plist-put property-list keyword value))))
    (unless (= property-count (/ (length property-list) 2))
      (error "Unknown properties set: %s"
             (nthcdr (* 2 property-count) property-list)))
    property-list))

(defun Struct:property-keywords (properties)
  (--map (Commons:symbol-to-keyword it)
         (--map (Struct:unsafe-get it :name)
                properties)))

(defun Struct:empty-property-list (type)
  (--splice (prog1 t (ignore it))
            (list it nil)
            (Struct:property-keywords
             (Struct:unsafe-get type :properties))))

(defun Struct:construct-property-list (type initial-property-list)
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
  "Define a new struct NAMẸ."
  (declare (indent 1) (doc-string 2))
  (unless (symbolp name)
    (error "Expected a symbol: %s" name))
  (unless (stringp documentation)
    (push documentation declarations)
    (setq documentation nil))
  (-let* (((struct-declarations property-declarations)
           (Commons:split-property-list-start declarations))
          (struct-properties
           (append struct-declarations
                   (list :name name :documentation documentation
                         :properties
                         (-map #'Struct:construct-property property-declarations))))
          (type (apply #'Struct:Type struct-properties))
          (star-name (intern (concat (symbol-name name) "*"))))
    `(prog1
         (defmacro ,name (&rest arguments)
           (declare (no-font-lock-keyword t))
           (list 'Struct:construct
                 '',name (Struct:expand-syntax arguments)))
       (defun ,star-name (&rest arguments)
         (Struct:construct ',name arguments))
       (put ',name Struct:type-symbol ',type)
       (when Struct:enable-syntax-highlighting
         (Struct:syntax-highlight-add ',name)))))

(defun Struct:construct-property (declaration)
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

(defun Struct:expand-syntax (arguments)
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

(defun Struct:syntax-highlight-add (name)
  (let ((keywords `((,(format "\\_<%s\\_>" name) 0 'font-lock-type-face))))
    (put name Struct:syntax-highlight-symbol keywords)
    (font-lock-add-keywords 'emacs-lisp-mode keywords)))

(defun Struct:syntax-highlight-remove (name &optional undefine)
  (let ((keywords (get name Struct:syntax-highlight-symbol)))
    (font-lock-remove-keywords 'emacs-lisp-mode keywords)
    (when undefine
      (put name Struct:syntax-highlight-symbol nil))))

(defun Struct:undefine (name)
  (when (memq name '(Struct:Type Struct:Property))
    (error "Attempted to undefine a core type: %s" name))
  (when-let (type (Struct:get-type name :no-error))
    (put name Struct:type-symbol nil)
    (Struct:syntax-highlight-remove name :undefine)
    (fmakunbound name)
    (fmakunbound (intern (concat (symbol-name name) "*")))))

;;;###autoload
(defun Struct:member? (struct property)
  (--find (eq property (Struct:unsafe-get it :keyword))
          (Struct:unsafe-get
           (Struct:get-type (car struct))
           :properties)))

;;;###autoload
(defsubst Struct:unsafe-get (struct property)
  (plist-get (cdr struct) property))

;;;###autoload
(defun Struct:get (struct property)
  (unless (Struct:member? struct property)
    (error "Property is not a member of struct: %s" property))
  (Struct:unsafe-get struct property))

;;;###autoload
(defsubst Struct:unsafe-set (struct property value)
  (setcdr struct
          (plist-put (cdr struct) property value))
  value)

;;;###autoload
(defun Struct:set (struct property value)
  (let ((struct-property (Struct:member? struct property))
        (type (Struct:get-type (car struct))))
    (unless struct-property
      (error "Property is not a member of struct: %s"))
    (when (and (null value)
               (Struct:unsafe-get struct-property :required))
      (error "Attempted to set required property to `nil': %s" property))
    (when (or (Struct:unsafe-get type :read-only)
              (Struct:unsafe-get struct-property :read-only))
      (error "Attempted to set read-only property: %s" property)))
  (Struct:unsafe-set struct property value))

;;;###autoload
(defsubst Struct:properties (struct)
  (copy-sequence (cdr struct)))

;;;###autoload
(defsubst Struct:unsafe-properties (struct)
  (cdr struct))

(provide 'Lang/Struct)
