;;; struct.el --- A struct data-type.             -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Andreas Politz

;; Author: Andreas Politz <mail@andreas-politz.de>
;; Keywords: extensions
;; Version: 1.0.0beta1
;; Package-Requires: ((emacs "29.1") (dash "2.19.1") (commons))

(require 'commons)
(eval-and-compile (require 'dash))

(defconst struct-type-symbol 'struct-Type
  "Symbol used to attach struct type-information to other symbols.")

(defconst struct-syntax-highlight-symbol 'struct-syntax-highlight
  "Symbol to attach font-lock information to other symbols.")

(defvar struct-enable-syntax-highlighting t
  "Whether to highlight struct types with face `font-lock-type-face'.")

(defun struct-get-type (name &optional no-error)
  (or (get name struct-type-symbol)
      (and (not no-error)
           (error "Not a struct: %s" name))))

;;;###autoload
(defsubst struct-unsafe-get (struct property)
  (plist-get (cdr struct) property))

;;;###autoload
(defsubst struct-unsafe-set (struct property value)
  (setcdr struct
          (plist-put (cdr struct) property value))
  value)

;;;###autoload
(defsubst struct-properties (struct)
  (copy-sequence (cdr struct)))

;;;###autoload
(defsubst struct-unsafe-properties (struct)
  (cdr struct))

(defun struct-type (&rest property-list)
  (struct-construct 'struct-type property-list))

(put 'struct-type struct-type-symbol
     '(struct-type
       :name struct-type
       :properties
       ((struct-property
         :name name
         :keyword :name
         :default-value nil
         :documentation "The name of this struct-typẹ."
         :required t
         :read-only t)
        (struct-property
         :name documentation
         :keyword :documentation
         :default-value nil
         :documentation "The documentation of this struct-typẹ."
         :required nil
         :read-only t)
        (struct-property
         :name properties
         :keyword :properties
         :default-value nil
         :documentation "The properties of this struct-typẹ."
         :required nil
         :read-only t)
        (struct-property
         :name read-only
         :keyword :read-only
         :default-value nil
         :documentation "Whether this type is immutable after its construction."
         :required nil
         :read-only t))))

(defun struct-property (&rest property-list)
  (struct-construct 'struct-property property-list))

(put 'struct-property struct-type-symbol
     `(struct-type
       :name struct-property
       :properties
       ((struct-property
         :name name
         :keyword :name
         :default-value nil
         :documentation "The name of this propertỵ."
         :required t
         :read-only t)
        (struct-property
         :name keyword
         :keyword :keyword
         :default-value (commons-symbol-to-keyword name)
         :documentation "The name of this propertỵ as a keyword."
         :required t
         :read-only t)
        (struct-property
         :name default-value
         :keyword :default-value
         :default-value nil
         :documentation "The default value of this propertỵ."
         :required nil
         :read-only t)
        (struct-property
         :name documentation
         :keyword :documentation
         :default-value nil
         :documentation "The documentation of this propertỵ."
         :required nil
         :read-only t)
        (struct-property
         :name required
         :keyword :required
         :default-value nil
         :documentation "Whether this property can have a `nil' valuẹ."
         :required nil
         :read-only t)
        (struct-property
         :name read-only
         :keyword :read-only
         :default-value nil
         :documentation "Whether this property is immutable after its construction."
         :required nil
         :read-only t))))

(defun struct-construct (name arguments)
  (let ((type (struct-get-type name)))
    (cons name (->> arguments
                    (struct-initial-property-list type)
                    (struct-construct-property-list type)))))

(defun struct-initial-property-list (type arguments)
  (let* ((property-list (struct-empty-property-list type))
         (property-count (/ (length property-list) 2)))
    (while arguments
      (let ((keyword (pop arguments))
            (value (pop arguments)))
        (setq property-list (plist-put property-list keyword value))))
    (unless (= property-count (/ (length property-list) 2))
      (error "Unknown properties set: %s"
             (nthcdr (* 2 property-count) property-list)))
    property-list))

(defun struct-property-keywords (properties)
  (--map (commons-symbol-to-keyword it)
         (--map (struct-unsafe-get it :name)
                properties)))

(defun struct-empty-property-list (type)
  (--splice (prog1 t (ignore it))
            (list it nil)
            (struct-property-keywords
             (struct-unsafe-get type :properties))))

(defun struct-construct-property-list (type initial-property-list)
  (let ((properties (struct-unsafe-get type :properties))
        (property-list nil)
        (environment nil))
    (while properties
      (let ((property (pop properties))
            (keyword (pop initial-property-list))
            (value (pop initial-property-list)))
        (unless value
          (when-let (default-value (struct-unsafe-get property :default-value))
            (setq value (eval default-value environment))))
        (unless (or value (not (struct-unsafe-get property :required)))
          (error "Required property not provided: %s"
                 (struct-unsafe-get property :name)))
        (push (cons (struct-unsafe-get property :name) value)
              environment)
        (push keyword property-list)
        (push value property-list)))
    (append (nreverse property-list)
            initial-property-list)))

;;;###autoload
(defmacro struct-define (name &optional documentation &rest declarations)
  "Define a new struct NAMẸ."
  (declare (indent 1) (doc-string 2))
  (unless (symbolp name)
    (error "Expected a symbol: %s" name))
  (unless (stringp documentation)
    (push documentation declarations)
    (setq documentation nil))
  (-let* (((struct-declarations property-declarations)
           (commons-split-property-list-start declarations))
          (struct-properties
           (append struct-declarations
                   (list :name name :documentation documentation
                         :properties
                         (-map #'struct-construct-property property-declarations))))
          (type (apply #'struct-type struct-properties))
          (star-name (intern (concat (symbol-name name) "*"))))
    `(prog1
         (defmacro ,name (&rest arguments)
           (declare (no-font-lock-keyword t))
           (list 'struct-construct
                 '',name (struct-expand-syntax arguments)))
       (defun ,star-name (&rest arguments)
         (struct-construct ',name arguments))
       (put ',name struct-type-symbol ',type)
       (when struct-enable-syntax-highlighting
         (struct-syntax-highlight-add ',name)))))

(defun struct-construct-property (declaration)
  (cond
   ((symbolp declaration)
    (struct-property :name declaration))
   ((consp declaration)
    (-let* ((((positional &as name default-value documentation)
              property-list)
             (commons-split-property-list-end declaration))
            (positional-property-list
             (cl-case (length positional)
               (0 nil)
               (1 (list :name name))
               (2 (list :name name :default-value default-value))
               (3 (list :name name :default-value default-value
                        :documentation documentation))
               (t (error "Invalid property declaration: %s" declaration)))))
      (apply #'struct-property (append positional-property-list
                                       property-list))))
   (t
    (error "Invalid property declaration: %s" declaration))))

(defun struct-expand-syntax (arguments)
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
          (push `(struct-properties ,(nth 1 argument))
                expanded-property-list))
         ((and (symbolp argument) (not (keywordp argument)))
          ;; Shorthand syntax.
          (push `(list ,(commons-symbol-to-keyword argument)
                       ,argument)
                expanded-property-list))
         (t
          ;; Probably an error later on.
          (push `(list ,argument)
                expanded-property-list)))))
    `(apply (function append)
            (list ,@(nreverse expanded-property-list)))))

(defun struct-syntax-highlight-add (name)
  (let ((keywords `((,(format "\\_<%s\\_>" name) 0 'font-lock-type-face))))
    (put name struct-syntax-highlight-symbol keywords)
    (font-lock-add-keywords 'emacs-lisp-mode keywords)))

(defun struct-syntax-highlight-remove (name &optional undefine)
  (let ((keywords (get name struct-syntax-highlight-symbol)))
    (font-lock-remove-keywords 'emacs-lisp-mode keywords)
    (when undefine
      (put name struct-syntax-highlight-symbol nil))))

(defun struct-undefine (name)
  (when (memq name '(struct-type struct-property))
    (error "Attempted to undefine a core type: %s" name))
  (when-let (type (struct-get-type name :no-error))
    (put name struct-type-symbol nil)
    (struct-syntax-highlight-remove name :undefine)
    (fmakunbound name)
    (fmakunbound (intern (concat (symbol-name name) "*")))))

;;;###autoload
(defun struct-member? (struct property)
  (--find (eq property (struct-unsafe-get it :keyword))
          (struct-unsafe-get
           (struct-get-type (car struct))
           :properties)))

;;;###autoload
(defun struct-get (struct property)
  (unless (struct-member? struct property)
    (error "Property is not a member of struct: %s" property))
  (struct-unsafe-get struct property))

;;;###autoload
(defun struct-set (struct property value)
  (let ((struct-property (struct-member? struct property))
        (type (struct-get-type (car struct))))
    (unless struct-property
      (error "Property is not a member of struct: %s" property))
    (when (and (null value)
               (struct-unsafe-get struct-property :required))
      (error "Attempted to set required property to `nil': %s" property))
    (when (or (struct-unsafe-get type :read-only)
              (struct-unsafe-get struct-property :read-only))
      (error "Attempted to set read-only property: %s" property)))
  (struct-unsafe-set struct property value))

(provide 'struct)
;;; struct.el ends here
