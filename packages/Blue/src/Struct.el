;; -*- lexical-binding: t -*-

(require 'Commons)
(eval-and-compile (require 'dash))

(defconst Struct:type-symbol 'Struct:Type
  "Symbol used to attach struct type-information to other symbols.")

(defvar Struct:enable-syntax-highlighting t
  "Whether to highlight struct types with face `font-lock-type-face'.")

(defun Struct:get-type (name)
  (or (get name Struct:type-symbol)
      (error "Not a struct: %s" name)))

(defun Struct:Type (&rest property-list)
  (Struct:construct 'Struct:Type property-list))

(put 'Struct:Type Struct:type-symbol
     '(Struct:Type
       :name Struct:Type
       :properties
       ((Struct:Property
         :name name
         :default-value nil
         :documentation "The name of this struct-typẹ."
         :required t)
        (Struct:Property
         :name documentation
         :default-value nil
         :documentation "The documentation of this struct-typẹ."
         :required nil)
        (Struct:Property
         :name properties
         :default-value nil
         :documentation "The properties of this struct-typẹ."
         :required nil))))

(defun Struct:Property (&rest property-list)
  (Struct:construct 'Struct:Property property-list))

(put 'Struct:Property Struct:type-symbol
     `(Struct:Type
       :name Struct:Property
       :properties
       ((Struct:Property
         :name name
         :default-value nil
         :documentation "The name of this propertỵ."
         :required t)
        (Struct:Property
         :name default-value
         :default-value nil
         :documentation "The default value of this propertỵ."
         :required nil)
        (Struct:Property
         :name documentation
         :default-value nil
         :documentation "The documentation of this propertỵ."
         :required nil)
        (Struct:Property
         :name required
         :default-value nil
         :documentation "Whether this property can have a `nil' valuẹ."
         :required nil))))

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

(defun Struct:empty-property-list (type)
  (--splice t (list (Commons:symbol-to-keyword it) nil)
            (--map (Struct:get it :name)
                   (Struct:get type :properties))))

(defun Struct:construct-property-list (type initial-property-list)
  (let ((properties (Struct:get type :properties))
        (property-list nil)
        (environment nil))
    (while properties
      (let ((property (pop properties))
            (keyword (pop initial-property-list))
            (value (pop initial-property-list)))
        (unless value
          (when-let (default-value (Struct:get property :default-value))
            (setq value (eval default-value environment))))
        (unless (or value (not (Struct:get property :required)))
          (error "Required property not provided: %s"
                 (Struct:get property :name)))
        (push (cons (Struct:get property :name) value)
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
  (let ((type (Struct:Type
               :name name :documentation documentation
               :properties
               (-map #'Struct:construct-property declarations)))
        (star-name (intern (concat (symbol-name name) "*"))))
    `(prog1
         (defmacro ,name (&rest arguments)
           (declare (no-font-lock-keyword t))
           (list 'Struct:construct
                 '',name (Struct:expand-syntax ',type arguments)))
       (defun ,star-name (&rest arguments)
         (Struct:construct ',name arguments))
       (put ',name Struct:type-symbol ',type)
       (when Struct:enable-syntax-highlighting
         (font-lock-add-keywords
          'emacs-lisp-mode
          '((,(format "\\_<%s\\_>" name) 0 'font-lock-type-face)))))))

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

(defun Struct:expand-syntax (type arguments)
  "Expands shorthand and spread-syntax in ARGUMENTS."
  (let ((property-names (--map (Struct:get it :name)
                               (Struct:get type :properties)))
        (expanded-property-list nil)
        (arguments (Struct:prepare-syntax arguments)))
    (while arguments
      (let ((argument (pop arguments)))
        (cond
         ((and (keywordp argument) arguments)
          ;; Keyword-value pair.
          (push `(list ,argument ,(pop arguments))
                expanded-property-list))
         ((and (eq '\,@ (car-safe argument))
               (= 1 (length argument)))
          ;; Spread syntax.
          (push `(Struct:properties ,(cadr arguments))
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

;;;###autoload
(defun Struct:get (struct property)
  (plist-get (cdr struct) property))

;;;###autoload
(defun Struct:set (struct property value)
  (setcdr struct
          (plist-put (cdr struct) property value)))

;;;###autoload
(defun Struct:properties (struct)
  (cdr struct))

(provide 'Struct)
