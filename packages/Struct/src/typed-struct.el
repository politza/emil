;;; Struct.el --- Structs based on plists -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024  Andreas Politz

;; Author: Andreas Politz <mail@andreas-politz.de>
;; Keywords: extensions
;; Version: 1.0.0beta1
;; Package-Requires: ((emacs "29.1") (dash "2.19.1"))

(require 'typed-core)
(require 'typed-util)
(require 'dash)
(eval-when-compile (require 'cl-lib))

(defmacro typed-define-struct (&rest parameters)
  "Define a new struct-type.

\(fn [NAME | (NAME . PARENTS)] &optional DOCUMENTATION &rest KEYWORD VALUE ... . DECLARATIONS\)"
  (declare (indent defun))
  (-let (((&plist :name :parents :documentation :declarations :configuration)
          (typed-define-struct-process-parameters parameters)))
    (typed-define-struct-validate name parents declarations)
    (let* ((child-properties (typed-define-struct-create-properties declarations))
           (parent-structs (typed-struct-resolve-types parents))
           (properties (typed-define-struct-merge-properties child-properties parent-structs))
           (configuration-struct (apply #'typed-struct-type-configuration configuration))
           (struct (typed-struct-type :name name :properties properties :configuration configuration-struct))
           (generated-documentation (typed-define-struct-generate-documentation struct documentation))
           (parameters (make-symbol "parameters")))
      (typed-struct-disclaim-type name)
      (prog1
          `(defun ,name (&rest ,parameters)
             ,generated-documentation
             ,(typed-define-struct-constructor-body struct parameters))
        (typed-declare-type name struct)))))

(defun typed-struct-disclaim-type (name)
  (when (typed-struct-type-p (typed-resolve-type name))
    (typed-disclaim-type name)
    (fmakunbound name)))

(defun typed-define-struct-generate-documentation (struct provided-documentation)
  (typed-with-print-documentation-buffer
   (-let (((_ &plist :name :properties) struct))
     (if provided-documentation
         (princ provided-documentation)
       (princ (format "Create a struct of type `%s'." (upcase (symbol-name name)))))
     (terpri nil t) (terpri)
     (princ "The struct is composed of the following properties.\n\n")
     (-each properties
       (-lambda ((_ &plist :name :type :default :optional :documentation))
         (princ (format "`%s': `%s'%s\n"
                        name type (if optional (format " = %s" default) "")))
         (when documentation
           (princ documentation))
         (terpri nil t) (terpri)))
     (princ "\(fn ")
     (-each properties
       (-lambda ((_ &plist :name :type :default :optional))
         (princ (format ":%s %s "
                        name
                        (if optional (format "(%s %s)" type default) type)))))
     (delete-backward-char 1)
     (princ "\)"))))

(defun typed-define-struct-process-parameters (parameters)
  (-let (((name-and-parents documentation . configuration-and-declarations) parameters))
    (unless (listp name-and-parents)
      (setq name-and-parents (list name-and-parents)))
    (unless (stringp documentation)
      (push documentation configuration-and-declarations)
      (setq documentation nil))
    (let ((configuration nil))
      (while (and configuration-and-declarations
                  (keywordp (car configuration-and-declarations)))
        (push (pop configuration-and-declarations) configuration)
        (unless configuration-and-declarations
          (typed-signal 'typed-syntax-error "Keyword value is missing after: %s" (car configuration)))
        (push (pop configuration-and-declarations) configuration))
      (-let (((name . parents) name-and-parents)
             (declarations configuration-and-declarations))
        `(:name ,name :parents ,parents :documentation ,documentation
          :declarations ,declarations :configuration ,(nreverse configuration))))))

(defun typed-struct-type-p (value)
  (and (consp value) (eq (car value) 'typed-struct-type)))

(defun typed-struct-get (struct property)
  (plist-get (cdr struct) property))

(cl-defun typed-struct-type (&key name properties configuration)
  "Create struct type.

\(fn :name typed-identifier :properties \(list typed-struct-type-property\)\)"

  (typed-assert-identifier name)
  (unless (listp properties)
    (typed-signal 'typed-syntax-error "Properties should be a list: %s" properties))

  `(typed-struct-type
    :name ,name
    :properties ,properties
    :configuration ,configuration))

(cl-defun typed-struct-type-property (&key name type default optional documentation)
  "Create a struct-type property.

\(fn :name typed-identifier :type symbol :default form\))"

  (typed-assert-identifier name)
  (typed-assert-identifier type)

  `(typed-struct-type-property
    :name ,name
    :type ,type
    :default ,default
    :optional ,optional
    :documentation ,documentation))

(cl-defun typed-struct-type-configuration (&key allow-other-keys strict)
  `(typed-struct-type-configuration
    :allow-other-keys ,(not (null allow-other-keys))
    :strict ,(not (null strict))))

(defun typed-struct-resolve-types (parents)
  (--map (let ((struct (typed-resolve-type it)))
           (unless (typed-struct-type-p struct)
             (typed-signal 'typed-type-error "Not a known struct type: %s" it))
           struct)
         parents))

(defun typed-define-struct-create-properties (declarations)
  (-> (-reduce-from
       (-lambda ((documentation . result) declaration)
         (cond
          ((stringp declaration)
           (when documentation
             (typed-signal 'typed-syntax-error "Repeated documentation string found after: %S" documentation))
           (cons declaration result))
          (t
           (-let (((name type default) declaration))
             (cons nil
                   (cons
                    (typed-struct-type-property
                     :name name :type type :default default
                     :optional (>= (length declaration) 3)
                     :documentation documentation)
                    result))))))
       nil declarations)
      cdr nreverse))

(defun typed-define-struct-merge-properties (properties parents)
  (let ((inherited-properties nil)
        (-compare-fn (lambda (property-0 property-1)
                       (eq (typed-struct-get property-0 :name)
                           (typed-struct-get property-1 :name)))))
    (-each parents
      (lambda (parent)
        (-each (typed-struct-get parent :properties)
          (lambda (property)
            (if-let (inherited-property (car (-contains? inherited-properties property)))
                (unless (eq (typed-struct-get property :type)
                            (typed-struct-get inherited-property :type))
                  (typed-signal 'typed-type-error
                    (concat "Can not inherit from parents `%S' simultaneously,"
                            " since property `%s' has incompatible types: %s")
                    (--map (typed-struct-get it :name) parents)
                    (typed-struct-get property :name)
                    (--map (typed-struct-get it :type)
                           (list inherited-property property))))
              (if-let (present-property (car (-contains? properties property)))
                  (unless (eq (typed-struct-get property :type)
                              (typed-struct-get present-property :type))
                    (typed-signal 'typed-type-error
                      (concat "Can not inherit from parent `%s',"
                              " since property `%s' has incompatible type: %s")
                      (typed-struct-get parent :name)
                      (typed-struct-get property :name)
                      (--map (typed-struct-get it :type)
                             (list present-property property))))
                (push property inherited-properties)))))))
    (-concat inherited-properties properties)))

(defun typed-define-struct-validate (name parents declarations)
  (typed-assert-identifier name "struct name")
  (when (typed-builtin-type-p name)
    (typed-signal 'typed-type-error "Can not redefine builtin type: %s" name))
  (--each parents
    (typed-assert-identifier it "parent struct"))
  (--each declarations
    (unless (or (stringp it) (consp it))
      (typed-signal 'typed-syntax-error
        "Property declaration should be a non-empty list: %s" it))
    (when (consp it)
      (-let (((name type _default . rest) it))
        (typed-assert-identifier name "property name")
        (typed-assert-identifier type "property type")
        (when rest
          (typed-signal 'typed-syntax-error
            "Property declaration should contain at most 3 forms: %s" it))))))

(defun typed-define-struct-constructor-body (struct parameters)
  (let* ((configuration (typed-struct-get struct :configuration))
         (struct-name (typed-struct-get struct :name))
         (properties (typed-struct-get struct :properties))
         (names (--map (typed-struct-get it :name) properties))
         (keys (--map (typed-identifier-to-keyword it) names))
         (value `(list ',struct-name ,@(-flatten-n 1 (-zip-with #'list keys names)))))
    `(let* ,(--map (typed-struct-constructor-body-binding it parameters)
                   properties)
       ,(unless (typed-struct-get configuration :allow-other-keys)
          `(when-let (foreign-key (typed-plist-find-foreign-key ,parameters ',keys))
             (typed-signal 'typed-type-error
               "Property is not known to be defined: %s"
               (typed-keyword-to-identifier foreign-key))))
       (let ((typed-check-type-enabled
              ,(typed-struct-get configuration :strict)))
         (typed-check-type ',struct-name ,value)))))

(defun typed-struct-constructor-body-binding (property parameters)
  (-let (((_ &plist :name :type :default :optional) property))
    `(,name
      (typed-plist-get
       ,parameters
       ,(typed-identifier-to-keyword name)
       ,(if optional
            `(lambda nil
               (condition-case error
                   ,default
                 (error
                  (typed-signal 'typed-error
                    "Error evaluating default for property `%s': %s"
                    ',name
                    (error-message-string error)))))
          `(lambda nil
             (typed-signal 'typed-error
               "Mandatory property not provided: %s" ',name)))))))

(provide 'typed-struct)
;;; typed-struct.el ends here
