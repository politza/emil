;; -*- lexical-binding: t -*-

(require 'Struct/Primitives)

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
                       :mutable :type)
               (Struct:unsafe-properties it)))
        (princ (format "- %s" name))
        (when type
          (princ (format " :: %s" type)))
        (princ "	(")
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
        (format "%s" (car it))
        (Struct:unsafe-get type :properties))
       (mapconcat #'identity it " ")
       (format "\(fn (&plist %s))" it)))

(defun Struct:-doc-predicate (type)
  (with-output-to-string
    (princ (format Struct:-doc-predicate (Struct:unsafe-get type :name)))
    (terpri)))

(define-symbol-prop 'Struct:Type 'function-documentation
  (Struct:-doc-constructor (Struct:Type:get 'Struct:Type :ensure)))

(define-symbol-prop 'Struct:Property 'function-documentation
  (Struct:-doc-constructor (Struct:Type:get 'Struct:Property :ensure)))

(provide 'Struct/Doc)
