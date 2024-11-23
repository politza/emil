;; -*- lexical-binding: t -*-

(require 'dash)
(require 'Commons)
(require 'Struct)
(require 'Struct/Impl)
(require 'Emil/Type)
(require 'Emil/Env)
(require 'Emil/Context)

(defvar Emil:Package:declarations nil)

(Struct:define Emil:Package:Declaration
  "Defines type-declaration for a versioned package."
  (name
   "The name of the package."
   :type symbol)
  (min-version
   "If non `nil', the minimum version of the package these declarations pertain to."
   :type (or null string))
  (max-version
   "If non `nil', the maximum version of the package these declarations pertain to."
   :type (or null string))
  (functions
   "An association list mapping function symbols to their corresponding type."
   :type list)
  (variables
   "An association list mapping variable symbols to their corresponding type."
   :type list))

(Struct:implement Emil:Package:Declaration
  (fn Emil:Package:Declaration:version= (self other)
    "Return `t', if SELF's version equals OTHER's version."
    (and (Emil:Package:version= (Struct:get self :min-version)
                                (Struct:get other :min-version))
         (Emil:Package:version= (Struct:get self :max-version)
                                (Struct:get other :max-version))))

  (fn Emil:Package:Declaration:match-version? (self version)
    "Return `t', if SELF's min- and max-version include VERSION.

Also returns `t', if VERSION is `nil'."
    (or (null version)
        (and (or (Emil:Package:min-version< (Struct:get self :min-version) version)
                 (Emil:Package:version= (Struct:get self :min-version) version))
             (or (Emil:Package:max-version< version (Struct:get self :max-version))
                 (Emil:Package:version= version (Struct:get self :max-version))))))

  (fn Emil:Package:Declaration:specialises? (self other)
    "Return `t', if SELF is more specific than OTHER.

A declaration A is more specific than B, if A's min-version is greater
than B's; or is equal to, but its max-version is less."
    (or (Emil:Package:min-version< (Struct:get other :min-version)
                                   (Struct:get self :min-version))
        (and (Emil:Package:version= (Struct:get self :min-version)
                                    (Struct:get other :min-version))
             (Emil:Package:max-version< (Struct:get self :max-version)
                                        (Struct:get other :max-version))))))

(defun Emil:Package:version= (version other)
  "Return `t', if VERSION equals OTHER.

Like `version=', but also accepts `nil' values."
  (or (and (null version)
           (null other))
      (and (not (null version))
           (not (null other))
           (version= version other))))

(defun Emil:Package:min-version< (version other)
  "Return `t', if VERSION is less than OTHER.

Like `version<', but also accepts `nil' as a version and treats it as
a lower bound."
  (and other
       (or (null version)
           (version< version other))))

(defun Emil:Package:max-version< (version other)
  "Return `t', if VERSION is less than OTHER.

Like `version<', but also accepts `nil' as a version and treats it as
an upper bound."
  (and version
       (or (null other)
           (version< version other))))

(defun Emil:Package:add-declaration (declaration)
  (let* ((name (Struct:get declaration :name))
         (elt (assq name Emil:Package:declarations)))
    (if (null elt)
        (push (list name declaration) Emil:Package:declarations)
      (-let (((left right)
              (--split-with (Emil:Package:Declaration:specialises? it declaration)
                            (cdr elt))))
        (when (and right (Emil:Package:Declaration:version= (car right) declaration))
          (let ((existing (pop right)))
            (setq declaration
                  (Emil:Package:Declaration*
                   ,@declaration
                   :variables (or (Struct:get declaration :variables)
                                  (Struct:get existing :variables))
                   :functions (or (Struct:get declaration :functions)
                                  (Struct:get existing :functions))))))
        (setcdr elt (append left (list declaration) right))))
    name))

(defmacro Emil:Package:declare-functions (package &rest properties-and-declarations)
  "Declare function types for PACKAGE.

PACKAGE should be a symbol naming the package.

PROPERTIES may declare additional properties pertaining to this
declaration. Supported properties are a `:min-version' and
`:max-version' by which the declarations may be limited to specific
versions of the package.

The remaining arguments should be conses \(FUNCTION . TYPE\), where
FUNCTION is a symbol naming the function and TYPE its type in a form
readable by `Emil:Type:read'.

\(fn PACKAGE &optional PROPERTIES &rest DECLARATIONS)"
  (declare (indent 1))
  (cl-check-type package symbol)
  (cl-check-type package (not (satisfies Commons:constant-symbol?)))
  `(-let (((properties declarations)
           (Commons:split-property-list-start ',properties-and-declarations)))
     (Emil:Package:add-declaration
      (Emil:Package:Declaration
       :name ',package
       :min-version (plist-get properties :min-version)
       :max-version (plist-get properties :max-version)
       :functions (-map (-lambda ((symbol . type))
                          (cl-check-type symbol symbol)
                          (cons symbol (if type (Emil:Type:read-function type))))
                        declarations)))))

(defmacro Emil:Package:declare-variables (package &rest properties-and-declarations)
  "Declare variables types for PACKAGE.

See `Emil:Package:declare-functions' for the PACKAGE and PROPERTIES
arguments.

The remaining arguments should be conses \(VARIABLE . TYPE\), where
VARIABLE is a symbol naming the variable and TYPE its type in a form
readable by `Emil:Type:read'.

\(fn PACKAGE &optional PROPERTIES &rest DECLARATIONS)"
  (declare (indent 1))
  (cl-check-type package symbol)
  (cl-check-type package (not (satisfies Commons:constant-symbol?)))
  `(-let (((properties declarations)
           (Commons:split-property-list-start ',properties-and-declarations)))
     (Emil:Package:add-declaration
      (Emil:Package:Declaration
       :name ',package
       :min-version (plist-get properties :min-version)
       :max-version (plist-get properties :max-version)
       :variables (-map (-lambda ((symbol . type))
                          (cl-check-type symbol symbol)
                          (cons symbol (if type (Emil:Type:read type))))
                        declarations)))))

(Struct:define Emil:Package:Env
  "Defines the environment for a versioned package."
  (name
   "The name of the package."
   :type symbol)
  (version
   "The optional version of the package."
   :type (or null string))
  (declarations
   "The list of declarations pertaining to the name and version."
   :type list))

(Trait:implement Emil:Env Emil:Package:Env
  (fn Emil:Env:lookup-variable (self (variable symbol)
                                     &optional (context Emil:Context))
    (cdr (--some (assq variable (Struct:get it :variables))
                 (Struct:get self :declarations))))

  (fn Emil:Env:lookup-function (self (function symbol)
                                     &optional (context Emil:Context))
    (cdr (--some (assq function (Struct:get it :functions))
                 (Struct:get self :declarations)))))

(defun Emil:Package:Env:for (package &optional version)
  (Emil:Package:Env
   :name package
   :version version
   :declarations (--filter (Emil:Package:Declaration:match-version? it version)
                           (cdr (assq package Emil:Package:declarations)))))

(provide 'Emil/Package)
