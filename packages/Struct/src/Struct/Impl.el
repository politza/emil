;; -*- lexical-binding: t -*-

(require 'Struct/Function)
(require 'Commons)
(eval-when-compile (require 'cl-macs))

(defconst Struct:self-argument-name 'self
  "The name of the dispatch argument of struct-functions.")

(declare-function Emil:Syntax:create-transformer "Emil/Syntax" ())

(defmacro Struct:implement (name &rest properties-and-body)
  (declare (indent 1) (no-font-lock-keyword t))
  (cl-check-type name symbol)
  (Struct:-handle-evaluation-context name)
  (-let* (((properties body)
           (Commons:split-property-list properties-and-body))
          (disable-syntax (plist-get properties :disable-syntax))
          (functions (Struct:-read-body name body (unless disable-syntax name)))
          (transformer (unless (or disable-syntax
                                   (not (require 'Emil nil t)))
                         (Emil:Syntax:create-transformer))))
    `(progn
       (eval-and-compile
         (Struct:-merge-functions
          (Struct:Type:get ',name :ensure)
          (copy-sequence ',functions)))
       ;; FIXME: Functions should be unimplemented, if defining them throws an error.
       ,@(--map (Struct:Function:emit-definition it transformer :flush)
                functions)
       ',name)))

(defun Struct:-handle-evaluation-context (name)
  (let ((filename (Commons:evaluation-context-filename)))
    (cl-ecase (Commons:evaluation-context)
      ((load eval)
       (Struct:unimplement-from name nil)
       (when filename 
         (Struct:unimplement-from name filename)))
      (compile
       (when (Struct:implemented-in? name filename)
         (error "%s implemented mutliple times in file: %s"
                name (abbreviate-file-name filename)))))))

(defun Struct:-read-body (struct-name body &optional namespace)
  (let ((functions (--map (Struct:Function:read it namespace) body)))
    (--each functions
      (let ((arguments (Struct:get it :arguments))
            (name (Struct:get it :name)))
        (when (--find (eq Struct:self-argument-name (Struct:get it :name))
                      (cdr arguments))
          (error "Dispatch argument-name %s may only appear in first position: %s"
                 Struct:self-argument-name name))
        (when (and arguments
                   (eq Struct:self-argument-name
                       (Struct:get (car arguments) :name)))
          (unless (Struct:get (car arguments) :type)
            (Struct:unsafe-set (car arguments) :type struct-name))
          (unless (equal struct-name
                         (Struct:get (car arguments) :type))
            (error "Dispatch argument of struct-function must have struct-type %s: %s"
                   struct-name name)
            ))))
    functions))

(defun Struct:-merge-functions (type functions)
  (Struct:update type :functions
    (lambda (alist)
      (let* ((update (--annotate (Struct:get it :qualified-name) functions))
             (filtered (--remove (assq (car it) update) alist)))
        (append update filtered)))))

(defun Struct:unimplement (name)
  (when-let (type (Struct:Type:get name))
    (--each (-map #'cdr (Struct:get type :functions))
      (fmakunbound (Struct:get it :qualified-name)))))

(defun Struct:unimplement-from (name filename)
  (when-let (type (Struct:Type:get name))
    (--each (Struct:-functions-from type filename)
      (fmakunbound (Struct:get it :qualified-name)))))

(defun Struct:implemented-in? (name filename)
  (when-let (type (Struct:Type:get name))
    (--some? (Struct:Function:declared-in (cdr it) filename)
             (Struct:get type :functions))))

(defun Struct:-functions-from (type filename)
  (--filter (Struct:Function:declared-in it filename)
            (-map #'cdr (Struct:get type :functions))))

(provide 'Struct/Impl)
