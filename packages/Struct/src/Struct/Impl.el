;; -*- lexical-binding: t -*-

(require 'Struct/Function)
(require 'Commons)
(eval-when-compile (require 'cl-macs))

(declare-function Emil:Syntax:transform "Emil/Syntax" (function))

(defvar Struct:declared-functions nil)

;; FIXME: Improve handling of redefined functions, same for traits.
(defmacro Struct:implement (name &rest properties-and-body)
  (declare (indent 1) (no-font-lock-keyword t))
  (cl-check-type name symbol)
  (Struct:-handle-evaluation-context name)
  (-let* (((properties body)
           (Commons:split-property-list properties-and-body))
          (disable-syntax (plist-get properties :disable-syntax))
          (functions
           (--map (Struct:read-function name it disable-syntax) body))
          (transformer (unless (or disable-syntax
                                   (not (require 'Emil nil t)))
                         #'Emil:Syntax:transform))
          (Struct:declared-functions
           (cons (cons name functions)
                 Struct:declared-functions)))
    `(progn
       ,@(--map (Struct:Function:emit-definition it transformer :flush)
                functions)
       (eval-and-compile
         (Struct:-update-functions
          (Struct:Type:get ',name :ensure)
          (copy-sequence ',functions)))
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
         (error "%s implemented multiple times in file: %s" name filename))))))

(defun Struct:read-function (struct-name form &optional disable-syntax)
  (let* ((function (Struct:Function:read form (unless disable-syntax struct-name)))
         (arguments (Struct:get function :arguments))
         (name (Struct:get function :name)))
    (when (--find (eq Struct:Function:self-symbol (Struct:get function :name))
                  (cdr arguments))
      (error "Dispatch argument-name %s may only appear in first position: %s"
             Struct:Function:self-symbol name))
    (when (and arguments
               (eq Struct:Function:self-symbol
                   (Struct:get (car arguments) :name)))
      (unless (Struct:get (car arguments) :type)
        (Struct:unsafe-set (car arguments) :type struct-name))
      (unless (equal struct-name
                     (Struct:get (car arguments) :type))
        (error "Dispatch argument of struct-function must have struct-type %s: %s"
               struct-name name)))
    function))

(defun Struct:-update-functions (type functions)
  (Struct:set type :functions
    (Struct:-merged-functions type functions)))

(defun Struct:-merged-functions (type functions)
  (let* ((update (--annotate (Struct:get it :qualified-name) functions))
         (filtered (--remove (assq (car it) update) (Struct:get type :functions))))
    (append update filtered)))

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

(defun Struct:functions (name)
  "Returns a list of functions implemented by struct with name NAME."
  (when-let (type (Struct:Type:get name))
    (-map #'cdr (Struct:-merged-functions
                 type (cdr (assq name Struct:declared-functions))))))

(provide 'Struct/Impl)
