;; -*- lexical-binding: t -*-

(require 'Struct/Function)

(defmacro Struct:implement (name &rest body)
  (declare (indent 1) (no-font-lock-keyword t))
  (cl-check-type name symbol)
  (Struct:-handle-evaluation-context name)
  (let ((functions (-map #'Struct:Function:read body)))
    `(progn
       ,@(--map (Struct:Function:emit-definition it nil t) functions)
       (Struct:-merge-functions
        (Struct:Type:get ',name :ensure)
        (copy-sequence ',functions))
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

(defun Struct:-merge-functions (type functions)
  (Struct:update type :functions
    (lambda (alist)
      (let* ((update (--annotate (Struct:get it :name) functions))
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
