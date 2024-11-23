;; -*- lexical-binding: t -*-

(require 'Transformer)
(require 'Trait)
(require 'trace)
(require 'Emil)
(require 'Emil/Type)
(require 'Emil/Context)

(defvar Emil:Trace:included-functions
  '(Emil:infer
    Emil:check
    Emil:lookup-variable
    Emil:lookup-function
    Emil:instantiate-arrow
    Emil:instantiate
    Emil:subtype
    Emil:infer-do
    Emil:infer-application))

(defvar Emil:Trace:excluded-functions
  '(Transformer:transform-form Transformer:transform-cons))

(defvar Emil:Trace:current-context nil)

(defun Emil:Trace:traced-functions ()
  "Returns a list functions to trace."
  (-difference
   (append
    Emil:Trace:included-functions
    (-map #'car (Struct:get (Trait:get 'Transformer) :methods)))
   Emil:Trace:excluded-functions))

(define-minor-mode Emil:Trace:mode
  "Trace methods calls of type-inference."
  :group 'emacs
  :global t
  (cond
   (Emil:Trace:mode
    (--each (Emil:Trace:traced-functions)
      (trace-function-background it))
    (advice-add #'trace-entry-message :override #'Emil:Trace:entry-message)
    (advice-add #'trace-exit-message :override #'Emil:Trace:exit-message))
   (t
    (advice-remove #'trace-entry-message #'Emil:Trace:entry-message)
    (advice-remove #'trace-exit-message #'Emil:Trace:exit-message)
    (untrace-all)
    (setq Emil:Trace:current-context nil))))

(defun Emil:Trace:pretty-print-context (context prefix)
  "Return CONTEXT as a pretty-printed string.

Prefix each line of the result with PREFIX."
  (cond
   ((and context (not (eq context Emil:Trace:current-context)))
    (setq Emil:Trace:current-context context)
    (concat "\n"
            (mapconcat (lambda (line)
                         (concat prefix line))
                       (split-string (pp-to-string context) "\n")
                       "\n")))
   (t "")))

(defun Emil:Trace:entry-message (function level arguments _context)
  "Replacement for `trace-entry-message'."
  (let ((print-escape-newlines t)
        (arguments (--filter (not (or (Emil? it) (Emil:Context? it))) arguments))
        (context (-find #'Emil:Context? arguments))
        (prefix (concat
                 (mapconcat #'char-to-string (make-string (max 0 (1- level)) ?|) " ")
                 (if (> level 1) " " ""))))
    (format "%s%d -> %S%s\n"
            prefix
            level
            (cons function arguments)
            (Emil:Trace:pretty-print-context context prefix))))

(defun Emil:Trace:exit-message (function level value _context)
  "Replacement for `trace-exit-message'."
  (let ((print-escape-newlines t)
        (context (cond
                  ((Emil:Context? value)
                   (prog1 value
                     (setq value nil)))
                  ((Emil:Context? (cdr-safe value))
                   (prog1 (cdr value)
                     (setq value (car value))))))
        (prefix (concat
                 (mapconcat 'char-to-string (make-string (1- level) ?|) " ")
                 (if (> level 1) " " ""))))
    (format "%s%d <- %s: %S%s\n"
            prefix
            level
            function
            value
            (Emil:Trace:pretty-print-context context prefix))))
