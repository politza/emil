;;; Transformer.el --- Transform lisp forms into something beautifully. -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Andreas Politz

;; Author: Andreas Politz <mail@andreas-politz.de>
;; Keywords: extensions
;; Version: 1.0.0beta1
;; Package-Requires: ((emacs "29.1") (dash "2.19.1") (Struct "1.0.0beta1"))

(require 'Struct)
(require 'Trait)
(require 'dash)

(Trait:define Transformer:Form ()
  (fn Form:value (self) self))

(define-error 'Transformer:syntax-error "Syntax Error")

(defun Transformer:syntax-error (fmt &rest arguments)
  (declare (indent 1))
  (signal 'Transformer:syntax-error
          (list (apply #'format fmt (butlast arguments))
                (car (last arguments)))))

(defun Transformer:map-transform (self forms &rest data)
  (--map (apply #'Transformer:transform-form self it data) forms))

(defun Transformer:transform-let-bindings (self bindings &rest data)
  (-map (lambda (binding)
          (if (symbolp binding)
              (apply #'Transformer:transform-form self binding data)
            (list (apply #'Transformer:transform-form self (car binding) data)
                  (apply #'Transformer:transform-form self (cadr binding) data))))
        bindings))

(Trait:define Transformer ()
  (fn Transformer:transform-form (self form &rest data)
    (cond
     ((numberp (Form:value form))
      (apply #'Transformer:transform-number self form data))
     ((stringp (Form:value form))
      (apply #'Transformer:transform-string self form data))
     ((vectorp (Form:value form))
      (apply #'Transformer:transform-vector self form data))
     ((symbolp (Form:value form))
      (apply #'Transformer:transform-symbol self form data))
     ((consp (Form:value form))
      (apply #'Transformer:transform-cons self form data))
     (t (error "Internal error: form is none of the above: %s" form))))
  
  (fn Transformer:transform-number (self number &rest data)
    (ignore self data)
    (Form:value number))
  
  (fn Transformer:transform-string (self string &rest data)
    (ignore self data)
    (Form:value string))
  
  (fn Transformer:transform-vector (self vector &rest data)
    (ignore self data)
    (Form:value vector))
  
  (fn Transformer:transform-symbol (self symbol &rest data)
    (ignore self data)
    (Form:value symbol))
  
  (fn Transformer:transform-cons (self cons &rest data)
    (let* ((elements (Form:value cons))
           (head (car elements))
           (rest (cdr elements)))
      (pcase (Form:value head)
        ((pred macrop)
         (apply #'Transformer:transform-macro self head rest data))
        (`and (apply #'Transformer:transform-and self rest data))
        (`catch
            (unless (symbolp (Form:value (car rest)))
              (Transformer:syntax-error "catch tag should be a symbol: %s"
                (car rest) (cons 'catch rest)))
          (apply #'Transformer:transform-catch self (car rest) (cdr rest) data))
        (`cond
         (apply #'Transformer:transform-cond self rest data))
        (`defconst
          (unless (symbolp (Form:value (car rest)))
            (Transformer:syntax-error "defconst name should be a symbol: %s"
              (car rest) (cons 'defconst rest)))
          (unless (and (<= 3 (length rest))
                       (>= (length rest) 2))
            (Transformer:syntax-error
                "defconst should have either 2 or 3 arguments: %s"
              (car rest) (cons 'defconst rest)))
          (apply #'Transformer:transform-defconst
           self (car rest) (cadr rest) (caddr rest) data))
        (`defvar
          (unless (symbolp (car rest))
            (Transformer:syntax-error "defvar name should be a symbol: %s"
              (car rest) (cons 'defvar rest)))
          (apply #'Transformer:transform-defvar
           self (car rest) (cadr rest) (caddr rest) data))
        (`function
         (unless (= 1 (length rest))
           (Transformer:syntax-error "function should have exactly one argument: %s"
             rest (cons 'function rest)))
         (apply #'Transformer:transform-function self (car rest) data))
        (`if (when (< (length rest) 2)
               (Transformer:syntax-error
                   "if should have a condition and then form: %s"
                 rest (cons 'if rest)))
            (apply #'Transformer:transform-if
             self (car rest) (cadr rest) (cddr rest) data))
        (`interactive
         (apply #'Transformer:transform-interactive self (car rest) (cdr rest) data))
        (`lambda
          (unless (and (listp (Form:value (car rest)))
                       (-every? #'symbolp (-map #'Form:value (car rest))))
            (Transformer:syntax-error
                "lambda arguments should be a list of symbols: %s"
              (car rest) (cons 'lambda rest)))
          (let ((arguments (pop rest))
                (documentation (and (stringp (Form:value (car rest)))
                                    (pop rest)))
                (interactive (and (eq 'interactive (car-safe (Form:value (car rest))))
                                  (pop rest))))
            (apply #'Transformer:transform-lambda
             self arguments documentation interactive rest data)))
        ((or `let `let*)
         (unless (and (listp (Form:value (car rest)))
                      (--every? (or (symbolp it)
                                    (and (consp it)
                                         (symbolp (Form:value (car it)))))
                                (-map #'Form:value (car rest))))
           (Transformer:syntax-error
               "let bindings should be a list of symbols or start with one: %s"
             (car rest) (cons head rest)))
         (if (eq head 'let)
             (apply #'Transformer:transform-let self (car rest) (cdr rest) data)
           (apply #'Transformer:transform-let* self (car rest) (cdr rest) data)))
        (`or (apply #'Transformer:transform-or self rest data))
        (`prog1
            (unless (>= (length rest) 1)
              (Transformer:syntax-error "prog1 should contain a first form: %s"
                rest (cons 'prog1 rest)))
          (apply #'Transformer:transform-prog1 self (car rest) (cdr rest) data))
        (`progn (apply #'Transformer:transform-progn self rest data))
        (`quote
         (unless (= 1 (length rest))
           (Transformer:syntax-error "quote should have exactly one argument: %s"
             rest elements))
         (apply #'Transformer:transform-quote self (car rest) data))
        (`save-current-buffer
          (apply #'Transformer:transform-save-current-buffer self rest data))
        (`save-excursion
          (apply #'Transformer:transform-save-excursion self rest data))
        (`save-restriction
          (apply #'Transformer:transform-save-restriction self rest data))
        (`setq
         (unless (= 0 (% (length rest) 2))
           (Transformer:syntax-error "setq should have an even number of arguments: %s"
             rest (cons head rest)))
         (--each rest
           (unless (or (= 1 (% it-index 2))
                       (symbolp (Form:value it)))
             (Transformer:syntax-error "setq place should be a symbol: %s"
               it elements)))
         (apply #'Transformer:transform-setq self rest data))
        ((or `unwind-protect `while)
         (unless (>= (length rest) 1)
           (Transformer:syntax-error "%s should have at least one argument: %s"
             head rest elements))
         (if (eq (Form:value head) 'unwind-protect)
             (apply #'Transformer:transform-unwind-protect self (car rest) (cdr rest) data)
           (apply #'Transformer:transform-while self (car rest) (cdr rest) data)))
        (_ (apply #'Transformer:transform-application self head rest data)))))  
  
  (fn Transformer:transform-and (self conditions &rest data)
    `(and ,@(apply #'Transformer:map-transform self conditions data)))
  
  (fn Transformer:transform-catch (self tag body &rest data)
    `(catch ,(apply #'Transformer:transform-form self tag data)
       ,@(apply #'Transformer:map-transform self body data)))
  
  (fn Transformer:transform-cond (self clauses &rest data)
    `(cond ,@(apply #'Transformer:map-transform self clauses data)))
  
  (fn Transformer:transform-defconst (self symbol init-value &optional
                                           doc-string &rest data)
    `(defconst ,(apply #'Transformer:transform-form self symbol data)
       ,(apply #'Transformer:transform-form self init-value data)
       ,@(and (Form:value doc-string)
              (list (apply #'Transformer:transform-form self doc-string data)))))
  
  (fn Transformer:transform-defvar (self symbol &optional init-value
                                         doc-string &rest data)
    `(defvar ,(apply #'Transformer:transform-form self symbol data)
       ,@(and (Form:value init-value)
              (list (apply #'Transformer:map-transform self init-value data)))
       ,@(and (Form:value doc-string)
              (list (apply #'Transformer:transform-form self doc-string data)))))
  
  (fn Transformer:transform-function (self argument &rest data)
    (ignore self data)
    `(function ,(Form:value argument)))
  
  (fn Transformer:transform-if (self condition then else &rest data)
    `(if ,(apply #'Transformer:transform-form self condition data)
         ,(apply #'Transformer:transform-form self then data)
       ,@(apply #'Transformer:map-transform self else data)))
  
  (fn Transformer:transform-interactive (self descriptor modes &rest data)
    `(interactive ,@(apply #'Transformer:map-transform self (cons descriptor modes) data)))
  
  (fn Transformer:transform-lambda (self arguments documentation
                                         interactive body &rest data)
    `(lambda ,(apply #'Transformer:map-transform self arguments data)
       ,@(and (Form:value documentation)
              (list (apply #'Transformer:transform-form self documentation data)))
       ,@(and (Form:value interactive)
              (apply #'Transformer:transform-form self interactive data))
       ,@(apply #'Transformer:map-transform self body data)))
  
  (fn Transformer:transform-let (self bindings body &rest data)
    `(let ,(apply #'Transformer:transform-let-bindings self bindings data)
       ,@(apply #'Transformer:map-transform self body data)))
  
  (fn Transformer:transform-let* (self bindings body &rest data)
    `(let* ,(apply #'Transformer:transform-let-bindings self bindings data)
       ,@(apply #'Transformer:map-transform self body data)))
  
  (fn Transformer:transform-or (self conditions &rest data)
    `(or ,@(apply #'Transformer:map-transform self conditions data)))
  
  (fn Transformer:transform-prog1 (self first body &rest data)
    `(prog1 ,(apply #'Transformer:transform-form self first data)
       ,@(apply #'Transformer:map-transform self body data)))
  
  (fn Transformer:transform-progn (self body &rest data)
    `(progn ,@(apply #'Transformer:map-transform self body data)))
  
  (fn Transformer:transform-quote (self argument &rest data)
    (ignore self data)
    `(quote ,(Form:value argument)))
  
  (fn Transformer:transform-save-current-buffer (self body &rest data)
    `(save-current-buffer ,@(apply #'Transformer:map-transform self body data)))
  
  (fn Transformer:transform-save-excursion (self body &rest data)
    `(save-excursion ,@(apply #'Transformer:map-transform self body data)))
  
  (fn Transformer:transform-save-restriction (self body &rest data)
    `(save-restriction ,@(apply #'Transformer:map-transform self body data)))
  
  (fn Transformer:transform-setq (self definitions &rest data)
    `(setq ,@(apply #'Transformer:map-transform self definitions data)))
  
  (fn Transformer:transform-unwind-protect (self form forms &rest data)
    `(unwind-protect ,(apply #'Transformer:transform-form self form data)
       ,@(apply #'Transformer:map-transform self forms data)))
  
  (fn Transformer:transform-while (self condition body &rest data)
    `(while ,(apply #'Transformer:transform-form self condition data)
       ,@(apply #'Transformer:map-transform self body data)))
  
  (fn Transformer:transform-application (self function arguments &rest data)
    (if (macrop (Form:value function))
        (apply #'Transformer:transform-form
         self
         (macroexpand (-map #'Form:value (cons function arguments)))
         data)
      `(,(apply #'Transformer:transform-form self function data)
        ,@(apply #'Transformer:map-transform self arguments data))))

  (fn Transformer:transform-macro (self macro arguments &rest data)
    (apply #'Transformer:transform-form
     self
     (macroexpand (-map #'Form:value (cons macro arguments)))
     data)))

(Struct:define Transformer:Identity)
(Trait:implement Transformer Transformer:Identity)

(defun Transformer:read (&optional point transformer &rest data)
  (apply #'Transformer:transform
   (save-excursion
     (goto-char (or point (point)))
     (read (current-buffer)))
   transformer data))

(defun Transformer:transform (form &optional transformer &rest data)
  (apply #'Transformer:transform-form
   (or transformer (Transformer:Identity))
   form data))

(provide 'Transformer)
;;; Transformer.el ends here
