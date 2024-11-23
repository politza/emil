;;; Transformer.el --- Transform lisp forms into something beautifully. -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Andreas Politz

;; Author: Andreas Politz <mail@andreas-politz.de>
;; Keywords: extensions
;; Version: 1.0.0beta1
;; Package-Requires: ((emacs "29.1") (dash "2.19.1") (Struct "1.0.0beta1") (Commons "1.0.0beta1"))

(require 'Commons)
(require 'Struct)
(require 'Trait)
(require 'dash)

(Trait:define Transformer:Form ()
  (fn Transformer:Form:value (self) self)

  (fn Transformer:Form:location (self -> (Trait Transformer:Location))
    (ignore self)
    nil)

  (fn Transformer:Form:unwrap (self)
    "Unwrap this form recursively to a plain Lisp one."
    (Transformer:Form:unwrap-n self most-positive-fixnum))
  
  (fn Transformer:Form:unwrap-n (self n)
    "Unwraps this form and its first N subforms recursively."
    (let ((value (Transformer:Form:value self)))
      (cond
       ((consp value)
        (append (--map (Transformer:Form:unwrap it)
                       (-take n value))
                (-drop n value)))
       (t value)))))

(Trait:define Transformer:Location()
  (fn Transformer:Location:position (self -> (Trait Transformer:Position)))
  (fn Transformer:Location:source (self -> string)))

(Trait:define Transformer:Position ()
  (fn Transformer:Position:start (self))
  (fn Transformer:Position:end (self)))

(Commons:define-error Transformer:syntax-error "Syntax Error")

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
    (let ((value (Transformer:Form:value form)))
      (cond
       ((numberp value)
        (apply #'Transformer:transform-number self form value data))
       ((stringp value)
        (apply #'Transformer:transform-string self form value data))
       ((vectorp value)
        (apply #'Transformer:transform-vector self form value data))
       ((symbolp value)
        (apply #'Transformer:transform-symbol self form value data))
       ((consp value)
        (apply #'Transformer:transform-cons self form value data))
       (t (error "Internal error: form is none of the above: %s" form)))))

  (fn Transformer:transform-cons (self form cons &rest data)
    (let* ((elements (Transformer:Form:value cons))
           (head (car elements))
           (rest (cdr elements)))
      (pcase (Transformer:Form:value head)
        ((pred macrop)
         (apply #'Transformer:transform-macro self form head rest data))
        (`and (apply #'Transformer:transform-and self form rest data))
        (`catch
            (apply #'Transformer:transform-catch
                   self form (car rest) (cdr rest) data))
        (`cond
         (apply #'Transformer:transform-cond self form rest data))
        (`defconst
          (unless (symbolp (Transformer:Form:value (car rest)))
            (Transformer:syntax-error "defconst name should be a symbol: %s"
                                      (car rest) (cons 'defconst rest)))
          (unless (and (<= 3 (length rest))
                       (>= (length rest) 2))
            (Transformer:syntax-error
             "defconst should have either 2 or 3 arguments: %s"
             (car rest) (cons 'defconst rest)))
          (apply #'Transformer:transform-defconst
                 self form (car rest) (cadr rest) (caddr rest) data))
        (`defvar
          (unless (symbolp (car rest))
            (Transformer:syntax-error "defvar name should be a symbol: %s"
                                      (car rest) (cons 'defvar rest)))
          (apply #'Transformer:transform-defvar
                 self form (car rest) (cadr rest) (caddr rest) data))
        (`function
         (unless (= 1 (length rest))
           (Transformer:syntax-error "function should have exactly one argument: %s"
                                     rest (cons 'function rest)))
         (unless (or (symbolp (car rest))
                     (eq 'lambda (car-safe (car rest))))
           (Transformer:syntax-error
            "function argument should be a symbol or lambda-form: %s"
            rest (cons 'function rest)))
         (apply #'Transformer:transform-function self form (car rest) data))
        (`if (when (< (length rest) 2)
               (Transformer:syntax-error
                "if should have a condition and then form: %s"
                rest (cons 'if rest)))
            (apply #'Transformer:transform-if
                   self form (car rest) (cadr rest) (cddr rest) data))
        (`interactive
         (apply #'Transformer:transform-interactive
                self form (car rest) (cdr rest) data))
        ((or `let `let*)
         (unless (and (listp (Transformer:Form:value (car rest)))
                      (--every? (or (symbolp it)
                                    (and (consp it)
                                         (symbolp (Transformer:Form:value (car it)))))
                                (-map #'Transformer:Form:value (car rest))))
           (Transformer:syntax-error
            "let bindings should be a list of symbols or start with one: %s"
            (car rest) (cons head rest)))
         (if (eq head 'let)
             (apply #'Transformer:transform-let self form (car rest) (cdr rest) data)
           (apply #'Transformer:transform-let* self form (car rest) (cdr rest) data)))
        (`or (apply #'Transformer:transform-or self form rest data))
        (`prog1
            (unless (>= (length rest) 1)
              (Transformer:syntax-error "prog1 should contain a first form: %s"
                                        rest (cons 'prog1 rest)))
          (apply #'Transformer:transform-prog1 self form (car rest) (cdr rest) data))
        (`progn (apply #'Transformer:transform-progn self form rest data))
        (`quote
         (unless (= 1 (length rest))
           (Transformer:syntax-error "quote should have exactly one argument: %s"
                                     rest elements))
         (apply #'Transformer:transform-quote self form (car rest) data))
        (`save-current-buffer
          (apply #'Transformer:transform-save-current-buffer self form rest data))
        (`save-excursion
          (apply #'Transformer:transform-save-excursion self form rest data))
        (`save-restriction
          (apply #'Transformer:transform-save-restriction self form rest data))
        (`setq
         (unless (= 0 (% (length rest) 2))
           (Transformer:syntax-error
            "setq should have an even number of arguments: %s"
            rest (cons head rest)))
         (--each rest
           (unless (or (= 1 (% it-index 2))
                       (symbolp (Transformer:Form:value it)))
             (Transformer:syntax-error "setq place should be a symbol: %s"
                                       it elements)))
         (apply #'Transformer:transform-setq self form rest data))
        ((or `unwind-protect `while)
         (unless (>= (length rest) 1)
           (Transformer:syntax-error "%s should have at least one argument: %s"
                                     head rest elements))
         (if (eq (Transformer:Form:value head) 'unwind-protect)
             (apply #'Transformer:transform-unwind-protect
                    self form (car rest) (cdr rest) data)
           (apply #'Transformer:transform-while
                  self form (car rest) (cdr rest) data)))
        (_ (when (special-form-p head)
             (warn "internal error: unknown special form: %s" (cons head rest)))
           (apply #'Transformer:transform-application self form head rest data)))))

  (fn Transformer:transform-number (self form number &rest data)
    (ignore self number data)
    form)

  (fn Transformer:transform-string (self form string &rest data)
    (ignore self string data)
    form)

  (fn Transformer:transform-vector (self form vector &rest data)
    (ignore self vector data)
    form)

  (fn Transformer:transform-symbol (self form symbol &rest data)
    (ignore self symbol data)
    form)

  (fn Transformer:transform-and (self form conditions &rest data)
    (cons (car form)
          (apply #'Transformer:map-transform self conditions data)))

  (fn Transformer:transform-catch (self form tag body &rest data)
    `(,(car form) ,(apply #'Transformer:transform-form self tag data)
       ,@(apply #'Transformer:map-transform self body data)))

  (fn Transformer:transform-cond (self form clauses &rest data)
    `(,(car form) ,@(apply #'Transformer:map-transform self clauses data)))

  (fn Transformer:transform-defconst (self form symbol init-value &optional
                                           doc-string &rest data)
    `(,(car form) ,(apply #'Transformer:transform-form self symbol data)
       ,(apply #'Transformer:transform-form self init-value data)
       ,@(and (Transformer:Form:value doc-string)
              (list (apply #'Transformer:transform-form self doc-string data)))))

  (fn Transformer:transform-defvar (self form symbol &optional init-value
                                         doc-string &rest data)
    `(,(car form) ,(apply #'Transformer:transform-form self symbol data)
       ,@(and (Transformer:Form:value init-value)
              (list
               (apply #'Transformer:map-transform self init-value data)))
       ,@(and (Transformer:Form:value doc-string)
              (list (apply #'Transformer:transform-form self doc-string data)))))

  (fn Transformer:transform-function (self form argument &rest data)
    (ignore self data)
    `(,(car form) ,(Transformer:Form:value argument)))

  (fn Transformer:transform-if (self form condition then else &rest data)
    `(,(car form) ,(apply #'Transformer:transform-form self condition data)
         ,(apply #'Transformer:transform-form self then data)
       ,@(apply #'Transformer:map-transform self else data)))

  (fn Transformer:transform-interactive (self form descriptor modes &rest data)
    `(,(car form)
      ,@(apply #'Transformer:map-transform
               self (cons descriptor modes) data)))

  (fn Transformer:transform-let (self form bindings body &rest data)
    `(,(car form)
      ,(apply #'Transformer:transform-let-bindings self bindings data)
       ,@(apply #'Transformer:map-transform self body data)))

  (fn Transformer:transform-let* (self form bindings body &rest data)
    `(,(car form)
      ,(apply #'Transformer:transform-let-bindings self bindings data)
       ,@(apply #'Transformer:map-transform self body data)))

  (fn Transformer:transform-or (self form conditions &rest data)
    `(,(car form)
      ,@(apply #'Transformer:map-transform self conditions data)))

  (fn Transformer:transform-prog1 (self form first body &rest data)
    `(,(car form) ,(apply #'Transformer:transform-form self first data)
       ,@(apply #'Transformer:map-transform self body data)))

  (fn Transformer:transform-progn (self form body &rest data)
    `(,(car form) ,@(apply #'Transformer:map-transform self body data)))

  (fn Transformer:transform-quote (self form argument &rest data)
    (ignore self form data)
    `(,(car form) ,(Transformer:Form:value argument)))

  (fn Transformer:transform-save-current-buffer (self form body &rest data)
    `(,(car form)
       ,@(apply #'Transformer:map-transform self body data)))

  (fn Transformer:transform-save-excursion (self form body &rest data)
    `(,(car form) ,@(apply #'Transformer:map-transform self body data)))

  (fn Transformer:transform-save-restriction (self form body &rest data)
    `(,(car form) ,@(apply #'Transformer:map-transform self body data)))

  (fn Transformer:transform-setq (self form definitions &rest data)
    `(,(car form)
      ,@(apply #'Transformer:map-transform self definitions data)))

  (fn Transformer:transform-unwind-protect (self form body-form unwind-forms
                                                 &rest data)
    `(,(car form) ,(apply #'Transformer:transform-form self body-form data)
       ,@(apply #'Transformer:map-transform self unwind-forms data)))

  (fn Transformer:transform-while (self form condition body &rest data)
    `(,(car form) ,(apply #'Transformer:transform-form self condition data)
       ,@(apply #'Transformer:map-transform self body data)))

  (fn Transformer:transform-application (self form function arguments &rest data)
    (ignore form)
    `(,(apply #'Transformer:transform-form self function data)
      ,@(apply #'Transformer:map-transform self arguments data)))

  (fn Transformer:transform-macro (self form macro arguments &rest data)
    (ignore form)
    (apply #'Transformer:transform-form
           self
           (macroexpand (-map #'Transformer:Form:value (cons macro arguments)))
           data)))

(defun Transformer:read (&optional point transformer &rest data)
  (apply #'Transformer:transform
   (save-excursion
     (goto-char (or point (point)))
     (read (current-buffer)))
   transformer data))

(Struct:define Transformer:Identity)

(Trait:implement Transformer Transformer:Identity)

(defun Transformer:transform (form &optional transformer &rest data)
  (apply #'Transformer:transform-form
         (or transformer (Transformer:Identity))
   form data))

(provide 'Transformer)
;;; Transformer.el ends here
