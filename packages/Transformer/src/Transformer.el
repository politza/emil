;;; Transformer.el --- Transform lisp forms into something beautifully. -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Andreas Politz

;; Author: Andreas Politz <mail@andreas-politz.de>
;; Keywords: extensions
;; Version: 1.0.0beta1
;; Package-Requires: ((emacs "29.1") (dash "2.19.1") (Struct "1.0.0beta1"))

(require 'Struct)
(require 'Trait)
(require 'dash)

(define-error 'Transformer:syntax-error "Syntax Error")

(defun Transformer:syntax-error (fmt &rest arguments)
  (declare (indent 1))
  (signal 'Transformer:syntax-error
          (list (apply #'format fmt (butlast arguments))
                (car (last arguments)))))

(defun Transformer:map-transform (self forms &optional data)
  (--map (Transformer:transform-form self it data) forms))

(defun Transformer:transform-let-bindings (self bindings &optional data)
  (-map (lambda (binding)
          (if (symbolp binding)
              (Transformer:transform-form self binding data)
            (list (Transformer:transform-form self (car binding) data)
                  (Transformer:transform-form self (cadr binding) data))))
        bindings))

(Trait:define Transformer ()
  (defmethod Transformer:transform-form (self form &optional data)
    (cond
     ((numberp form)
      (Transformer:transform-number self form data))
     ((stringp form)
      (Transformer:transform-string self form data))
     ((vectorp form)
      (Transformer:transform-vector self form data))
     ((symbolp form)
      (Transformer:transform-symbol self form data))
     ((consp form)
      (Transformer:transform-cons self form data))
     (t (error "Internal error: form is none of the above: %s" form))))
  
  (defmethod Transformer:transform-number (self number &optional data)
    number)
  
  (defmethod Transformer:transform-string (self string &optional data)
    string)
  
  (defmethod Transformer:transform-vector (self vector &optional data)
    vector)
  
  (defmethod Transformer:transform-symbol (self symbol &optional data)
    symbol)
  
  (defmethod Transformer:transform-cons (self cons &optional data)
    (let ((form (car cons))
          (forms (cdr cons)))
      (pcase form
        (`and (Transformer:transform-and self forms data))
        (`catch
            (unless (symbolp (car forms))
              (Transformer:syntax-error "catch tag should be a symbol: %s"
                (car forms) (cons 'catch forms)))
          (Transformer:transform-catch self (car forms) (cdr forms) data))
        (`cond
         (Transformer:transform-cond self forms data))
        (`defconst
          (unless (symbolp (car forms))
            (Transformer:syntax-error "defconst name should be a symbol: %s"
              (car forms) (cons 'defconst forms)))
          (unless (and (<= 3 (length forms))
                       (>= (length forms) 2))
            (Transformer:syntax-error
                "defconst should have either 2 or 3 arguments: %s"
              (car forms) (cons 'defconst forms)))
          (Transformer:transform-defconst
           self (car forms) (cadr forms) (caddr forms) data))
        (`defvar
          (unless (symbolp (car forms))
            (Transformer:syntax-error "defvar name should be a symbol: %s"
              (car forms) (cons 'defvar forms)))
          (Transformer:transform-defvar
           self (car forms) (cadr forms) (caddr forms) data))
        (`function
         (unless (= 1 (length forms))
           (Transformer:syntax-error "function should have exactly one argument: %s"
             forms (cons 'function forms)))
         (Transformer:transform-function self (car forms) data))
        (`if (when (< (length forms) 2)
               (Transformer:syntax-error
                   "if should have a condition and then form: %s"
                 forms (cons 'if forms)))
            (Transformer:transform-if
             self (car forms) (cadr forms) (cddr forms) data))
        (`interactive
         (Transformer:transform-interactive self (car forms) (cdr forms) data))
        (`lambda
          (unless (and (listp (car forms))
                       (-every? #'symbolp (car forms)))
            (Transformer:syntax-error
                "lambda arguments should be a list of symbols: %s"
              (car forms) (cons 'lambda forms)))
          (let ((arguments (pop forms))
                (documentation (and (stringp (car forms)) (pop forms)))
                (interactive (and (eq 'interactive (car-safe (car forms)))
                                  (pop forms))))
            (Transformer:transform-lambda
             self arguments documentation interactive forms data)))
        ((or `let `let*)
         (unless (and (listp (car forms))
                      (--every? (or (symbolp it)
                                    (and (consp it)
                                         (symbolp (car it))))
                                (car forms)))
           (Transformer:syntax-error
               "let bindings should be a list of symbols or start with one: %s"
             (car forms) (cons form forms)))
         (if (eq form 'let)
             (Transformer:transform-let self (car forms) (cdr forms) data)
           (Transformer:transform-let* self (car forms) (cdr forms) data)))
        (`or (Transformer:transform-or self forms data))
        (`prog1
            (unless (>= (length forms) 1)
              (Transformer:syntax-error "prog1 should contain a first form: %s"
                forms (cons 'prog1 forms)))
          (Transformer:transform-prog1 self (car forms) (cdr forms) data))
        (`prog2
            (unless (>= (length forms) 2)
              (Transformer:syntax-error
                  "prog2 should contain a first and second form: %s"
                forms (cons 'prog2 forms)))
            (Transformer:transform-prog2
             self (car forms) (cadr forms) (cdr forms) data))
        (`progn (Transformer:transform-progn self forms data))
        (`quote
         (unless (= 1 (length forms))
           (Transformer:syntax-error "quote should have exactly one argument: %s"
             forms (cons 'function forms)))
         (Transformer:transform-quote self (car forms) data))
        (`save-current-buffer
          (Transformer:transform-save-current-buffer self forms data))
        (`save-excursion
          (Transformer:transform-save-excursion self forms data))
        (`save-restriction
          (Transformer:transform-save-restriction self forms data))
        ((or `setq `setq-default)
         (unless (= 0 (% (length forms) 2))
           (Transformer:syntax-error "%s should have an even number of arguments: %s"
             form forms (cons form forms)))
         (--each forms
           (unless (or (= 1 (% it-index 2))
                       (symbolp it))
             (Transformer:syntax-error "%s place should be a symbol: %s"
               form it (cons form forms))))
         (if (eq form 'setq)
             (Transformer:transform-setq self forms data)
           (Transformer:transform-setq-default self forms data)))
        ((or `unwind-protect `while)
         (unless (>= (length forms) 1)
           (Transformer:syntax-error "%s should have at least one argument: %s"
             form forms (cons 'unwind-protect forms)))
         (if (eq form 'unwind-protect)
             (Transformer:transform-unwind-protect self (car forms) (cdr forms) data)
           (Transformer:transform-while self (car forms) (cdr forms) data)))
        ((pred macrop) (Transformer:transform-macro self form forms data))
        (_ (Transformer:transform-application self form forms data)))))  
  
  (defmethod Transformer:transform-and (self conditions &optional data)
    `(and ,@(Transformer:map-transform self conditions data)))
  
  (defmethod Transformer:transform-catch (self tag body &optional data)
    `(catch ,(Transformer:transform-form self tag data)
       ,@(Transformer:map-transform self body data)))
  
  (defmethod Transformer:transform-cond (self clauses &optional data)
    `(cond ,@(Transformer:map-transform self clauses data)))
  
  (defmethod Transformer:transform-defconst (self symbol init-value &optional
                                                  doc-string data)
    `(defconst ,(Transformer:transform-form self symbol data)
       ,(Transformer:transform-form self init-value data)
       ,@(and doc-string
              (list (Transformer:map-transform self doc-string data)))))
  
  (defmethod Transformer:transform-defvar (self symbol &optional init-value
                                                doc-string data)
    `(defvar ,(Transformer:transform-form self symbol data)
       ,@(and init-value
              (list (Transformer:map-transform self init-value data)))
       ,@(and doc-string
              (list (Transformer:map-transform self doc-string data)))))
  
  (defmethod Transformer:transform-function (self argument &optional data)
    `(function ,(Transformer:transform-form self argument data)))
  
  (defmethod Transformer:transform-if (self condition then else &optional data)
    `(if ,(Transformer:transform-form self condition data)
         (Transformer:transform-form self then data)
       ,@(Transformer:map-transform self else data)))
  
  (defmethod Transformer:transform-interactive (self descriptor modes &optional data)
    `(interactive ,@(Transformer:map-transform self (cons descriptor modes) data)))
  
  (defmethod Transformer:transform-lambda (self arguments documentation
                                                interactive body &optional data)
    `(lambda ,(Transformer:map-transform self arguments data)
       ,@(and documentation
              (list (Transformer:transform-form self documentation data)))
       ,@(and interactive
              (Transformer:transform-form self interactive data))
       ,@(Transformer:map-transform self body data)))
  
  (defmethod Transformer:transform-let (self bindings body &optional data)
    `(let ,(Transformer:transform-let-bindings self bindings data)
       ,@(Transformer:map-transform self body data)))
  
  (defmethod Transformer:transform-let* (self bindings body &optional data)
    `(let* ,(Transformer:transform-let-bindings self bindings data)
       ,@(Transformer:map-transform self body data)))
  
  (defmethod Transformer:transform-or (self conditions &optional data)
    `(or ,@(Transformer:map-transform self conditions data)))
  
  (defmethod Transformer:transform-prog1 (self first body &optional data)
    `(prog1 ,(Transformer:transform-form self first data)
       ,@(Transformer:map-transform self body data)))
  
  (defmethod Transformer:transform-prog2 (self first second body &optional data)
    `(prog2 ,(Transformer:transform-form self first data)
         ,(Transformer:transform-form self second data)
       ,@(Transformer:map-transform self body data)))
  
  (defmethod Transformer:transform-progn (self body &optional data)
    `(progn ,@(Transformer:map-transform self body data)))
  
  (defmethod Transformer:transform-quote (self argument &optional data)
    `(quote ,argument))
  
  (defmethod Transformer:transform-save-current-buffer (self body &optional data)
    `(save-current-buffer ,@(Transformer:map-transform self body data)))
  
  (defmethod Transformer:transform-save-excursion (self body &optional data)
    `(save-excursion ,@(Transformer:map-transform self body data)))
  
  (defmethod Transformer:transform-save-restriction (self body &optional data)
    `(save-restriction ,@(Transformer:map-transform self body data)))
  
  (defmethod Transformer:transform-setq (self definitions &optional data)
    `(setq ,@(Transformer:map-transform self definitions data)))
  
  (defmethod Transformer:transform-setq-default (self definitions &optional data)
    `(setq-default ,@(Transformer:map-transform self definitions data)))
  
  (defmethod Transformer:transform-unwind-protect (self form forms &optional data)
    `(unwind-protect (Transformer:transform-form self form data)
       ,@(Transformer:map-transform self forms data)))
  
  (defmethod Transformer:transform-while (self condition body &optional data)
    `(while (Transformer:transform-form self condition data)
       ,@(Transformer:map-transform self body data)))

  (defmethod Transformer:transform-macro (self macro arguments &optional data)
    (Transformer:transform-form self (macroexpand (cons macro arguments)) data))

  (defmethod Transformer:transform-application (self function arguments
                                                     &optional data)
    `(,(Transformer:transform-form self function)
      ,@(Transformer:map-transform self arguments data))))

(Struct:define Transformer:Identity)

(Trait:implement Transformer Transformer:Identity)

(defun Transformer:read (&optional point transformer)
  (Transformer:transform
   (save-excursion
     (goto-char (or point (point)))
     (read (current-buffer)))
   transformer))

(defun Transformer:transform (form &optional transformer)
  (Transformer:transform-form
   (or transformer (Transformer:Identity))
   form))

(provide 'Transformer)
;;; Transformer.el ends here
