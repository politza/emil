;; -*- lexical-binding: t -*-

(require 'Transformer)

;; This is more or less just a demo implementation.
(Struct:define Transformer:Identity)

(defun Transformer:Identity:map-transform (self forms &rest data)
  (--map (apply #'Transformer:transform-form self it data) forms))

(defun Transformer:Identity:transform-let-bindings (self bindings &rest data)
  (-map (lambda (binding)
          (if (symbolp binding)
              (apply #'Transformer:transform-form self binding data)
            (list (apply #'Transformer:transform-form self (car binding) data)
                  (apply #'Transformer:transform-form self (cadr binding) data))))
        bindings))

(Trait:implement Transformer Transformer:Identity
  (fn Transformer:transform-number (self form number &rest data)
    (ignore self form data)
    number)

  (fn Transformer:transform-string (self form string &rest data)
    (ignore self form data)
    string)

  (fn Transformer:transform-vector (self form vector &rest data)
    (ignore self form data)
    vector)

  (fn Transformer:transform-symbol (self form symbol &rest data)
    (ignore self form data)
    symbol)

  (fn Transformer:transform-and (self form conditions &rest data)
    (ignore form)
    `(and ,@(apply #'Transformer:Identity:map-transform self conditions data)))

  (fn Transformer:transform-catch (self form tag body &rest data)
    (ignore form)
    `(catch ,(apply #'Transformer:transform-form self tag data)
       ,@(apply #'Transformer:Identity:map-transform self body data)))

  (fn Transformer:transform-cond (self form clauses &rest data)
    (ignore form)
    `(cond ,@(apply #'Transformer:Identity:map-transform self clauses data)))

  (fn Transformer:transform-defconst (self form symbol init-value &optional
                                           doc-string &rest data)
    (ignore form)
    `(defconst ,(apply #'Transformer:transform-form self symbol data)
       ,(apply #'Transformer:transform-form self init-value data)
       ,@(and (Transformer:Form:value doc-string)
              (list (apply #'Transformer:transform-form self doc-string data)))))

  (fn Transformer:transform-defvar (self form symbol &optional init-value
                                         doc-string &rest data)
    (ignore form)
    `(defvar ,(apply #'Transformer:transform-form self symbol data)
       ,@(and (Transformer:Form:value init-value)
              (list
               (apply #'Transformer:Identity:map-transform self init-value data)))
       ,@(and (Transformer:Form:value doc-string)
              (list (apply #'Transformer:transform-form self doc-string data)))))

  (fn Transformer:transform-function (self form argument &rest data)
    (ignore self form data)
    `(function ,(Transformer:Form:value argument)))

  (fn Transformer:transform-if (self form condition then else &rest data)
    (ignore form)
    `(if ,(apply #'Transformer:transform-form self condition data)
         ,(apply #'Transformer:transform-form self then data)
       ,@(apply #'Transformer:Identity:map-transform self else data)))

  (fn Transformer:transform-interactive (self form descriptor modes &rest data)
    (ignore form)
    `(interactive
      ,@(apply #'Transformer:Identity:map-transform
               self (cons descriptor modes) data)))

  (fn Transformer:transform-let (self form bindings body &rest data)
    (ignore form)
    `(let ,(apply #'Transformer:Identity:transform-let-bindings self bindings data)
       ,@(apply #'Transformer:Identity:map-transform self body data)))

  (fn Transformer:transform-let* (self form bindings body &rest data)
    (ignore form)
    `(let* ,(apply #'Transformer:Identity:transform-let-bindings self bindings data)
       ,@(apply #'Transformer:Identity:map-transform self body data)))

  (fn Transformer:transform-or (self form conditions &rest data)
    (ignore form)
    `(or ,@(apply #'Transformer:Identity:map-transform self conditions data)))

  (fn Transformer:transform-prog1 (self form first body &rest data)
    (ignore form)
    `(prog1 ,(apply #'Transformer:transform-form self first data)
       ,@(apply #'Transformer:Identity:map-transform self body data)))

  (fn Transformer:transform-progn (self form body &rest data)
    (ignore form)
    `(progn ,@(apply #'Transformer:Identity:map-transform self body data)))

  (fn Transformer:transform-quote (self form argument &rest data)
    (ignore self form data)
    `(quote ,(Transformer:Form:value argument)))

  (fn Transformer:transform-save-current-buffer (self form body &rest data)
    (ignore form)
    `(save-current-buffer
       ,@(apply #'Transformer:Identity:map-transform self body data)))

  (fn Transformer:transform-save-excursion (self form body &rest data)
    (ignore form)
    `(save-excursion ,@(apply #'Transformer:Identity:map-transform self body data)))

  (fn Transformer:transform-save-restriction (self form body &rest data)
    (ignore form)
    `(save-restriction ,@(apply #'Transformer:Identity:map-transform self body data)))

  (fn Transformer:transform-setq (self form definitions &rest data)
    (ignore form)
    `(setq ,@(apply #'Transformer:Identity:map-transform self definitions data)))

  (fn Transformer:transform-unwind-protect (self form body-form unwind-forms
                                                 &rest data)
    (ignore form)
    `(unwind-protect ,(apply #'Transformer:transform-form self body-form data)
       ,@(apply #'Transformer:Identity:map-transform self unwind-forms data)))

  (fn Transformer:transform-while (self form condition body &rest data)
    (ignore form)
    `(while ,(apply #'Transformer:transform-form self condition data)
       ,@(apply #'Transformer:Identity:map-transform self body data)))

  (fn Transformer:transform-application (self form function arguments &rest data)
    (ignore form)
    (if (macrop (Transformer:Form:value function))
        (apply #'Transformer:transform-form
               self
               (macroexpand (-map #'Transformer:Form:value (cons function arguments)))
               data)
      `(,(apply #'Transformer:transform-form self function data)
        ,@(apply #'Transformer:Identity:map-transform self arguments data))))

  (fn Transformer:transform-macro (self form macro arguments &rest data)
    (ignore form)
    (apply #'Transformer:transform-form
           self
           (macroexpand (-map #'Transformer:Form:value (cons macro arguments)))
           data)))

(provide 'Transformer/Identity)
