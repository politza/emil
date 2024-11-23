(Trait:define Transformer ()
  (fn transform-form (self form &optional data)
    (cond
     ((numberp form)
      (self.transform-number form data))
     ((stringp form)
      (self.transform-string form data))
     ((vectorp form)
      (self.transform-vector form data))
     ((symbolp form)
      (self.transform-symbol form data))
     ((consp form)
      (self.transform-cons form data))
     (t (error "Internal error: form is none of the above: %s" form))))
  
  (fn transform-number (self number &optional data)
    number)
  
  (fn transform-string (self string &optional data)
    string)
  
  (fn transform-vector (self vector &optional data)
    vector)
  
  (fn transform-symbol (self symbol &optional data)
    symbol)
  
  (fn transform-cons (self cons &optional data)
    (let ((form (car cons))
          (forms (cdr cons)))
      (pcase form
        (`and (self.transform-and forms data))
        (`catch
            (unless (symbolp (car forms))
              (Transformer:syntax-error "catch tag should be a symbol: %s"
                (car forms) (cons 'catch forms)))
          (self.transform-catch (car forms) (cdr forms) data))
        (`cond
         (self.transform-cond forms data))
        (`defconst
          (unless (symbolp (car forms))
            (Transformer:syntax-error "defconst name should be a symbol: %s"
              (car forms) (cons 'defconst forms)))
          (unless (and (<= 3 (length forms))
                       (>= (length forms) 2))
            (Transformer:syntax-error
                "defconst should have either 2 or 3 arguments: %s"
              (car forms) (cons 'defconst forms)))
          (self.transform-defconst (car forms) (cadr forms) (caddr forms) data))
        (`defvar
          (unless (symbolp (car forms))
            (Transformer:syntax-error "defvar name should be a symbol: %s"
              (car forms) (cons 'defvar forms)))
          (self.transform-defvar (car forms) (cadr forms) (caddr forms) data))
        (`function
         (unless (= 1 (length forms))
           (Transformer:syntax-error "function should have exactly one argument: %s"
             forms (cons 'function forms)))
         (self.transform-function (car forms) data))
        (`if (when (< (length forms) 2)
               (Transformer:syntax-error
                   "if should have a condition and then form: %s"
                 forms (cons 'if forms)))
            (self.transform-if (car forms) (cadr forms) (cddr forms) data))
        (`interactive
         (self.transform-interactive (car forms) (cdr forms) data))
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
            (self.transform-lambda arguments documentation interactive forms data)))
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
             (self.transform-let (car forms) (cdr forms) data)
           (self.transform-let* (car forms) (cdr forms) data)))
        (`or (self.transform-or forms data))
        (`prog1
            (unless (>= (length forms) 1)
              (Transformer:syntax-error "prog1 should contain a first form: %s"
                forms (cons 'prog1 forms)))
          (self.transform-prog1 (car forms) (cdr forms) data))
        (`prog2
            (unless (>= (length forms) 2)
              (Transformer:syntax-error
                  "prog2 should contain a first and second form: %s"
                forms (cons 'prog2 forms)))
            (self.transform-prog2 (car forms) (cadr forms) (cdr forms) data))
        (`progn (self.transform-progn forms data))
        (`quote
         (unless (= 1 (length forms))
           (Transformer:syntax-error "quote should have exactly one argument: %s"
             forms (cons 'function forms)))
         (self.transform-quote (car forms) data))
        (`save-current-buffer
          (self.transform-save-current-buffer forms data))
        (`save-excursion
          (self.transform-save-excursion forms data))
        (`save-restriction
          (self.transform-save-restriction forms data))
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
             (self.transform-setq forms data)
           (self.transform-setq-default forms data)))
        ((or `unwind-protect `while)
         (unless (>= (length forms) 1)
           (Transformer:syntax-error "%s should have at least one argument: %s"
             form forms (cons 'unwind-protect forms)))
         (if (eq form 'unwind-protect)
             (self.transform-unwind-protect (car forms) (cdr forms) data)
           (self.transform-while (car forms) (cdr forms) data)))
        ((pred macrop) (self.transform-macro form forms data))
        (_ (self.transform-application form forms data)))))  
  
  (fn transform-and (self conditions &optional data)
    `(and ,@(self.map-transform conditions data)))
  
  (fn transform-catch (self tag body &optional data)
    `(catch ,(self.transform-form tag data)
       ,@(self.map-transform body data)))
  
  (fn transform-cond (self clauses &optional data)
    `(cond ,@(self.map-transform clauses data)))
  
  (fn transform-defconst (self symbol init-value &optional
                               doc-string data)
    `(defconst ,(self.transform-form symbol data)
       ,(self.transform-form init-value data)
       ,@(and doc-string
              (list (self.map-transform doc-string data)))))
  
  (fn transform-defvar (self symbol &optional init-value
                             doc-string data)
    `(defvar ,(self.transform-form symbol data)
       ,@(and init-value
              (list (self.map-transform init-value data)))
       ,@(and doc-string
              (list (self.map-transform doc-string data)))))
  
  (fn transform-function (self argument &optional data)
    `(function ,(self.transform-form argument data)))
  
  (fn transform-if (self condition then else &optional data)
    `(if ,(self.transform-form condition data)
         (self.transform-form then data)
       ,@(self.map-transform else data)))
  
  (fn transform-interactive (self descriptor modes &optional data)
    `(interactive ,@(self.map-transform (cons descriptor modes) data)))
  
  (fn transform-lambda (self arguments documentation
                             interactive body &optional data)
    `(lambda ,(self.map-transform arguments data)
       ,@(and documentation
              (list (self.transform-form documentation data)))
       ,@(and interactive
              (self.transform-form interactive data))
       ,@(self.map-transform body data)))
  
  (fn transform-let (self bindings body &optional data)
    `(let ,(self.transform-let-bindings bindings data)
       ,@(self.map-transform body data)))
  
  (fn transform-let* (self bindings body &optional data)
    `(let* ,(self.transform-let-bindings bindings data)
       ,@(self.map-transform body data)))
  
  (fn transform-or (self conditions &optional data)
    `(or ,@(self.map-transform conditions data)))
  
  (fn transform-prog1 (self first body &optional data)
    `(prog1 ,(self.transform-form first data)
       ,@(self.map-transform body data)))
  
  (fn transform-prog2 (self first second body &optional data)
    `(prog2 ,(self.transform-form first data)
         ,(self.transform-form second data)
       ,@(self.map-transform body data)))
  
  (fn transform-progn (self body &optional data)
    `(progn ,@(self.map-transform body data)))
  
  (fn transform-quote (self argument &optional data)
    `(quote ,argument))
  
  (fn transform-save-current-buffer (self body &optional data)
    `(save-current-buffer ,@(self.map-transform body data)))
  
  (fn transform-save-excursion (self body &optional data)
    `(save-excursion ,@(self.map-transform body data)))
  
  (fn transform-save-restriction (self body &optional data)
    `(save-restriction ,@(self.map-transform body data)))
  
  (fn transform-setq (self definitions &optional data)
    `(setq ,@(self.map-transform definitions data)))
  
  (fn transform-setq-default (self definitions &optional data)
    `(setq-default ,@(self.map-transform definitions data)))
  
  (fn transform-unwind-protect (self form forms &optional data)
    `(unwind-protect (self.transform-form form data)
       ,@(self.map-transform forms data)))
  
  (fn transform-while (self condition body &optional data)
    `(while (self.transform-form condition data)
       ,@(self.map-transform body data)))

  (fn transform-macro (self macro arguments &optional data)
    (self.transform-form (macroexpand (cons macro arguments)) data))

  (fn transform-application (self function arguments
                                  &optional data)
    `(,(self.transform-form function)
      ,@(self.map-transform arguments data))))