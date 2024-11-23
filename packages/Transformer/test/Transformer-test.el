;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'Transformer)
(require 'cl-macs)

(describe "Transformer"
  (describe "provides an identity mapping by default for"
    (it "numbers"
      (expect (Transformer:transform 0) :to-equal 0))

    (it "strings"
      (expect (Transformer:transform "string") :to-equal "string"))

    (it "vectors"
      (expect (Transformer:transform [((vector))])
              :to-equal [((vector))]))

    (it "symbols"
      (expect (Transformer:transform 'symbol) :to-equal 'symbol)
      (expect (Transformer:transform nil) :to-equal nil))

    (describe "cons cells with"
      (describe "special-form"
        (it "and"
          (expect (Transformer:transform '(and form other))
                  :to-equal '(and form other)))
        
        (it "catch"
          (expect (Transformer:transform '(catch tag body))
                  :to-equal '(catch tag body)))
        
        (it "cond"
          (expect (Transformer:transform '(cond (cond-0 body-0)
                                                (cond-1 body-1)))
                  :to-equal '(cond (cond-0 body-0)
                                   (cond-1 body-1))))
        
        (it "defconst"
          (expect (Transformer:transform
                   '(defconst symbol init-value "doc-string"))
                  :to-equal
                  '(defconst symbol init-value "doc-string")))
        
        (it "defvar"
          (expect (Transformer:transform
                   '(defconst symbol init-value "doc-string"))
                  :to-equal
                  '(defconst symbol init-value "doc-string")))
        
        (it "function"
          (expect (Transformer:transform '(function (lambda nil)))
                  :to-equal
                  '(function (lambda nil))))
        
        (it "if"
          (expect (Transformer:transform
                   '(if condition then else-0 else-1))
                  :to-equal
                  '(if condition then else-0 else-1)))
        
        (it "interactive"
          (expect (Transformer:transform '(interactive "p"))
                  :to-equal '(interactive "p")))
        
        (it "lambda"
          (expect (Transformer:transform
                   '(lambda (arg) "documentation" body-0 body-1))
                  :to-equal
                  '(lambda (arg) "documentation" body-0 body-1)))
        
        (it "let"
          (expect (Transformer:transform
                   '(let ((binding-0 value-0)
                          (binding-1 value-1))
                      body-0 body-1))
                  :to-equal
                  '(let ((binding-0 value-0)
                         (binding-1 value-1))
                     body-0 body-1)))
        
        (it "let*"
          (expect (Transformer:transform
                   '(let* ((binding-0 value-0)
                           (binding-1 value-1))
                      body-0 body-1))
                  :to-equal
                  '(let* ((binding-0 value-0)
                          (binding-1 value-1))
                     body-0 body-1)))
        
        (it "or"
          (expect (Transformer:transform '(or form-0 form-1) )
                  :to-equal
                  '(or form-0 form-1)))
        
        (it "prog1"
          (expect (Transformer:transform '(prog1 first body-0 body-1))
                  :to-equal
                  '(prog1 first body-0 body-1)))
        
        (it "prog2"
          (expect (Transformer:transform
                   '(prog2 first second body-0 body-1))
                  :to-equal
                  '(prog2 first second body-0 body-1)))
        
        (it "progn"
          (expect (Transformer:transform
                   '(prog body-0 body-1))
                  :to-equal
                  '(prog body-0 body-1)))
        
        (it "quote"
          (expect (Transformer:transform '(quote ((illegal-form))))
                  :to-equal
                  '(quote ((illegal-form)))))
        
        (it "save-current-buffer"
          (expect (Transformer:transform
                   '(save-current-buffer body-0 body-1))
                  :to-equal
                  '(save-current-buffer body-0 body-1)))
        
        (it "save-excursion"
          (expect (Transformer:transform
                   '(save-excursion body-0 body-1))
                  :to-equal
                  '(save-excursion body-0 body-1)))
        
        (it "save-restriction"
          (expect (Transformer:transform
                   '(save-restriction body-0 body-1))
                  :to-equal
                  '(save-restriction body-0 body-1)))
        
        (it "setq"
          (expect (Transformer:transform '(setq variable-0 value-0
                                                variable-1 value-1))
                  :to-equal
                  '(setq variable-0 value-0
                         variable-1 value-1)))
        
        (it "setq-default"
          (expect (Transformer:transform '(setq-default variable-0 value-0
                                                        variable-1 value-1))
                  :to-equal
                  '(setq-default variable-0 value-0
                                 variable-1 value-1)))
        
        (it "unwind-protect"
          (expect (Transformer:transform
                   '(unwind-protect body-form
                      unwind-form-0 unwind-form-1))
                  :to-equal
                  '(unwind-protect body-form
                     unwind-form-0 unwind-form-1)))

        (it "while"
          (expect (Transformer:transform
                   '(while condition
                      body-0 body-1))
                  :to-equal
                  '(while condition
                     body-0 body-1))))
      
      (it "application"
        (expect (Transformer:transform
                 '(list 0 1))
                :to-equal
                '(list 0 1)))
      
      (it "macros"
        (expect (Transformer:transform
                 '(cl-macrolet ((macro (arg) `(list 0 ,arg)))
                    (macro 1)))
                :to-equal
                '(list 0 1))))))
