;; -*- lexical-binding: t -*-

(require 'Emil)
(require 'Struct/Function)
(require 'buttercup)

(describe "Emil:Syntax"
  (describe "Emil:Syntax:transform"
    :var ((function (Struct:Function
                     :name 'method
                     :qualified-name 'TestStruct:method
                     :arguments (list (Struct:Argument:read '(self TestStruct)))
                     :return-type 'number
                     :body nil)))

    (before-each
      (eval '(Struct:define TestStruct property))
      (eval '(Struct:implement TestStruct (fn method (self)))))

    (after-each
      (Struct:undefine 'TestStruct))

    (it "property access"
      (expect (Emil:Syntax:transform
               (Struct:Function*
                ,@function
                :body '(self.property)))
              :to-equal
              '((Struct:unsafe-get self :property))))

    (it "method call"
      (expect (Emil:Syntax:transform
               (Struct:Function*
                ,@function
                :body '((self.method))))
              :to-equal
              '((TestStruct:method self))))

    (it "method reference"
      (expect (Emil:Syntax:transform
               (Struct:Function*
                ,@function
                :body '((funcall #'self.method))))
              :to-equal
              '((funcall #'(lambda nil (TestStruct:method self))))))

    (it "property assignment"
      (expect (Emil:Syntax:transform
               (Struct:Function*
                ,@function
                :body '((setf self.property 1))))
              :to-equal
              '((Struct:set self :property 1))))

    (it "string"
      (expect (Emil:Syntax:transform
               (Struct:Function*
                ,@function
                :body '("a string")))
              :to-equal
              '("a string")))

    (it "number"
      (expect (Emil:Syntax:transform
               (Struct:Function*
                ,@function
                :body '(1)))
              :to-equal
              '(1)))

    (it "vector"
      (expect (Emil:Syntax:transform
               (Struct:Function*
                ,@function
                :body '([a vector])))
              :to-equal
              '([a vector])))

    (it "symbol"
      (expect (Emil:Syntax:transform
               (Struct:Function*
                ,@function
                :body '(self)))
              :to-equal
              '(self)))

    (it "application"
      (expect (Emil:Syntax:transform
               (Struct:Function*
                ,@function
                :body '((+ 1 self.property))))
              :to-equal
              '((+ 1 (Struct:unsafe-get self :property)))))

    (it "and"
      (expect (Emil:Syntax:transform
               (Struct:Function*
                ,@function
                :body '((and t self.property))))
              :to-equal
              '((and t (Struct:unsafe-get self :property)))))

    (it "catch"
      (expect (Emil:Syntax:transform
               (Struct:Function*
                ,@function
                :body '((catch 'tag (throw 'tag self.property)))))
              :to-equal
              '((catch 'tag (throw 'tag (Struct:unsafe-get self :property))))))

    (it "cond"
      (expect (Emil:Syntax:transform
               (Struct:Function*
                ,@function
                :body '((cond (self.property self.property)))))
              :to-equal
              '((cond ((Struct:unsafe-get self :property) (Struct:unsafe-get self :property))))) )

    (it "defconst"
      (expect (Emil:Syntax:transform
               (Struct:Function*
                ,@function
                :body '((defconst symbol self.property "documentation"))))
              :to-equal
              '((defconst symbol (Struct:unsafe-get self :property) "documentation"))))

    (it "defvar"
      (expect (Emil:Syntax:transform
               (Struct:Function*
                ,@function
                :body '((defvar symbol self.property "documentation"))))
              :to-equal
              '((defvar symbol (Struct:unsafe-get self :property) "documentation"))))

    (it "function"
      (expect (Emil:Syntax:transform
               (Struct:Function*
                ,@function
                :body '((function self.method))))
              :to-equal
              '((function (lambda nil (TestStruct:method self))))))

    (it "if"
      (expect (Emil:Syntax:transform
               (Struct:Function*
                ,@function
                :body '((if self.property self.property self.property))))
              :to-equal
              '((if (Struct:unsafe-get self :property)
                   (Struct:unsafe-get self :property)
                 (Struct:unsafe-get self :property)))))

    (xit "interactive"
      ;; FIXME: interactive not properly implemented.
      (expect (Emil:Syntax:transform
               (Struct:Function*
                ,@function
                :body '((interactive self.property))))
              :to-equal
              '((interactive (Struct:unsafe-get self :property)))))

    (it "let"
      (expect (Emil:Syntax:transform
               (Struct:Function*
                ,@function
                :body '((let ((variable self.property))))))
               :to-equal
               '((let ((variable (Struct:unsafe-get self :property)))))))

    (it "or"
      (expect (Emil:Syntax:transform
               (Struct:Function*
                ,@function
                :body '((or t self.property))))
              :to-equal
              '((or t (Struct:unsafe-get self :property)))))

    (it "prog1"
      (expect (Emil:Syntax:transform
               (Struct:Function*
                ,@function
                :body '((prog1 self.property self.property))))
              :to-equal
              '((prog1 (Struct:unsafe-get self :property)
                 (Struct:unsafe-get self :property)))))

    (it "progn like"
      (expect (Emil:Syntax:transform
               (Struct:Function*
                ,@function
                :body '((progn self.property self.property))))
              :to-equal
              '((progn (Struct:unsafe-get self :property)
                      (Struct:unsafe-get self :property)))))

    (it "quote"
      (expect (Emil:Syntax:transform
               (Struct:Function*
                ,@function
                :body '((quote self.property))))
              :to-equal
              '((quote self.property))))

    (it "setq"
      (expect (Emil:Syntax:transform
               (Struct:Function*
                ,@function
                :body '((setq variable self.property))))
              :to-equal
              '((setq variable (Struct:unsafe-get self :property)))))

    (it "unwind-protect"
      (expect (Emil:Syntax:transform
               (Struct:Function*
                ,@function
                :body '((unwind-protect self.property self.property))))
              :to-equal
              '((unwind-protect (Struct:unsafe-get self :property)
                  (Struct:unsafe-get self :property)))))

    (it "while"
      (expect (Emil:Syntax:transform
               (Struct:Function*
                ,@function
                :body '((while self.property self.property))))
              :to-equal
              '((while (Struct:unsafe-get self :property)
                  (Struct:unsafe-get self :property))))))

  (describe "method transformations"
    (after-each
      (Struct:undefine 'TestStruct)
      (Trait:undefine 'TestTrait)
      (Trait:undefine 'OtherTrait))

    (it "call in same struct implementation"
      (expect (eval '(progn
                       (Struct:define TestStruct)
                       (Struct:implement TestStruct
                         (fn f (self) (self.g))
                         (fn g (self) 0))))
              :to-equal 'TestStruct)
      (expect (eval '(TestStruct:f (TestStruct)))
              :to-equal 0))

    (it "call in same trait implementation"
      (expect (eval '(progn
                       (Trait:define TestTrait ()
                         (fn f (self) (self.g))
                         (fn g (self) 0))
                       (Struct:define TestStruct)
                       (Trait:implement TestTrait TestStruct)))
              :to-equal 'TestStruct)
      (expect (eval '(TestTrait:f (TestStruct)))
              :to-equal 0))

    (it "calling a trait method"
      (expect (eval '(progn
                       (Trait:define TestTrait ()
                         (fn f (self) 0))
                       (Struct:define TestStruct)
                       (Struct:implement TestStruct
                         (fn g (self) (self.f)))
                       (Trait:implement TestTrait TestStruct)))
              :to-equal 'TestStruct)
      (expect (eval '(TestStruct:g (TestStruct)))
              :to-equal 0))

    (xit "calling a super-trait method"
      (expect (eval '(progn
                       (Trait:define TestTrait ()
                         (fn f (self) 0))
                       (Trait:define OtherTrait (TestTrait)
                         (fn g (self) (self.f)))
                       (Struct:define TestStruct)
                       (Struct:implement TestStruct
                         (fn h (self) (self.g))
                         (fn i (self) (self.f)))
                       (Trait:implement TestTrait TestStruct)
                       (Trait:implement OtherTrait TestStruct)))
              :to-equal 'TestStruct)
      (expect (eval '(TestStruct:h (TestStruct)))
              :to-equal 0)
      (expect (eval '(TestStruct:i (TestStruct)))
              :to-equal 0))))
