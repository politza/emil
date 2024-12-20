;; -*- lexical-binding: t -*-

;; Copyright (C) 2024  Andreas Politz

;; Author: Andreas Politz

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

(require 'Emil)
(require 'Struct/Function)
(require 'buttercup)

(describe "Emil:Syntax"
  (describe "Emil:Syntax:transform-syntax"
    :var ((function (Struct:Function
                     :name 'method
                     :qualified-name 'TestStruct:method
                     :arguments (list (Struct:Argument:read '(self TestStruct)))
                     :return-type 'number
                     :body nil)))

    (before-each
      (eval '(Struct:define TestStruct (property :type integer :mutable t)))
      (eval '(Struct:implement TestStruct (fn method (self)))))

    (after-each
      (Struct:undefine 'TestStruct))

    (it "property access"
      (expect (Emil:Syntax:transform-syntax
               (Struct:Function*
                ,@function
                :body '(self.property)))
              :to-equal
              '((Struct:unsafe-get self :property))))

    (it "method call"
      (expect (Emil:Syntax:transform-syntax
               (Struct:Function*
                ,@function
                :body '((self.method))))
              :to-equal
              '((TestStruct:method self))))

    (it "method reference"
      (expect (Emil:Syntax:transform-syntax
               (Struct:Function*
                ,@function
                :body '((funcall #'self.method))))
              :to-equal
              '((funcall #'(lambda nil (TestStruct:method self))))))

    (it "property assignment"
      (expect (Emil:Syntax:transform-syntax
               (Struct:Function*
                ,@function
                :body '((setf self.property 1))))
              :to-equal
              '((Struct:unsafe-set self :property 1))))

    (it "property assignment of wrong type"
      (expect (Emil:Syntax:transform-syntax
               (Struct:Function*
                ,@function
                :body '((setf self.property "1"))))
              :to-throw
              'Emil:type-error))

    (it "string"
      (expect (Emil:Syntax:transform-syntax
               (Struct:Function*
                ,@function
                :body '("a string")))
              :to-equal
              '("a string")))

    (it "number"
      (expect (Emil:Syntax:transform-syntax
               (Struct:Function*
                ,@function
                :body '(1)))
              :to-equal
              '(1)))

    (it "vector"
      (expect (Emil:Syntax:transform-syntax
               (Struct:Function*
                ,@function
                :body '([a vector])))
              :to-equal
              '([a vector])))

    (it "symbol"
      (expect (Emil:Syntax:transform-syntax
               (Struct:Function*
                ,@function
                :body '(self)))
              :to-equal
              '(self)))

    (it "application"
      (expect (Emil:Syntax:transform-syntax
               (Struct:Function*
                ,@function
                :body '((+ 1 self.property))))
              :to-equal
              '((+ 1 (Struct:unsafe-get self :property)))))

    (it "and"
      (expect (Emil:Syntax:transform-syntax
               (Struct:Function*
                ,@function
                :body '((and t self.property))))
              :to-equal
              '((and t (Struct:unsafe-get self :property)))))

    (it "catch"
      (expect (Emil:Syntax:transform-syntax
               (Struct:Function*
                ,@function
                :body '((catch 'tag (throw 'tag self.property)))))
              :to-equal
              '((catch 'tag (throw 'tag (Struct:unsafe-get self :property))))))

    (it "cond"
      (expect (Emil:Syntax:transform-syntax
               (Struct:Function*
                ,@function
                :body '((cond (self.property self.property)))))
              :to-equal
              '((cond ((Struct:unsafe-get self :property)
                       (Struct:unsafe-get self :property))))))

    (it "condition-case"
      (expect (Emil:Syntax:transform-syntax
               (Struct:Function*
                ,@function
                :body '((condition-case variable
                            self.property
                          (error
                           self.property
                           variable)))))
              :to-equal
              '((condition-case variable
                    (Struct:unsafe-get self :property)
                  (error (Struct:unsafe-get self :property)
                         variable)))))

    (it "defconst"
      (expect (Emil:Syntax:transform-syntax
               (Struct:Function*
                ,@function
                :body '((defconst symbol self.property "documentation"))))
              :to-equal
              '((defconst symbol (Struct:unsafe-get self :property) "documentation"))))

    (it "defvar"
      (expect (Emil:Syntax:transform-syntax
               (Struct:Function*
                ,@function
                :body '((defvar symbol self.property "documentation"))))
              :to-equal
              '((defvar symbol (Struct:unsafe-get self :property) "documentation"))))

    (it "function"
      (expect (Emil:Syntax:transform-syntax
               (Struct:Function*
                ,@function
                :body '((function self.method))))
              :to-equal
              '((function (lambda nil (TestStruct:method self))))))

    (it "if"
      (expect (Emil:Syntax:transform-syntax
               (Struct:Function*
                ,@function
                :body '((if self.property self.property self.property))))
              :to-equal
              '((if (Struct:unsafe-get self :property)
                    (Struct:unsafe-get self :property)
                  (Struct:unsafe-get self :property)))))

    (xit "interactive"
      ;; FIXME: interactive not properly implemented.
      (expect (Emil:Syntax:transform-syntax
               (Struct:Function*
                ,@function
                :body '((interactive self.property))))
              :to-equal
              '((interactive (Struct:unsafe-get self :property)))))

    (it "let"
      (expect (Emil:Syntax:transform-syntax
               (Struct:Function*
                ,@function
                :body '((let ((variable self.property))))))
              :to-equal
              '((let ((variable (Struct:unsafe-get self :property)))))))

    (it "or"
      (expect (Emil:Syntax:transform-syntax
               (Struct:Function*
                ,@function
                :body '((or t self.property))))
              :to-equal
              '((or t (Struct:unsafe-get self :property)))))

    (it "prog1"
      (expect (Emil:Syntax:transform-syntax
               (Struct:Function*
                ,@function
                :body '((prog1 self.property self.property))))
              :to-equal
              '((prog1 (Struct:unsafe-get self :property)
                  (Struct:unsafe-get self :property)))))

    (it "progn like"
      (expect (Emil:Syntax:transform-syntax
               (Struct:Function*
                ,@function
                :body '((progn self.property self.property))))
              :to-equal
              '((progn (Struct:unsafe-get self :property)
                       (Struct:unsafe-get self :property)))))

    (it "quote"
      (expect (Emil:Syntax:transform-syntax
               (Struct:Function*
                ,@function
                :body '((quote self.property))))
              :to-equal
              '((quote self.property))))

    (it "setq"
      (expect (Emil:Syntax:transform-syntax
               (Struct:Function*
                ,@function
                :body '((setq variable self.property))))
              :to-equal
              '((setq variable (Struct:unsafe-get self :property)))))

    (it "unwind-protect"
      (expect (Emil:Syntax:transform-syntax
               (Struct:Function*
                ,@function
                :body '((unwind-protect self.property self.property))))
              :to-equal
              '((unwind-protect (Struct:unsafe-get self :property)
                  (Struct:unsafe-get self :property)))))

    (it "while"
      (expect (Emil:Syntax:transform-syntax
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

    (it "calling a super-trait method"
      (expect (eval '(progn
                       (Trait:define TestTrait ()
                         (fn f (self) 0))
                       (Trait:define OtherTrait (TestTrait)
                         (fn g (self) (self.f)))
                       (Struct:define TestStruct)
                       (Trait:implement TestTrait TestStruct)
                       (Trait:implement OtherTrait TestStruct)
                       (Struct:implement TestStruct
                         (fn h (self) (self.g))
                         (fn i (self) (self.f)))))
              :to-equal 'TestStruct)
      (expect (eval '(TestStruct:h (TestStruct)))
              :to-equal 0)
      (expect (eval '(TestStruct:i (TestStruct)))
              :to-equal 0)))

  (describe "property handling"
    (after-each
      (Struct:undefine 'TestStruct)
      (Struct:undefine 'OtherStruct))

    (it "updating a member's property"
      (expect (eval '(progn
                       (Struct:define OtherStruct (property :type number :mutable t))
                       (Struct:define TestStruct (other :type OtherStruct))
                       (Struct:implement TestStruct
                         (fn set (self (value number))
                           (setf self.other.property value))
                         (fn get (self)
                           self.other.property))))
              :to-equal 'TestStruct)
      (expect (eval '(let ((struct (TestStruct :other (OtherStruct :property 0))))
                       (TestStruct:set struct 1)
                       (TestStruct:get struct)))
              :to-equal 1))

    (it "updating a read-only property"
      (expect (eval '(progn
                       (Struct:define TestStruct (property :type number))
                       (Struct:implement TestStruct
                         (fn set (self (value number))
                           (setf self.property value)))))
              :to-throw 'Emil:type-error))))
