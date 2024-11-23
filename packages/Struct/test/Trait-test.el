;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'Trait)

;; Shutup "Optimization failure for cl-typep" warnings.
(put 'cl-typep 'compiler-macro nil)

(describe "Trait"
  (before-each
    (Struct:define TestStruct property))

  (after-each
    (Trait:undefine 'TestTrait)
    (Trait:undefine 'SuperTrait)
    (Trait:undefine 'OtherTrait)
    (Struct:undefine 'TestStruct))

  (describe "with a basic trait"
    (before-each
      (Trait:define TestTrait ()
        "TestTrait documentation."

        (defmethod TestTrait:required (self argument)
          "TestTrait:required documentation.")

        (defmethod TestTrait:optional(self argument)
          "TestTrait:optional documentation."
          (1+ argument)))

      (Trait:define OtherTrait ()))

    (it "defines a trait type"
      (let ((trait (Trait:get 'TestTrait :ensure)))
        (expect (Struct:get trait :name)
                :to-be 'TestTrait)
        (expect (length (Struct:get trait :methods))
                :to-be 2)
        (expect (Struct:get trait :implementing-types)
                :to-be nil)
        (expect (Struct:get trait :supertraits)
                :to-be nil)
        (let ((required (cdr (assq 'TestTrait:required
                                   (Struct:get trait :methods))))
              (optional (cdr (assq 'TestTrait:optional
                                   (Struct:get trait :methods)))))
          (expect (Struct:get required :name) :to-be 'TestTrait:required)
          (expect (Struct:get required :arguments) :to-equal '(self argument))
          (expect (Struct:get required :documentation)
                  :to-equal
                  "TestTrait:required documentation.")
          (expect (Struct:get required :default-implementation) :to-be nil)
          (expect (Struct:get required :implementations) :to-be nil)
          (expect (functionp (Struct:get required :dispatch-function))
                  :to-be t)

          (expect (Struct:get optional :name) :to-be 'TestTrait:optional)
          (expect (Struct:get optional :arguments) :to-equal '(self argument))
          (expect (Struct:get optional :documentation)
                  :to-equal
                  "TestTrait:optional documentation.")
          (expect (functionp
                   (Struct:get optional :default-implementation))
                  :to-be t)
          (expect (Struct:get optional :implementations) :to-be nil)
          (expect (functionp (Struct:get optional :dispatch-function))
                  :to-be t))))

    (it "can be undefined"
      (expect (Trait:get 'TestTrait) :not :to-be nil)
      (Trait:undefine 'TestTrait)
      (expect (Trait:get 'TestTrait) :to-be nil)
      (expect (get 'TestTrait 'function-documentation)
              :to-be nil)
      (expect (fboundp 'TestTrait:required) :to-be nil)
      (expect (get 'TestTrait:required 'function-documentation)
              :to-be nil)
      (expect (fboundp 'TestTrait:optional) :to-be nil)
      (expect (get 'TestTrait:optional 'function-documentation)
              :to-be nil))

    (describe "with an implementation"
      (before-each
        (Trait:implement TestTrait TestStruct
          (defmethod TestTrait:required (self argument)
            (+ (Struct:get self :property) argument))))

      (it "can invoke a default method"
        (expect (TestTrait:optional (TestStruct) 2)
                :to-be
                3))

      (it "can invoke a required method"
        (expect (TestTrait:required (TestStruct :property 2) 2)
                :to-be
                4))

      (it "can be used with cl's type-system"
        (expect (cl-check-type (TestStruct) (Trait TestTrait))
                :to-be nil)
        (expect (cl-check-type (TestStruct) (Trait OtherTrait))
                :to-throw 'wrong-type-argument '((Trait OtherTrait) (TestStruct :property nil) (TestStruct)))
        (expect (cl-check-type "TestStruct" (Trait TestTrait))
                :to-throw 'wrong-type-argument '((Trait TestTrait) "TestStruct" "TestStruct"))

        (expect (cl-typep (TestStruct) '(Trait TestTrait))
                :to-be t)
        (expect (cl-typep (TestStruct) '(Trait OtherTrait))
                :to-be nil)
        (expect (cl-typep "TestStruct" '(Trait TestTrait))
                :to-be nil))))

  (describe "Trait:define"
    (describe "recognizes syntax-errors"
      (it "rejects non-symbol trait-names"
        (expect (macroexpand-all '(Trait:define 0 nil))
                :to-throw 'wrong-type-argument))

      (it "rejects invalid supertraits declaration"
        (expect (macroexpand-all '(Trait:define TestTrait ("SuperTrait")))
                :to-throw 'wrong-type-argument)
        (expect (macroexpand-all '(Trait:define TestTrait SuperTrait))
                :to-throw 'wrong-type-argument))

      (it "rejects illegal method-forms"
        (expect (macroexpand-all '(Trait:define TestTrait nil
                                    foo))
                :to-throw 'error
                '("Method definition should be a non-empty list: foo"))
        (expect (macroexpand-all '(Trait:define TestTrait nil
                                    (cl-defmethod foo (self))))
                :to-throw 'error
                '("Method declaration should start with defmethod: cl-defmethod"))
        (expect (macroexpand-all '(Trait:define TestTrait nil
                                    (defmethod 42)))
                :to-throw 'error
                '("Method name should be a symbol: 42"))
        (expect (macroexpand-all '(Trait:define TestTrait nil
                                    (defmethod foo [])))
                :to-throw 'wrong-type-argument
                '(listp []))
        (expect (macroexpand-all '(Trait:define TestTrait nil
                                    (defmethod foo nil)))
                :to-throw 'error
                '("Trait method must accept at least one argument: foo"))
        (expect (macroexpand-all '(Trait:define TestTrait nil
                                    (defmethod foo (self) (declare (indent 1)))))
                :to-throw 'error
                '("Declare not supported for methods"))
        (expect (macroexpand-all '(Trait:define TestTrait nil
                                    (defmethod foo ((self TestTrait)))))
                :to-throw 'error
                '("First argument can not be typed: (self TestTrait)"))))

    (describe "recognizes runtime-errors"
      (it "rejects undefined supertraits"
        (expect (Trait:define TestTrait (SuperTrait))
                :to-throw 'wrong-type-argument))))

  (describe "Trait:implement"
    (describe "recognizes syntax-errors"
      (it "rejects non symbol traits"
        (expect (macroexpand-all '(Trait:implement 42 TestStruct))
                :to-throw 'wrong-type-argument)
        (expect (macroexpand-all '(Trait:implement [] TestStruct))
                :to-throw 'wrong-type-argument))

      (it "rejects non symbol types"
        (expect (macroexpand-all '(Trait:implement TestTrait 42))
                :to-throw 'wrong-type-argument)
        (expect (macroexpand-all '(Trait:implement TestTrait []))
                :to-throw 'wrong-type-argument))

      (it "rejects illegal method-forms"
        (expect (macroexpand-all '(Trait:implement TestTrait TestStruct 42))
                :to-throw 'error
                '("Expected a non-empty list: 42"))
        (expect (macroexpand-all '(Trait:implement TestTrait TestStruct
                                    (cl-defmethod foo ())))
                :to-throw 'error
                '("Method implementation should start with defmethod: cl-defmethod"))
        (expect (macroexpand-all '(Trait:implement TestTrait TestStruct
                                    (defmethod 42)))
                :to-throw 'error
                '("Method name should be a symbol: 42"))
        (expect (macroexpand-all '(Trait:implement TestTrait TestStruct
                                    (defmethod foo [])))
                :to-throw 'error
                '("Invalid method argument-list declaration: []"))
        (expect (macroexpand-all '(Trait:implement TestTrait TestStruct
                                    (defmethod foo () (declare (indent 1)))))
                :to-throw 'error
                '("Declare not supported for methods"))))

    (describe "recognizes runtime-errors"
      (before-each (Trait:define TestTrait ()
                     (defmethod TestTrait:required (self &optional argument))))

      (it "rejects if required methods are not implemented"
        (expect (Trait:implement TestTrait TestStruct)
                :to-throw 'error
                '("Required method not implemented: TestTrait:required")))

      (it "rejects if non-trait methods are provided"
        (expect (Trait:implement TestTrait TestStruct
                  (defmethod TestTrait:required (self &optional argument))
                  (defmethod TestTrait:no-such-method (self)))
                :to-throw 'error
                '("Method not declared by this trait: TestTrait:no-such-method")))

      (it "rejects if method signatures are incompatible"
        (expect (Trait:implement TestTrait TestStruct
                  (defmethod TestTrait:required (self argument)))
                :to-throw 'error
                '("Signature incompatible with method declared by trait: TestTrait:required, (self &optional argument), (self argument)")))))

  (describe "with a supertrait"
    (before-each
      (Trait:define SuperTrait ()
        (defmethod SuperTrait:optional (self argument)
          (1+ argument)))

      (Trait:define TestTrait (SuperTrait))

      (Trait:implement SuperTrait TestStruct)
      (Trait:implement TestTrait TestStruct))

    (it "rejects unimplemented supertraits"
      (expect (Trait:implement TestTrait OtherTestStruct)
              :to-throw 'error
              '("Required supertrait not implemented by type: SuperTrait")))

    (it "invokes a supertrait method"
      (expect (SuperTrait:optional (TestStruct) 1) :to-be 2)))

  (describe "with Struct:lambda features"
    (before-each
      (Trait:define TestTrait ()
        (defmethod TestTrait:with-number (self (arg number)))
        (defmethod TestTrait:with-struct (self (arg TestStruct)))
        (defmethod TestTrait:with-rest-struct (self &struct (arg TestStruct))))

      (Trait:implement TestTrait TestStruct
        (defmethod TestTrait:with-number (self (arg number))
          (+ (Struct:get self :property 0) arg))
        (defmethod TestTrait:with-struct (self (arg TestStruct))
          (+ (Struct:get self :property 0)
             (Struct:get arg :property 0)))
        (defmethod TestTrait:with-rest-struct (self &struct (arg TestStruct))
          (+ (Struct:get self :property 0)
             (Struct:get arg :property 0)))))
    
    (it "can use types in methods"
      (expect (TestTrait:with-number (TestStruct) 1)
              :to-be 1)
      (expect (TestTrait:with-number (TestStruct :property 1) 2)
              :to-be 3)
      (expect (TestTrait:with-struct (TestStruct) (TestStruct))
              :to-be 0)
      (expect (TestTrait:with-struct (TestStruct :property 1)
                                     (TestStruct :property 2))
              :to-be 3)
      (expect (TestTrait:with-rest-struct (TestStruct))
              :to-be 0)
      (expect (TestTrait:with-rest-struct
               (TestStruct :property 1) :property 2)
              :to-be 3)
      (expect (TestTrait:with-rest-struct
               (TestStruct :property 1)
               (TestStruct :property 2))
              :to-be 3))

    (it "can use reject wrong types in methods"
      (expect (TestTrait:with-number (TestStruct) "1")
              :to-throw 'wrong-type-argument
              '(number "1" arg))
      (expect (TestTrait:with-number 1 2)
              :to-throw 'error
              '("Type does not implement trait: integer, TestTrait"))
      (expect (TestTrait:with-struct (TestStruct) 1)
              :to-throw 'wrong-type-argument
              '(TestStruct 1 arg))
      (expect (TestTrait:with-rest-struct (TestStruct) :no-such-property 1)
              :to-throw 'error
              '("Undeclared properties set: (:no-such-property 1)")))))
