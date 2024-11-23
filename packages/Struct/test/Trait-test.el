;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'Trait)

;; Shutup "Optimization failure for cl-typep" warnings.
(put 'cl-typep 'compiler-macro nil)

(describe "Trait"
  (before-each
    (Struct:define TestStruct (property)))

  (after-each
    (Trait:undefine 'TestTrait)
    (Trait:undefine 'SuperTrait)
    (Trait:undefine 'OtherTrait)
    (Struct:undefine 'TestStruct))

  (describe "with a basic trait"
    (before-each
      (eval '(Trait:define TestTrait ()
        "TestTrait documentation."

        (fn TestTrait:required (self argument)
          "TestTrait:required documentation.")

        (fn TestTrait:optional(self argument)
          "TestTrait:optional documentation."
          (1+ argument))))

      (eval '(Trait:define OtherTrait ())))

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
          (expect (Struct:get required :function)
                  :to-equal
                  '(Struct:Function
                    :name TestTrait:required
                    :qualified-name TestTrait:required :arguments
                    ((Struct:Argument :name self :type nil :default nil
                                      :kind nil)
                     (Struct:Argument :name argument :type nil
                                      :default nil :kind nil))
                    :return-type nil
                    :documentation "TestTrait:required documentation."
                    :body nil
                    :filename nil))
          (expect (Struct:get required :default-implementation) :to-be nil)
          (expect (Struct:get required :implementations) :to-be nil)
          (expect (functionp (Struct:get required :dispatch-function))
                  :to-be t)

          (expect (Struct:get optional :function)
                  :to-equal
                  '(Struct:Function
                    :name TestTrait:optional
                    :qualified-name TestTrait:optional :arguments
                    ((Struct:Argument :name self :type nil :default nil
                                      :kind nil)
                     (Struct:Argument :name argument :type nil
                                      :default nil :kind nil))
                    :return-type nil
                    :documentation "TestTrait:optional documentation."
                    :body ((1+ argument))
                    :filename nil))
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
        (eval '(Trait:implement TestTrait TestStruct
          (fn TestTrait:required (self argument)
            (+ (Struct:get self :property) argument)))))

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
                :to-be nil))
      ))

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
                :to-throw 'wrong-type-argument
                '(cons foo form))
        (expect (macroexpand-all '(Trait:define TestTrait nil
                                    (cl-fn foo (self))))
                :to-throw 'error
                '("Function declaration should start with fn: (cl-fn foo (self))"))
        (expect (macroexpand-all '(Trait:define TestTrait nil
                                    (fn 42)))
                :to-throw 'error
                '("Function name should be a symbol: 42"))
        (expect (macroexpand-all '(Trait:define TestTrait nil
                                    (fn foo [])))
                :to-throw 'wrong-type-argument
                '(list [] form))
        (expect (macroexpand-all '(Trait:define TestTrait nil
                                    (fn foo nil)))
                :to-throw 'error
                '("A method requires at least one argument: foo"))
        (expect (macroexpand-all '(Trait:define TestTrait nil
                                    (fn foo (self) (declare (indent 1)))))
                :to-throw 'error
                '("Declare form not supported: (declare (indent 1))"))
        (expect (macroexpand-all '(Trait:define TestTrait nil
                                    (fn foo ((self TestTrait)))))
                :to-throw 'error
                '("Self argument of method can not be typed: foo"))))

    (describe "recognizes runtime-errors"
      (it "rejects undefined supertraits"
        (expect (eval '(Trait:define TestTrait (SuperTrait)))
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
                :to-throw 'wrong-type-argument
                '(cons 42 form))
        (expect (macroexpand-all '(Trait:implement TestTrait TestStruct
                                    (cl-fn foo ())))
                :to-throw 'error
                '("Function declaration should start with fn: (cl-fn foo nil)"))
        (expect (macroexpand-all '(Trait:implement TestTrait TestStruct
                                    (fn 42)))
                :to-throw 'error
                '("Function name should be a symbol: 42"))
        (expect (macroexpand-all '(Trait:implement TestTrait TestStruct
                                    (fn foo [])))
                :to-throw 'wrong-type-argument
                '(list [] form))
        (expect (macroexpand-all '(Trait:implement TestTrait TestStruct
                                    (fn foo () (declare (indent 1)))))
                :to-throw 'error
                '("Declare form not supported: (declare (indent 1))"))))

    (describe "recognizes runtime-errors"
      (before-each (eval '(Trait:define TestTrait ()
                            (fn TestTrait:required (self &optional argument)))))

      (it "rejects if required methods are not implemented"
        (expect (eval '(Trait:implement TestTrait TestStruct))
                :to-throw 'error
                '("Required method not implemented: TestTrait:required")))

      (it "rejects if non-trait methods are provided"
        (expect (eval '(Trait:implement TestTrait TestStruct
                  (fn TestTrait:required (self &optional argument))
                  (fn TestTrait:no-such-method (self))))
                :to-throw 'error
                '("Method not declared by this trait: TestTrait:no-such-method")))

      (it "rejects if method signatures are incompatible"
        (expect (eval '(Trait:implement TestTrait TestStruct
                  (fn TestTrait:required (self argument))))
                :to-throw 'error
                '("Signature incompatible with method declared by trait: TestTrait, TestTrait:required")))))

  (describe "with a supertrait"
    (eval '(before-each
      (eval '(Trait:define SuperTrait ()
               (fn SuperTrait:optional (self argument)
                 (1+ argument))))

      (eval '(Trait:define TestTrait (SuperTrait)))

      (eval '(Trait:implement SuperTrait TestStruct))
      (eval '(Trait:implement TestTrait TestStruct))))

    (it "rejects unimplemented supertraits"
      (expect (eval '(Trait:implement TestTrait OtherTestStruct))
              :to-throw 'error
              '("Required supertrait not implemented by type: SuperTrait")))

    (it "invokes a supertrait method"
      (expect (SuperTrait:optional (TestStruct) 1) :to-be 2)))

  (describe "with Struct:lambda features"
    (before-each
      (eval '(Trait:define TestTrait ()
               (fn TestTrait:with-number (self (arg number)))
               (fn TestTrait:with-struct (self (arg TestStruct)))
               (fn TestTrait:with-rest-struct (self &struct (arg TestStruct)))))

      (eval '(Trait:implement TestTrait TestStruct
        (fn TestTrait:with-number (self (arg number))
          (+ (Struct:get self :property 0) arg))
        (fn TestTrait:with-struct (self (arg TestStruct))
          (+ (Struct:get self :property 0)
             (Struct:get arg :property 0)))
        (fn TestTrait:with-rest-struct (self &struct (arg TestStruct))
          (+ (Struct:get self :property 0)
             (Struct:get arg :property 0))))))

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
