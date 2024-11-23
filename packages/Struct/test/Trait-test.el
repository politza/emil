;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'Trait)

;; Shutup "Optimization failure for cl-typep" warnings.
(put 'cl-typep 'compiler-macro nil)

(describe "Trait"
  (before-each
    (eval '(Struct:define TestStruct (property))))

  (after-each
    (Trait:undefine 'TestTrait)
    (Trait:undefine 'SuperTrait)
    (Trait:undefine 'OtherTrait)
    (Struct:undefine 'TestStruct))

  (describe "with a basic trait"
    (before-each
      (eval '(Trait:define TestTrait ()
               "TestTrait documentation."
               :disable-syntax t

               (fn TestTrait:required (self argument)
                 "TestTrait:required documentation.")

               (fn TestTrait:optional (self argument)
                 "TestTrait:optional documentation."
                 (1+ argument))))

      (eval '(Trait:define OtherTrait ()
               :disable-syntax t)))

    (it "defines a trait type"
      (let ((trait (Trait:get 'TestTrait :ensure)))
        (expect (Struct:get trait :name)
                :to-be 'TestTrait)
        (expect (length (Struct:get trait :functions))
                :to-be 2)
        (expect (Struct:get trait :implementing-types)
                :to-be nil)
        (expect (Struct:get trait :supertraits)
                :to-be nil)
        (let ((required (cdr (assq 'TestTrait:required
                                   (Struct:get trait :functions))))
              (optional (cdr (assq 'TestTrait:optional
                                   (Struct:get trait :functions)))))
          (expect (Struct:get required :function)
                  :to-equal
                  '(Struct:Function
                    :name TestTrait:required
                    :qualified-name TestTrait:required :arguments
                    ((Struct:Argument :name self :type (Trait TestTrait) :default nil
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
                    ((Struct:Argument :name self :type (Trait TestTrait) :default nil
                                      :kind nil)
                     (Struct:Argument :name argument :type nil
                                      :default nil :kind nil))
                    :return-type nil
                    :documentation "TestTrait:optional documentation."
                    :body nil
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
                 :disable-syntax t
                 (fn TestTrait:required (self argument)
                   (+ (Struct:get self :property) argument)))))

      (it "can invoke a default function"
        (expect (TestTrait:optional (TestStruct) 2)
                :to-be
                3))

      (it "can invoke a required function"
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

      (it "rejects non-implementing types at runtime"
        (expect (TestTrait:optional (record 'TestRecord) 0)
                :to-throw
                'error '("Type does not implement trait: TestRecord, TestTrait"))
        (expect (TestTrait:required (record 'TestRecord) 0)
                :to-throw
                'error '("Type does not implement trait: TestRecord, TestTrait")))))

  (describe "Trait:define"
    (describe "recognizes syntax-errors"
      (it "rejects non-symbol trait-names"
        (expect (macroexpand-all '(Trait:define 0 nil
                                    :disable-syntax t))
                :to-throw 'wrong-type-argument))

      (it "rejects invalid supertraits declaration"
        (expect (macroexpand-all '(Trait:define TestTrait ("SuperTrait")
                                    :disable-syntax t))
                :to-throw 'wrong-type-argument)
        (expect (macroexpand-all '(Trait:define TestTrait SuperTrait
                                    :disable-syntax t))
                :to-throw 'wrong-type-argument))

      (it "rejects illegal function-forms"
        (expect (macroexpand-all '(Trait:define TestTrait nil
                                    :disable-syntax t
                                    foo))
                :to-throw 'wrong-type-argument
                '(cons foo form))
        (expect (macroexpand-all '(Trait:define TestTrait nil
                                    :disable-syntax t
                                    (cl-fn foo (self))))
                :to-throw 'error
                '("Function declaration should start with fn: (cl-fn foo (self))"))
        (expect (macroexpand-all '(Trait:define TestTrait nil
                                    :disable-syntax t
                                    (fn 42)))
                :to-throw 'error
                '("Function name should be a symbol: 42"))
        (expect (macroexpand-all '(Trait:define TestTrait nil
                                    :disable-syntax t
                                    (fn foo [])))
                :to-throw 'wrong-type-argument
                '(list [] form))
        (expect (macroexpand-all '(Trait:define TestTrait nil
                                    :disable-syntax t
                                    (fn foo nil)))
                :to-throw 'error
                '("A function requires at least one argument: foo"))
        (expect (macroexpand-all '(Trait:define TestTrait nil
                                    :disable-syntax t
                                    (fn foo (self) (declare (indent 1)))))
                :to-throw 'error
                '("Declare form not supported: (declare (indent 1))"))
        (expect (macroexpand-all '(Trait:define TestTrait nil
                                    :disable-syntax t
                                    (fn foo ((self (Trait TestTrait))))))
                :not :to-throw)
        (expect (macroexpand-all '(Trait:define TestTrait nil
                                    :disable-syntax t
                                    (fn foo ((self number)))))
                :to-throw 'error
                '("Dispatch argument of trait-function must have trait-type (Trait TestTrait): foo"))

        (expect (macroexpand-all '(Trait:define TestTrait nil
                                    :disable-syntax t
                                    (fn foo ((self (Trait TestTrait) :default)))))
                :to-throw 'error
                '("Dispatch argument of trait-function can not have a default: foo"))))

    (describe "recognizes runtime-errors"
      (it "rejects undefined supertraits"
        (expect (eval '(Trait:define TestTrait (SuperTrait)
                         :disable-syntax t))
                :to-throw 'wrong-type-argument))))

  (describe "Trait:implement"
    (describe "recognizes syntax-errors"
      (it "rejects non symbol traits"
        (expect (macroexpand-all '(Trait:implement 42 TestStruct
                                    :disable-syntax t))
                :to-throw 'wrong-type-argument)
        (expect (macroexpand-all '(Trait:implement [] TestStruct
                                    :disable-syntax t))
                :to-throw 'wrong-type-argument))

      (it "rejects non symbol types"
        (expect (macroexpand-all '(Trait:implement TestTrait 42
                                    :disable-syntax t))
                :to-throw 'wrong-type-argument)
        (expect (macroexpand-all '(Trait:implement TestTrait []
                                    :disable-syntax t))
                :to-throw 'wrong-type-argument))

      (it "rejects illegal function-forms"
        (expect (macroexpand-all '(Trait:implement TestTrait TestStruct
                                    :disable-syntax t
                                    42))
                :to-throw 'wrong-type-argument
                '(cons 42 form))
        (expect (macroexpand-all '(Trait:implement TestTrait TestStruct
                                    :disable-syntax t
                                    (cl-fn foo ())))
                :to-throw 'error
                '("Function declaration should start with fn: (cl-fn foo nil)"))
        (expect (macroexpand-all '(Trait:implement TestTrait TestStruct
                                    :disable-syntax t
                                    (fn 42)))
                :to-throw 'error
                '("Function name should be a symbol: 42"))
        (expect (macroexpand-all '(Trait:implement TestTrait TestStruct
                                    :disable-syntax t
                                    (fn foo [])))
                :to-throw 'wrong-type-argument
                '(list [] form))
        (expect (macroexpand-all '(Trait:implement TestTrait TestStruct
                                    :disable-syntax t
                                    (fn foo () (declare (indent 1)))))
                :to-throw 'error
                '("Declare form not supported: (declare (indent 1))"))))

    (describe "recognizes runtime-errors"
      (before-each (eval '(Trait:define TestTrait ()
                            :disable-syntax t
                            (fn TestTrait:required (self &optional argument)))))

      (it "rejects if required functions are not implemented"
        (expect (eval '(Trait:implement TestTrait TestStruct
                         :disable-syntax t))
                :to-throw 'error
                '("Required function not implemented: TestTrait:required")))

      (it "rejects if non-trait functions are provided"
        (expect (eval '(Trait:implement TestTrait TestStruct
                         :disable-syntax t
                         (fn TestTrait:required (self &optional argument))
                         (fn TestTrait:no-such-function (self))))
                :to-throw 'error
                '("Function not declared by this trait: TestTrait:no-such-function")))

      (it "rejects if function signatures are incompatible"
        (expect (eval '(Trait:implement TestTrait TestStruct
                         :disable-syntax t
                         (fn TestTrait:required (self argument more))))
                :to-throw 'error
                '("Signature incompatible with function declared by trait: TestTrait, TestTrait:required"))
        (expect (eval '(Trait:implement TestTrait TestStruct
                         :disable-syntax t
                         (fn TestTrait:required ((self string)))))
                :to-throw 'error
                '("Dispatch argument of trait-function must have implementors type TestStruct: TestTrait:required")))))

  (describe "with a supertrait"
    (eval '(before-each
             (eval '(Trait:define SuperTrait ()
                      :disable-syntax t
                      (fn SuperTrait:optional (self argument)
                        (1+ argument))))

             (eval '(Trait:define TestTrait (SuperTrait)
                      :disable-syntax t))

             (eval '(Trait:implement SuperTrait TestStruct
                      :disable-syntax t))
             (eval '(Trait:implement TestTrait TestStruct
                      :disable-syntax t))))

    (it "rejects unimplemented supertraits"
      (expect (eval '(Trait:implement TestTrait OtherTestStruct
                       :disable-syntax t))
              :to-throw 'error
              '("Required supertrait not implemented by type: SuperTrait")))

    (it "invokes a supertrait function"
      (expect (SuperTrait:optional (TestStruct) 1) :to-be 2)))

  (describe "with Struct:lambda features"
    (before-each
      (eval '(Trait:define TestTrait ()
               :disable-syntax t
               (fn TestTrait:with-number (self (arg number)))
               (fn TestTrait:with-struct (self (arg TestStruct)))
               (fn TestTrait:with-rest-struct (self &struct (arg TestStruct)))))

      (eval '(Trait:implement TestTrait TestStruct
               :disable-syntax t
               (fn TestTrait:with-number (self (arg number))
                 (+ (Struct:get self :property 0) arg))
               (fn TestTrait:with-struct (self (arg TestStruct))
                 (+ (Struct:get self :property 0)
                    (Struct:get arg :property 0)))
               (fn TestTrait:with-rest-struct (self &struct (arg TestStruct))
                 (+ (Struct:get self :property 0)
                    (Struct:get arg :property 0))))))

    (it "can use types in functions"
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

    (it "rejects wrong types in functions"
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
              :to-throw 'error)))

  (describe "Trait:extends?"
    (before-each
      (eval '(progn
               (Trait:define SuperTrait ())
               (Trait:define SubTrait (SuperTrait))
               (Trait:define OtherTrait ()))))

    (after-each
      (Trait:undefine 'SuperTrait)
      (Trait:undefine 'SubTrait)
      (Trait:undefine 'OtherTrait))

    (it "a trait does not extend itself"
      (expect (Trait:extends? 'OtherTrait 'OtherTrait)
              :to-be nil))

    (it "a non-trait does not extend anything"
      (expect (Trait:extends? 'NoSuchTrait 'OtherTrait)
              :to-be nil))

    (it "a sub-trait does extend a super-trait"
      (expect (Trait:extends? 'SubTrait 'SuperTrait)
              :to-be t))

    (it "a super-trait does extend a sub-trait"
      (expect (Trait:extends? 'SuperTrait 'SubTrait)
              :to-be nil))))
