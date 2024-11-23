;; -*- lexical-binding: t -*-

(require 'Struct/Primitives)

(describe "Struct:Primitives"
  (before-each
    (Struct:define TestStruct
      (property-0) (property-1))
    (Struct:define OtherTestStruct
      (property-0) (property-1)))
  
  (after-each
    (Struct:undefine 'TestStruct)
    (Struct:undefine 'OtherTestStruct))

  (describe "Struct:name"
    (it "returns the name of a struct value"
      (expect (Struct:name (TestStruct))
              :to-equal 'TestStruct))

    (it "does not check if value is really a defined struct"
      (expect (Struct:name (list 'FakeTestStruct :property 0))
              :to-equal 'FakeTestStruct))

    (it "throws an error if value is not a struct"
      (expect (Struct:name 42)
              :to-throw 'wrong-type-argument)
      (expect (Struct:name (list :FakeTestStruct :property 0))
              :to-throw 'wrong-type-argument))))

