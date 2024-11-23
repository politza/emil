;; -*- lexical-binding: t -*-

(require 'Struct/Primitives)

(describe "Struct:Primitives"
  (before-each
    (eval '(Struct:define TestStruct
             (property-0) (property-1)))
    (eval '(Struct:define OtherTestStruct
             (property-0) (property-1))))
  
  (after-each
    (Struct:undefine 'TestStruct)
    (Struct:undefine 'OtherTestStruct))

  (describe "Struct:name"
    (it "returns the name of a struct value"
      (expect (Struct:name (TestStruct))
              :to-equal 'TestStruct))

    (it "does check if value is not a defined struct"
      (expect (Struct:name (list 'FakeTestStruct :property 0))
              :to-equal nil))

    (it "returns nil if value is not a struct"
      (expect (Struct:name 42)
              :to-equal nil)
      (expect (Struct:name (list :FakeTestStruct :property 0))
              :to-equal nil))))

