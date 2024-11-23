;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'Lang/Struct)

(describe "Lang/Struct"
  (describe "with a basic struct"
    (before-each
      (Struct:define TestStruct
        "A test struct."
        (optional 0 "An optional property.")
        (required nil "A required property." :required t)
        (read-only 0 "A read-only property." :read-only t)))

    (after-each
      (Struct:undefine 'TestStruct))

    (it "defines a type"
      (let ((type (Struct:get-type 'TestStruct)))
        (expect type :to-be-truthy)
        (expect (Struct:get type :documentation)
                :to-equal
                "A test struct.")
        (expect (Struct:get type :name)
                :to-equal
                'TestStruct)
        (expect (length (Struct:get type :properties)) :to-equal 3)
        
        (let ((optional (nth 0 (Struct:get type :properties))))
          (expect (Struct:get optional :name)
                  :to-equal 'optional)
          (expect (Struct:get optional :default-value)
                  :to-equal 0)
          (expect (Struct:get optional :documentation)
                  :to-equal "An optional property.")
          (expect (Struct:get optional :required)
                  :to-equal nil)
          (expect (Struct:get optional :read-only)
                  :to-equal nil))

        (let ((required (nth 1 (Struct:get type :properties))))
          (expect (Struct:get required :name)
                  :to-equal 'required)
          (expect (Struct:get required :default-value)
                  :to-equal nil)
          (expect (Struct:get required :documentation)
                  :to-equal "A required property.")
          (expect (Struct:get required :required)
                  :to-equal t)
          (expect (Struct:get required :read-only)
                  :to-equal nil))

        (let ((read-only (nth 2 (Struct:get type :properties))))
          (expect (Struct:get read-only :name)
                  :to-equal 'read-only)
          (expect (Struct:get read-only :default-value)
                  :to-equal 0)
          (expect (Struct:get read-only :documentation)
                  :to-equal "A read-only property.")
          (expect (Struct:get read-only :required)
                  :to-equal nil)
          (expect (Struct:get read-only :read-only)
                  :to-equal t))))

    (it "defines constructors"
      (expect (fboundp 'TestStruct) :to-equal t)
      (expect (fboundp 'TestStruct*) :to-equal t))
    
    (it "can be constructed"
      (expect (TestStruct :optional 1 :required "string" :read-only 2)
              :to-equal
              `(TestStruct :optional 1 :required "string" :read-only 2)))

    (it "keeps properties ordered"
      (expect (TestStruct :read-only 0 :required "string" :optional 1)
              :to-equal
              `(TestStruct :optional 1 :required "string" :read-only 0)))

    (it "can provide default values"
      (expect (TestStruct :required "other")
              :to-equal
              `(TestStruct :optional 0 :required "other" :read-only 0)))

    (it "can not provide required values"
      (expect (TestStruct :optional 0 :read-only 0)
              :to-throw))

    (it "can use shorthand syntax"
      (let ((optional 1)
            (required 0))
        (expect (TestStruct optional required)
                :to-equal
                `(TestStruct :optional ,optional
                             :required ,required
                             :read-only 0))))

    (it "can use spread syntax"
      (let ((other (TestStruct :required 1)))
        (expect (TestStruct ,@other :optional 2)
                :to-equal
                `(TestStruct :optional 2 :required 1 :read-only 0))))

    (it "can get a property"
      (expect (Struct:get (TestStruct :required 1)
                          :required)
              :to-equal 1))

    (it "can not get an unknown property"
      (expect (Struct:get (TestStruct :required 1)
                          :unknown 2)
              :to-throw))

    (it "can set a property"
      (let ((struct (TestStruct :required 1)))
        (expect (Struct:set struct :optional 2)
                :to-equal 2)
        (expect (Struct:get struct :optional)
                :to-equal 2)))

    (it "can not set an unknown property"
      (expect (Struct:set (TestStruct :required 1)
                          :unknown 2)
              :to-throw))

    (it "can not set a read-only property"
      (expect (Struct:set (TestStruct :required 1)
                          :read-only 2)
              :to-throw))

    (it "can get all properties"
      (expect (Struct:properties (TestStruct :required 1))
              :to-equal
              `(:optional 0 :required 1 :read-only 0)))

    (it "can be undefined"
      (Struct:undefine 'TestStruct)
      (expect (Struct:get-type 'TestStruct) :to-throw 'error)
      (expect (fboundp 'TestStruct) :to-equal nil)
      (expect (fboundp 'TestStruct*) :to-equal nil))

    (it "can not undefine core types"
      (expect (Struct:undefine 'Struct:Type) :to-throw)
      (expect (Struct:undefine 'Struct:Property) :to-throw)))

  (describe "with a read-only struct"
    (before-each
      (Struct:define TestStruct
        "A test struct."
        :read-only t
        (property 0 "An optional property.")))

    (after-each
      (Struct:undefine 'TestStruct))

    (it "can not set any property"
      (expect (Struct:set (TestStruct) :property 1) :to-throw))))
