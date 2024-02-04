;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'struct)

(describe "struct"
  (describe "with a basic struct"
    (before-each
      (struct-define test-struct
        "A test struct."
        (optional 0 "An optional property.")
        (required nil "A required property." :required t)
        (read-only 0 "A read-only property." :read-only t)))

    (after-each
      (struct-undefine 'test-struct))

    (it "defines a type"
      (let ((type (struct-get-type 'test-struct)))
        (expect type :to-be-truthy)
        (expect (struct-get type :documentation)
                :to-equal
                "A test struct.")
        (expect (struct-get type :name)
                :to-equal
                'test-struct)
        (expect (length (struct-get type :properties)) :to-equal 3)
        
        (let ((optional (nth 0 (struct-get type :properties))))
          (expect (struct-get optional :name)
                  :to-equal 'optional)
          (expect (struct-get optional :default-value)
                  :to-equal 0)
          (expect (struct-get optional :documentation)
                  :to-equal "An optional property.")
          (expect (struct-get optional :required)
                  :to-equal nil)
          (expect (struct-get optional :read-only)
                  :to-equal nil))

        (let ((required (nth 1 (struct-get type :properties))))
          (expect (struct-get required :name)
                  :to-equal 'required)
          (expect (struct-get required :default-value)
                  :to-equal nil)
          (expect (struct-get required :documentation)
                  :to-equal "A required property.")
          (expect (struct-get required :required)
                  :to-equal t)
          (expect (struct-get required :read-only)
                  :to-equal nil))

        (let ((read-only (nth 2 (struct-get type :properties))))
          (expect (struct-get read-only :name)
                  :to-equal 'read-only)
          (expect (struct-get read-only :default-value)
                  :to-equal 0)
          (expect (struct-get read-only :documentation)
                  :to-equal "A read-only property.")
          (expect (struct-get read-only :required)
                  :to-equal nil)
          (expect (struct-get read-only :read-only)
                  :to-equal t))))

    (it "defines constructors"
      (expect (fboundp 'test-struct) :to-equal t)
      (expect (fboundp 'test-struct*) :to-equal t))
    
    (it "can be constructed"
      (expect (test-struct :optional 1 :required "string" :read-only 2)
              :to-equal
              `(test-struct :optional 1 :required "string" :read-only 2)))

    (it "keeps properties ordered"
      (expect (test-struct :read-only 0 :required "string" :optional 1)
              :to-equal
              `(test-struct :optional 1 :required "string" :read-only 0)))

    (it "can provide default values"
      (expect (test-struct :required "other")
              :to-equal
              `(test-struct :optional 0 :required "other" :read-only 0)))

    (it "can not provide required values"
      (expect (test-struct :optional 0 :read-only 0)
              :to-throw))

    (it "can use shorthand syntax"
      (let ((optional 1)
            (required 0))
        (expect (test-struct optional required)
                :to-equal
                `(test-struct :optional ,optional
                             :required ,required
                             :read-only 0))))

    (it "can use spread syntax"
      (let ((other (test-struct :required 1)))
        (expect (test-struct ,@other :optional 2)
                :to-equal
                `(test-struct :optional 2 :required 1 :read-only 0))))

    (it "can get a property"
      (expect (struct-get (test-struct :required 1)
                          :required)
              :to-equal 1))

    (it "can not get an unknown property"
      (expect (struct-get (test-struct :required 1)
                          :unknown 2)
              :to-throw))

    (it "can set a property"
      (let ((struct (test-struct :required 1)))
        (expect (struct-set struct :optional 2)
                :to-equal 2)
        (expect (struct-get struct :optional)
                :to-equal 2)))

    (it "can not set an unknown property"
      (expect (struct-set (test-struct :required 1)
                          :unknown 2)
              :to-throw))

    (it "can not set a read-only property"
      (expect (struct-set (test-struct :required 1)
                          :read-only 2)
              :to-throw))

    (it "can get all properties"
      (expect (struct-properties (test-struct :required 1))
              :to-equal
              `(:optional 0 :required 1 :read-only 0)))

    (it "can be undefined"
      (struct-undefine 'test-struct)
      (expect (struct-get-type 'test-struct) :to-throw 'error)
      (expect (fboundp 'test-struct) :to-equal nil)
      (expect (fboundp 'test-struct*) :to-equal nil))

    (it "can not undefine core types"
      (expect (struct-undefine 'struct-type) :to-throw)
      (expect (struct-undefine 'struct-property) :to-throw)))

  (describe "with a read-only struct"
    (before-each
      (struct-define test-struct
        "A test struct."
        :read-only t
        (property 0 "An optional property.")))

    (after-each
      (struct-undefine 'test-struct))

    (it "can not set any property"
      (expect (struct-set (test-struct) :property 1) :to-throw))))
