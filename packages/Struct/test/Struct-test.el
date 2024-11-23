;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'Struct)

;; Shutup "Optimization failure for cl-typep" warnings.
(put 'cl-typep 'compiler-macro nil)

(describe "Struct"
  (after-each
      (Struct:undefine 'TestStruct))

  (describe "with a basic struct"
    (before-each
      (Struct:define TestStruct
        "A test struct."
        (optional
         "An optional property."
         :default-value 0 :mutable t)
        (required
         "A required property."
         :required t :mutable t)
        (read-only
         "A read-only property."
         :default-value 0)))
    
    (it "defines a type"
      (let ((type (Struct:Type:get 'TestStruct :ensure)))
        (expect type :to-be-truthy)
        (expect (Struct:get type :documentation)
                :to-equal
                "A test struct.")
        (expect (Struct:get type :name)
                :to-equal
                'TestStruct)
        (expect (length (Struct:get type :properties)) :to-equal 3)
        (expect (Struct:Type? 'TestStruct) :to-equal t)
        (expect (Struct:Type? (Struct:Type:get 'TestStruct :ensure)) :to-equal t)

        (let ((optional (nth 0 (Struct:get type :properties))))
          (expect (Struct:get optional :name)
                  :to-equal 'optional)
          (expect (Struct:get optional :default-value)
                  :to-equal 0)
          (expect (Struct:get optional :documentation)
                  :to-equal "An optional property.")
          (expect (Struct:get optional :required)
                  :to-equal nil)
          (expect (Struct:get optional :mutable)
                  :to-equal t))

        (let ((required (nth 1 (Struct:get type :properties))))
          (expect (Struct:get required :name)
                  :to-equal 'required)
          (expect (Struct:get required :default-value)
                  :to-equal nil)
          (expect (Struct:get required :documentation)
                  :to-equal "A required property.")
          (expect (Struct:get required :required)
                  :to-equal t)
          (expect (Struct:get required :mutable)
                  :to-equal t))

        (let ((read-only (nth 2 (Struct:get type :properties))))
          (expect (Struct:get read-only :name)
                  :to-equal 'read-only)
          (expect (Struct:get read-only :default-value)
                  :to-equal 0)
          (expect (Struct:get read-only :documentation)
                  :to-equal "A read-only property.")
          (expect (Struct:get read-only :required)
                  :to-equal nil)
          (expect (Struct:get read-only :mutable)
                  :to-equal nil))))

    (it "defines constructors and predicates"
      (expect (fboundp 'TestStruct) :to-equal t)
      (expect (fboundp 'TestStruct*) :to-equal t)
      (expect (fboundp 'TestStruct?) :to-equal t))

    (it "can be constructed"
      (expect (TestStruct :optional 1 :required "string" :read-only 2)
              :to-equal
              `(TestStruct :optional 1 :required "string" :read-only 2)))

    (it "can type-check a value"
      (expect (TestStruct? (TestStruct :required 0))
              :to-equal t)
      (expect (TestStruct? '(NonTestStruct :required 4))
              :to-equal nil)
      (expect (TestStruct? 0)
              :to-equal nil))

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
        (expect (TestStruct* optional required)
                :to-equal
                `(TestStruct :optional ,optional
                             :required ,required
                             :read-only 0))))

    (it "can use spread syntax"
      (let ((other (TestStruct :required 1)))
        (expect (TestStruct* ,@other :optional 2)
                :to-equal
                `(TestStruct :optional 2 :required 1 :read-only 0))))

    (it "can get a property"
      (expect (Struct:get (TestStruct :required 1)
                          :required)
              :to-equal 1))

    (it "can get a property with default"
      (let ((struct (TestStruct :required 1)))
        ;; Need to set it explicitly, since otherwise default-value applies.
        (Struct:set struct :optional nil)
        (expect (Struct:get struct :optional 2)
                :to-equal 2)))

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

    (it "can update a property"
      (let ((struct (TestStruct :required 1)))
        (expect (Struct:update struct :required #'1+)
                :to-equal 2)
        (expect (Struct:get struct :required)
                :to-equal 2)))

    (it "can update a property anaphorically"
      (let ((struct (TestStruct :required 1)))
        (expect (Struct:update- struct :required (1+ it))
                :to-equal 2)
        (expect (Struct:get struct :required)
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
      (expect (Struct:Type:get 'TestStruct :ensure) :to-throw 'error)
      (expect (fboundp 'TestStruct) :to-equal nil)
      (expect (fboundp 'TestStruct*) :to-equal nil))

    (it "can not undefine core types"
      (expect (Struct:undefine 'Struct:Type) :to-throw)
      (expect (Struct:undefine 'Struct:Property) :to-throw))

    (it "can be used with cl's type-system"
      (expect (cl-check-type (TestStruct :required 1) TestStruct)
              :to-be nil)
      (expect (cl-check-type "TestStruct" TestStruct)
              :to-throw 'wrong-type-argument)
      (expect (cl-typep (TestStruct :required 1) 'TestStruct)
              :to-be t)
      (expect (cl-typep "TestStruct" 'TestStruct)
              :to-be nil))

    (it "accepts nil for non-required typed properties"
      (Struct:define TestStruct (property :type number))

      (expect (TestStruct) :to-equal (TestStruct :property nil))
      (expect (TestStruct :property 0) :to-equal (TestStruct :property 0))
      (expect (TestStruct :property "0")
              :to-throw 'wrong-type-argument '(number "0"))))

  (describe "with a read-only struct"
    (before-each
      (Struct:define TestStruct
        "A test struct."
        :mutable nil
        (property
         "An optional property."
         :default-value 0)))

    (after-each
      (Struct:undefine 'TestStruct))

    (it "can not set any property"
      (expect (Struct:set (TestStruct) :property 1) :to-throw)))

  (describe "with an integer typed property"
    (before-each
      (Struct:define TestStruct
        "A test struct."
        (property
         "An integer property."
         :default-value 0
         :mutable t
         :type integer)))

    (after-each
      (Struct:undefine 'TestStruct))

    (it "can be constructed with an integer value"
      (expect (TestStruct :property 1)
              :to-equal
              '(TestStruct :property 1)))

    (it "can not be constructed with a non-integer value"
      (expect (TestStruct :property "one") :to-throw))
    
    (it "can set an integer value"
      (expect (Struct:set (TestStruct) :property 1)
              :to-equal 1))
    
    (it "can not set a non-integer value"
      (expect (Struct:set (TestStruct) :property "one") :to-throw)))

  (it "can define empty structs"
    (expect (Struct:define TestStruct)
            :to-be 'TestStruct)
    (expect (Struct:define TestStruct "Documentation.")
            :to-be 'TestStruct))

  (it "can define properties with keywords alone"
    (expect (Struct:define TestStruct
              (:name property :documentation "Property documentation."))
            :to-be 'TestStruct)
    (let ((property (nth 0 (Struct:get (Struct:Type:get 'TestStruct :ensure)
                                       :properties))))
      (expect (Struct:get property :name)
              :to-be 'property)
      (expect (Struct:get property :default-value)
              :to-be nil)
      (expect (Struct:get property :documentation)
              :to-equal "Property documentation.")
      (expect (Struct:get property :required)
              :to-be nil)
      (expect (Struct:get property :mutable)
              :to-be nil)))

  (describe "Struct:defun"
    (before-each
      (Struct:define TestStruct (property :default-value 0)))

    (describe "used at compile-time"
      (it "throws errors when appropriate"
        (expect (eval '(Struct:defun fn (&rest rest &struct struct)))
                :to-throw
                'error '("&rest and &struct are mutually exclusive: (&rest rest &struct struct)"))
        (expect (eval '(Struct:defun fn (&struct)))
                :to-throw
                'error '("&struct is missing an argument: (&struct)"))
        (expect (eval '(Struct:defun fn (&struct struct argument)))
                :to-throw
                'error '("&struct argument must be last: (&struct struct argument)"))
        (expect (eval '(Struct:defun fn (&struct struct)))
                :to-throw
                'error '("&struct argument should specify a struct-type: (&struct struct)"))
        (expect (eval '(Struct:defun fn (&rest)))
                :to-throw
                'error '("&rest is missing an argument: (&rest)"))
        (expect (eval '(Struct:defun fn (&rest rest argument)))
                :to-throw
                'error '("&rest argument must be last: (&rest rest argument)"))
        (expect (eval '(Struct:defun fn (&rest (integer rest))))
                :to-throw
                'error '("Providing a type for &rest arguments is not supported: (&rest (integer rest))"))
        (expect (eval '(Struct:defun fn (&rest [])))
                :to-throw
                'error '("Argument should be a symbol or have the form (argument type): (&rest [])"))
        (expect (eval '(Struct:defun fn ((a b c))))
                :to-throw
                'error '("Argument should be a symbol or have the form (argument type): ((a b c))"))))
    
    (describe "used at runtime"
      (after-each (fmakunbound 'TestFn))
      (it "can be defined"
        (Struct:defun TestFn ((self TestStruct) argument)
          (+ (Struct:get self :property))))

      (it "can be used without types"
        (Struct:defun TestFn (a b)
          (+ a b))
        (expect (TestFn 2 3) :to-be 5))

      (it "can be used with types"
        (Struct:defun TestFn ((a number) (b number))
          (+ a b))
        (expect (TestFn 2 3) :to-be 5)
        (expect (TestFn "2" "3")
                :to-throw 'wrong-type-argument '(number "2" a)))

      (it "can be used with &optional arguments"
        (Struct:defun TestFn ((a number) &optional (b number))
          (+ a (or b 10)))
        (expect (TestFn 2 3) :to-be 5)
        (expect (TestFn 2) :to-be 12)
        (expect (TestFn "2")
                :to-throw 'wrong-type-argument '(number "2" a))
        (expect (TestFn 2 "3")
                :to-throw 'wrong-type-argument '((or null number) "3" b)))

      (it "can be used with a &rest argument"
        (Struct:defun TestFn ((a number) &rest rest)
          (apply #'+ a rest))
        (expect (TestFn 2 3 4) :to-be 9)
        (expect (TestFn 2 3) :to-be 5)
        (expect (TestFn 2) :to-be 2)
        (expect (TestFn "2")
                :to-throw 'wrong-type-argument '(number "2" a))
        (expect (TestFn 2 "3")
                :to-throw 'wrong-type-argument '(number-or-marker-p "3")))

      (it "can be used with a &struct argument"
        (Struct:defun TestFn ((a number) &struct (struct TestStruct))
          (+ a (Struct:get struct :property)))
        
        (expect (TestFn 2) :to-be 2)
        (expect (TestFn 2 :property 3) :to-be 5)
        (expect (TestFn 2 (TestStruct :property 4)) :to-be 6)
        (expect (TestFn "2")
                :to-throw 'wrong-type-argument '(number "2" a))
        (expect (TestFn 2 3)
                :to-throw)))))
