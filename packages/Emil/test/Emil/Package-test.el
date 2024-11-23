;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'Emil/Package)

(describe "Emil/Package"
  (after-each (setq Emil:Package:declarations nil))

  (describe "Emil:Package:Declaration:version="
    (it "same versions"
      (expect (Emil:Package:Declaration:version=
               (Emil:Package:Declaration :name 'a :min-version "1.0" :max-version "2.0")
               (Emil:Package:Declaration :name 'b :min-version "1.0" :max-version "2.0"))
              :to-equal t))

    (it "null min versions"
      (expect (Emil:Package:Declaration:version=
               (Emil:Package:Declaration :name 'a :min-version nil :max-version "2.0")
               (Emil:Package:Declaration :name 'b :min-version nil :max-version "2.0"))
              :to-equal t))

    (it "null max versions"
      (expect (Emil:Package:Declaration:version=
               (Emil:Package:Declaration :name 'a :min-version "1.0" :max-version nil)
               (Emil:Package:Declaration :name 'b :min-version "1.0" :max-version nil))
              :to-equal t))

    (it "different min versions"
      (expect (Emil:Package:Declaration:version=
               (Emil:Package:Declaration :name 'a :min-version "1.0" :max-version "2.0")
               (Emil:Package:Declaration :name 'a :min-version "2.0" :max-version "2.0"))
              :to-equal nil))

    (it "different max versions"
      (expect (Emil:Package:Declaration:version=
               (Emil:Package:Declaration :name 'a :min-version "1.0" :max-version "2.0")
               (Emil:Package:Declaration :name 'a :min-version "1.0" :max-version "3.0"))
              :to-equal nil)))

  (describe "Emil:Package:Declaration:specialises?"
    (it "equal packages"
      (expect (Emil:Package:Declaration:specialises?
               (Emil:Package:Declaration :name 'a :min-version "1.0" :max-version "2.0")
               (Emil:Package:Declaration :name 'a :min-version "1.0" :max-version "2.0"))
              :to-equal nil))

    (it "equal packages with null max"
      (expect (Emil:Package:Declaration:specialises?
               (Emil:Package:Declaration :name 'a :min-version "1.0")
               (Emil:Package:Declaration :name 'a :min-version "1.0"))
              :to-equal nil))

    (it "null self-min, non-null other-min"
      (expect (Emil:Package:Declaration:specialises?
               (Emil:Package:Declaration :name 'a :min-version nil :max-version "1.0")
               (Emil:Package:Declaration :name 'b :min-version "1.0" :max-version "2.0"))
              :to-equal nil))

    (it "non-null self-min, null other-min"
      (expect (Emil:Package:Declaration:specialises?
               (Emil:Package:Declaration :name 'a :min-version "1.0" :max-version "1.0")
               (Emil:Package:Declaration :name 'b :min-version nil :max-version "2.0"))
              :to-equal t))

    (it "self-min < other-min"
      (expect (Emil:Package:Declaration:specialises?
               (Emil:Package:Declaration :name 'a :min-version "1.0" :max-version "1.0")
               (Emil:Package:Declaration :name 'b :min-version "1.1" :max-version "2.0"))
              :to-equal nil))

    (it "self-min > other-min"
      (expect (Emil:Package:Declaration:specialises?
               (Emil:Package:Declaration :name 'a :min-version "1.1" :max-version "1.0")
               (Emil:Package:Declaration :name 'b :min-version "1.0" :max-version "2.0"))
              :to-equal t))

    (it "self-min = other-min, self-max < other-max"
      (expect (Emil:Package:Declaration:specialises?
               (Emil:Package:Declaration :name 'a :min-version "1.0" :max-version "1.0")
               (Emil:Package:Declaration :name 'b :min-version "1.0" :max-version "2.0"))
              :to-equal t))

    (it "self-min = other-min, self-max > other-max"
      (expect (Emil:Package:Declaration:specialises?
               (Emil:Package:Declaration :name 'a :min-version "1.0" :max-version "2.0")
               (Emil:Package:Declaration :name 'b :min-version "1.0" :max-version "1.0"))
              :to-equal nil)))

  (describe "Emil:Package:add-declaration"
    (it "new package"
      (Emil:Package:add-declaration
       (Emil:Package:Declaration :name 'test))
      (expect Emil:Package:declarations
              :to-equal '((test (Emil:Package:Declaration
                                 :name test
                                 :min-version nil
                                 :max-version nil
                                 :functions nil
                                 :variables nil)))))

    (it "more specialized declarations"
      (setq Emil:Package:declarations
            `((test ,(Emil:Package:Declaration :name 'test :min-version "1.0"))))
      (Emil:Package:add-declaration
       (Emil:Package:Declaration :name 'test :min-version "2.0"))
      (expect Emil:Package:declarations
              :to-equal '((test (Emil:Package:Declaration
                                 :name test
                                 :min-version "2.0"
                                 :max-version nil
                                 :functions nil
                                 :variables nil)
                                (Emil:Package:Declaration
                                 :name test
                                 :min-version "1.0"
                                 :max-version nil
                                 :functions nil
                                 :variables nil)))))

    (it "more generic declarations"
      (setq Emil:Package:declarations
            `((test ,(Emil:Package:Declaration :name 'test :min-version "2.0"))))
      (Emil:Package:add-declaration
       (Emil:Package:Declaration :name 'test :min-version "1.0"))
      (expect Emil:Package:declarations
              :to-equal '((test (Emil:Package:Declaration
                                 :name test
                                 :min-version "2.0"
                                 :max-version nil
                                 :functions nil
                                 :variables nil)
                                (Emil:Package:Declaration
                                 :name test
                                 :min-version "1.0"
                                 :max-version nil
                                 :functions nil
                                 :variables nil)))))

    (it "merges with an existing declaration"
      (setq Emil:Package:declarations
            `((test ,(Emil:Package:Declaration
                      :name 'test
                      :min-version "2.0"
                      :functions `((car . ,(Emil:Type:read '(-> (Any) Any))))
                      :variables `((most-negative-fixnum . ,(Emil:Type:read 'integer)))))))
      (Emil:Package:add-declaration
       (Emil:Package:Declaration :name 'test
                                 :min-version "2.0"
                                 :functions `((car . ,(Emil:Type:read '(-> ((Cons 'a 'b)) 'a))))))
      (expect Emil:Package:declarations
              :to-equal `((test (Emil:Package:Declaration
                                 :name test
                                 :min-version "2.0"
                                 :max-version nil
                                 :functions ((car . ,(Emil:Type:read '(-> ((Cons 'a 'b)) 'a))))
                                 :variables ((most-negative-fixnum . ,(Emil:Type:read 'integer)))))))))

  (describe "Emil:Package:Declaration:match-version?"
    (it "nil always matches"
      (expect (Emil:Package:Declaration:match-version?
               (Emil:Package:Declaration
                :name 'test
                :min-version "1.0"
                :max-version "2.0"
                :functions nil
                :variables nil)
               nil)
              :to-equal t))

    (it "matching version"
      (expect (Emil:Package:Declaration:match-version?
               (Emil:Package:Declaration
                :name 'test
                :min-version "1.0"
                :max-version "2.0"
                :functions nil
                :variables nil)
               "1.5")
              :to-equal t))

    (it "version to low"
      (expect (Emil:Package:Declaration:match-version?
               (Emil:Package:Declaration
                :name 'test
                :min-version "2.0"
                :max-version "3.0"
                :functions nil
                :variables nil)
               "1.0")
              :to-equal nil))

    (it "version to high"
      (expect (Emil:Package:Declaration:match-version?
               (Emil:Package:Declaration
                :name 'test
                :min-version "1.0"
                :max-version "2.0"
                :functions nil
                :variables nil)
               "3.0")
              :to-equal nil)))

  (describe "Emil:Package:declare-functions"
    (it "basic"
      (eval '(Emil:Package:declare-functions emacs
               :min-version "1.0"
               :max-version "2.0"
               (car . (-> ((Cons 'a 'b)) 'a))
               (cdr . (-> ((Cons 'a 'b)) 'b))))
      (expect Emil:Package:declarations
              :to-equal `((emacs (Emil:Package:Declaration
                                  :name emacs
                                  :min-version "1.0"
                                  :max-version "2.0"
                                  :functions ((car . ,(Emil:Type:read '(-> ((Cons 'a 'b)) 'a)))
                                              (cdr . ,(Emil:Type:read '(-> ((Cons 'a 'b)) 'b))))
                                  :variables nil))))))

  (describe "Emil:Package:declare-variables"
    (it "basic"
      (eval '(Emil:Package:declare-variables emacs
               :min-version "1.0"
               :max-version "2.0"
               (most-negative-fixnum . integer)
               (most-positive-fixnum . integer)))
      (expect Emil:Package:declarations
              :to-equal `((emacs (Emil:Package:Declaration
                                  :name emacs
                                  :min-version "1.0"
                                  :max-version "2.0"
                                  :functions nil
                                  :variables ((most-negative-fixnum . ,(Emil:Type:read 'integer))
                                              (most-positive-fixnum . ,(Emil:Type:read 'integer)))))))))

  (describe "Emil:Package:Env"
    (it "Emil:Env:lookup-variable"
      (setq Emil:Package:declarations
            `((test (Emil:Package:Declaration
                     :name test
                     :min-version "2.0"
                     :max-version nil
                     :functions nil
                     :variables ((most-positive-fixnum . ,(Emil:Type:read 'integer))))
                    (Emil:Package:Declaration
                     :name test
                     :min-version "1.0"
                     :max-version nil
                     :functions nil
                     :variables ((most-negative-fixnum . ,(Emil:Type:read 'integer)))))))
      (expect (Emil:Type:print
               (Emil:Env:lookup-variable
                (Emil:Package:Env:for 'test "3.0")
                'most-negative-fixnum))
              :to-equal 'integer)
      (expect (Emil:Type:print
               (Emil:Env:lookup-variable
                (Emil:Package:Env:for 'test "3.0")
                'most-positive-fixnum ))
              :to-equal 'integer))

    (it "Emil:Env:lookup-function"
      (setq Emil:Package:declarations
            `((test (Emil:Package:Declaration
                     :name test
                     :min-version "2.0"
                     :max-version nil
                     :functions ((cdr . ,(Emil:Type:read '(-> ((Cons 'a 'b)) 'b))))
                     :variables nil)
                    (Emil:Package:Declaration
                     :name test
                     :min-version "1.0"
                     :max-version nil
                     :functions ((car . ,(Emil:Type:read '(-> ((Cons 'a 'b)) 'a))))
                     :variables nil))))
      (expect (Emil:Type:print
               (Emil:Env:lookup-function
                (Emil:Package:Env:for 'test "3.0")
                'car))
              :to-equal '(-> ((Cons 'a 'b)) 'a))
      (expect (Emil:Type:print
               (Emil:Env:lookup-function
                (Emil:Package:Env:for 'test "3.0")
                'cdr))
              :to-equal '(-> ((Cons 'a 'b)) 'b)))))
