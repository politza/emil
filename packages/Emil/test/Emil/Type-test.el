;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'Emil/Type)

(describe "Emil:Type"
  (describe "Emil:Type:read"
    (it "Null"
      (expect (Emil:Type:read 'Null)
              :to-equal (Emil:Type:Null)))

    (it "Any"
      (expect (Emil:Type:read 'Any)
              :to-equal (Emil:Type:Any)))

    (it "Never"
      (expect (Emil:Type:read 'Never)
              :to-equal (Emil:Type:Never)))

    (it "Void"
      (expect (Emil:Type:read 'Void)
              :to-equal (Emil:Type:Void)))

    (it "builtin"
      (expect (Emil:Type:read 'string)
              :to-equal (Emil:Type:Basic :name 'string)))

    (describe "function"
      (it "basic"
        (expect (Emil:Type:read '(-> (string) number))
                :to-equal
                '(Emil:Type:Arrow
                  :arguments ((Emil:Type:Basic :name string))
                  :rest? nil
                  :returns (Emil:Type:Basic :name number)
                  :min-arity 1)))

      (it "identity"
        (expect (Emil:Type:read '(-> ('a) 'a))
                :to-equal
                '(Emil:Type:Forall
                  :parameters ((Emil:Type:Variable :name a))
                  :type
                  (Emil:Type:Arrow
                   :arguments ((Emil:Type:Variable :name a))
                   :rest? nil
                   :returns (Emil:Type:Variable :name a)
                   :min-arity 1))))

      (it "with &optional argument"
        (expect (Emil:Type:read '(-> (string Any &optional Never) Void))
                :to-equal
                '(Emil:Type:Arrow
                  :arguments ((Emil:Type:Basic :name string)
                                   (Emil:Type:Any)
                                   (Emil:Type:Never))
                  :rest? nil
                  :returns (Emil:Type:Void)
                  :min-arity 2)))

      (it "with &rest argument"
        (expect (Emil:Type:read '(-> (string Any &rest Never) Void))
                :to-equal
                '(Emil:Type:Arrow
                  :arguments ((Emil:Type:Basic :name string)
                              (Emil:Type:Any)
                              (Emil:Type:Never))
                  :rest? t 
                  :returns (Emil:Type:Void)
                  :min-arity 2)))))

  (describe "Emil:Type:monomorph?"
    (it "Null"
      (expect (Emil:Type:monomorph? (Emil:Type:Null))
              :to-equal t))

    (it "Any"
      (expect (Emil:Type:monomorph? (Emil:Type:Any))
              :to-equal t))

    (it "Never"
      (expect (Emil:Type:monomorph? (Emil:Type:Never))
              :to-equal t))

    (it "Void"
      (expect (Emil:Type:monomorph? (Emil:Type:Void))
              :to-equal t))

    (it "basic"
      (expect (Emil:Type:monomorph? (Emil:Type:Basic :name 'string))
              :to-equal t))

    (it "instance"
      (expect (Emil:Type:monomorph? (Emil:Type:Existential :name 'a))
              :to-equal t))

    (it "monomorph function"
      (expect (Emil:Type:monomorph? (Emil:Type:read '(-> (string) Void)))
              :to-equal t)
      (expect (Emil:Type:monomorph?
               (Emil:Type:read '(-> (&optional string &rest string) Void)))
              :to-equal t)
      (expect (Emil:Type:monomorph? (Emil:Type:read '(-> () Void)))
              :to-equal t))

    (it "polymorph function"
      (expect (Emil:Type:monomorph? (Emil:Type:read '(-> ('a) Void)))
              :to-equal nil)
      (expect (Emil:Type:monomorph?
               (Emil:Type:read '(-> (&optional string &rest 'a) Void)))
              :to-equal nil)
      (expect (Emil:Type:monomorph? (Emil:Type:read '(-> () 'a)))
              :to-equal nil))

    (it "polymorph pseudo-types"
      ;; buttercup seems to (wrongly) interpret some keywords of the
      ;; Emil:Type:Forall constructor, thus they are all quoted.
      (expect (Emil:Type:monomorph? '(Emil:Type:Forall
                                      :parameters (list (Emil:Type:Variable :name a))
                                      :type (Emil:Type:Never)))
              :to-equal nil)
      (expect (Emil:Type:monomorph? '(Emil:Type:Forall
                                      :parameters nil
                                      :type (Emil:Type:Never)))
              :to-equal t)))

  (describe "Emil:Type:free-variables"
    (it "Null"
      (expect (Emil:Type:free-variables (Emil:Type:Null))
              :to-equal nil))

    (it "Any"
      (expect (Emil:Type:free-variables (Emil:Type:Any))
              :to-equal nil))

    (it "Never"
      (expect (Emil:Type:free-variables (Emil:Type:Never))
              :to-equal nil))

    (it "Void"
      (expect (Emil:Type:free-variables (Emil:Type:Void))
              :to-equal nil))

    (it "basic"
      (expect (Emil:Type:free-variables (Emil:Type:Basic :name 'string))
              :to-equal nil))

    (it "instance"
      (expect (Emil:Type:free-variables (Emil:Type:Existential :name 'a))
              :to-equal '(a)))

    (it "function basic"
      (expect (Emil:Type:free-variables (Emil:Type:read '(-> () Void)))
              :to-equal nil))

    (it "function instantiated"
      (expect (Emil:Type:free-variables
               (Emil:Type:Arrow
                :arguments (list (Emil:Type:Existential :name 'a)
                                 (Emil:Type:Existential :name 'b))
                :returns (Emil:Type:Existential :name 'c)
                :rest? t
                :min-arity 1))
              :to-equal '(a b c)))

    (it "function non-instantiated"
      (expect (Emil:Type:free-variables
               (Emil:Type:Arrow
                :arguments (list (Emil:Type:Variable :name 'a)
                                 (Emil:Type:Variable :name 'b))
                :returns (Emil:Type:Variable :name 'c)
                :rest? t
                :min-arity 1))
              :to-equal nil))

    (it "forAll"
      (expect (Emil:Type:free-variables
               '(Emil:Type:Forall
                 :parameters (Emil:Type:Variable :name d)
                 :type (Emil:Type:Arrow
                        :arguments (list (Emil:Type:Existential :name a)
                                         (Emil:Type:Existential :name b))
                        :returns (Emil:Type:Existential :name c)
                        :rest? t
                        :min-arity 1)))
              :to-equal '(a b c))))

  (describe "print"
    (it "Null"
      (expect (Emil:Type:print (Emil:Type:Null))
              :to-equal 'Null))

    (it "Any"
      (expect (Emil:Type:print (Emil:Type:Any))
              :to-equal 'Any))

    (it "Never"
      (expect (Emil:Type:print (Emil:Type:Never))
              :to-equal 'Never))

    (it "Void"
      (expect (Emil:Type:print (Emil:Type:Void))
              :to-equal 'Void))

    (it "basic"
      (expect (Emil:Type:print (Emil:Type:Basic :name 'string))
              :to-equal 'string))

    (it "variable"
      (expect (Emil:Type:print (Emil:Type:Variable :name 'a))
              :to-equal ''a))
    
    (it "instance"
      (expect (Emil:Type:print (Emil:Type:Existential :name 'a))
              :to-equal '''a))

    (it "monomorph function"
      (expect (Emil:Type:print (Emil:Type:read '(-> () Void)))
              :to-equal '(-> () Void)))

    (it "polymorph function"
      (expect (Emil:Type:print (Emil:Type:read '(-> ('a) 'b)))
              :to-equal '(-> ('a) 'b)))

    (it "polymorph function instantiated"
      (expect (Emil:Type:print
               (Emil:Type:Arrow
                :arguments (list (Emil:Type:Existential :name 'a)
                                 (Emil:Type:Existential :name 'b))
                :returns (Emil:Type:Existential :name 'c)
                :rest? t
                :min-arity 1))
              :to-equal '(-> (''a &rest ''b) ''c)))

    (it "forAll"
      (expect (Emil:Type:print '(Emil:Type:Forall
                                 :parameters (Emil:Type:Variable :name a)
                                 :type (Emil:Type:Never)))
              :to-equal 'Never))))
