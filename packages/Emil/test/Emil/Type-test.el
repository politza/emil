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

    (it "names should start with a letter"
      (expect (Emil:Type:read '0-is-the-new-1)
              :to-throw 'Emil:invalid-type-form))

    (it "names should not end with a question-mark"
      (expect (Emil:Type:read 'ready?)
              :to-throw 'Emil:invalid-type-form))

    (it "quote may not be used as a name"
      (expect (Emil:Type:read 'quote)
              :to-throw 'Emil:invalid-type-form))

    (it "constant symbols may not be used as a name"
      (expect (Emil:Type:read t)
              :to-throw 'Emil:invalid-type-form)
      (expect (Emil:Type:read nil)
              :to-throw 'Emil:invalid-type-form)
      (expect (Emil:Type:read :keyword)
              :to-throw 'Emil:invalid-type-form))

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
                  :min-arity 2)))

      (it "nested polymorph"
        (expect (Emil:Type:read '(-> ((-> ('a) 'b) 'a) 'b))
                :to-equal
                '(Emil:Type:Forall
                  :parameters ((Emil:Type:Variable :name a)
                               (Emil:Type:Variable :name b))
                  :type (Emil:Type:Arrow
                         :arguments ((Emil:Type:Arrow
                                      :arguments ((Emil:Type:Variable :name a))
                                      :rest? nil
                                      :returns (Emil:Type:Variable :name b)
                                      :min-arity 1)
                                     (Emil:Type:Variable :name a))
                         :rest? nil
                         :returns (Emil:Type:Variable :name b)
                         :min-arity 2))))

      (it "invalid polymorph function"
        (expect (Emil:Type:read '(-> 'a 'b))
                :to-throw 'Emil:invalid-type-form)))

    (describe "compound types"
      (it "zero parameter"
        (expect (Emil:Type:read '(Nothing))
                :to-equal
                '(Emil:Type:Compound
                  :name Nothing
                  :arguments nil)))

      (it "multiple arguments"
        (expect (Emil:Type:read '(Map string integer))
                :to-equal
                '(Emil:Type:Compound
                  :name Map
                  :arguments ((Emil:Type:Basic :name string)
                               (Emil:Type:Basic :name integer)))))

      (it "monomorph"
        (expect (Emil:Type:read '(List integer))
                :to-equal
                '(Emil:Type:Compound
                  :name List
                  :arguments ((Emil:Type:Basic :name integer)))))

      (it "polymorph"
        (expect (Emil:Type:read '(List 'a))
                :to-equal
                '(Emil:Type:Forall
                  :parameters ((Emil:Type:Variable :name a))
                  :type (Emil:Type:Compound
                         :name List
                         :arguments ((Emil:Type:Variable :name a))))))

      (it "constructor name may not end with a ?"
        (expect (Emil:Type:read '(list? 'a))
                :to-throw 'Emil:invalid-type-form))

      (it "validation of builtins"
        (expect (Emil:Type:read '(List a b))
                :to-throw 'Emil:invalid-type-form)
        (expect (Emil:Type:read '(Cons a))
                :to-throw 'Emil:invalid-type-form))))

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
              :to-equal t))

    (it "monomorph compound"
      (expect (Emil:Type:monomorph? (Emil:Type:read '(Map string integer)))
              :to-equal t))

    (it "polymorph compound"
      (expect (Emil:Type:monomorph? (Emil:Type:read '(Map 'a 'b)))
              :to-equal nil)))

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
              :to-equal '(a b c)))

    (it "compound"
      (expect (Emil:Type:free-variables
               (Emil:Type:Compound
                :name 'List :arguments
                (list (Emil:Type:Variable :name 'a))))
              :to-equal nil))

    (it "compound instantiated"
      (expect (Emil:Type:free-variables
               (Emil:Type:Compound
                :name 'List :arguments
                (list (Emil:Type:Existential :name 'a))))
              :to-equal '(a))))

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
              :to-equal ''a?))

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
              :to-equal '(-> ('a? &rest 'b?) 'c?)))

    (it "forAll"
      (expect (Emil:Type:print '(Emil:Type:Forall
                                 :parameters (Emil:Type:Variable :name a)
                                 :type (Emil:Type:Never)))
              :to-equal 'Never))

    (it "compound"
      (expect (Emil:Type:print '(Emil:Type:Compound
                                 :name List :arguments
                                 ((Emil:Type:Variable :name a))))
              :to-equal '(List 'a))))

  (describe "Emil:Type:normalize"
    (it "basic"
      (expect (Emil:Type:print
               (Emil:Type:normalize (Emil:Type:read 'integer)))
              :to-equal 'integer))

    (it "monomorph function"
      (expect (Emil:Type:print
               (Emil:Type:normalize (Emil:Type:read '(-> (integer) string))))
              :to-equal '(-> (integer) string)))

    (it "polymorph function"
      (expect (Emil:Type:print
               (Emil:Type:normalize (Emil:Type:read '(-> ('x 'y) 'y))))
              :to-equal '(-> ('a 'b) 'b)))

    (it "mixed function"
      (expect (Emil:Type:print
               (Emil:Type:normalize (Emil:Type:read
                                     '(-> (string 'y) 'y))))
              :to-equal '(-> (string 'a) 'a)))

    (it "nested function"
      (expect (Emil:Type:print
               (Emil:Type:normalize
                (Emil:Type:read '(-> ((-> ('x) 'y) 'x 'y)
                                     (-> ('x 'y) 'z)))))
              :to-equal '(-> ((-> ('a) 'b) 'a 'b)
                             (-> ('a 'b) 'c)))))

  (describe "Emil:Type:Arrow"
    (describe "Emil:Type:Arrow:arity"
      (it "no arguments"
        (expect (Emil:Type:Arrow:arity
                 (Emil:Type:-read '(-> () Any)))
                :to-equal (cons 0 0)))

      (it "fixed arguments"
        (expect (Emil:Type:Arrow:arity
                 (Emil:Type:-read '(-> (Any Any) Any)))
                :to-equal (cons 2 2)))

      (it "optional arguments"
        (expect (Emil:Type:Arrow:arity
                 (Emil:Type:-read '(-> (Any &optional Any Any) Any)))
                :to-equal (cons 1 3)))

      (it "rest arguments"
        (expect (Emil:Type:Arrow:arity
                 (Emil:Type:-read '(-> (Any &rest Any) Any)))
                :to-equal (cons 1 'many)))

      (it "rest arguments (numeric)"
        (expect (Emil:Type:Arrow:arity
                 (Emil:Type:-read '(-> (Any &rest Any) Any))
                 t)
                :to-equal (cons 1 most-positive-fixnum)))

      (it "mixed arguments"
        (expect (Emil:Type:Arrow:arity
                 (Emil:Type:-read '(-> (Any &optional Any &rest Any) Any)))
                :to-equal (cons 1 'many))))

    (describe "Emil:Type:Arrow:arity-assignable-to?"
      (it "fixed arguments"
        (expect (Emil:Type:Arrow:arity-assignable-to?
                 (Emil:Type:-read '(-> (Any Any) Any))
                 (Emil:Type:-read '(-> (Any Any) Any)))
                :to-equal t)
        (expect (Emil:Type:Arrow:arity-assignable-to?
                 (Emil:Type:-read '(-> (Any Any) Any))
                 (Emil:Type:-read '(-> (Any Any Any) Any)))
                :to-equal nil))

      (it "reflexiveness"
        (expect (Emil:Type:Arrow:arity-assignable-to?
                 (Emil:Type:-read '(-> (Any &optional Any &rest Any) Any))
                 (Emil:Type:-read '(-> (Any &optional Any &rest Any) Any)))
                :to-equal t))

      (it "rest and no rest"
        (expect (Emil:Type:Arrow:arity-assignable-to?
                 (Emil:Type:-read '(-> (Any &rest Any) Any))
                 (Emil:Type:-read '(-> (Any &optional Any) Any)))
                :to-equal t)
        (expect (Emil:Type:Arrow:arity-assignable-to?
                 (Emil:Type:-read '(-> (Any &optional Any) Any))
                 (Emil:Type:-read '(-> (Any &rest Any) Any)))
                :to-equal nil)))

    (describe "Emil:Type:Arrow:arity-assignable-from?"
      (it "fixed arguments"
        (expect (Emil:Type:Arrow:arity-assignable-from?
                 (Emil:Type:-read '(-> (Any Any) Any))
                 (Emil:Type:-read '(-> (Any Any) Any)))
                :to-equal t)
        (expect (Emil:Type:Arrow:arity-assignable-from?
                 (Emil:Type:-read '(-> (Any Any) Any))
                 (Emil:Type:-read '(-> (Any Any Any) Any)))
                :to-equal nil))

      (it "reflexiveness"
        (expect (Emil:Type:Arrow:arity-assignable-from?
                 (Emil:Type:-read '(-> (Any &optional Any &rest Any) Any))
                 (Emil:Type:-read '(-> (Any &optional Any &rest Any) Any)))
                :to-equal t))

      (it "rest and no rest"
        (expect (Emil:Type:Arrow:arity-assignable-from?
                 (Emil:Type:-read '(-> (Any &optional Any) Any))
                 (Emil:Type:-read '(-> (Any &rest Any) Any)))
                :to-equal t)
        (expect (Emil:Type:Arrow:arity-assignable-from?
                 (Emil:Type:-read '(-> (Any &rest Any) Any))
                 (Emil:Type:-read '(-> (Any &optional Any) Any)))
                :to-equal nil)))

    (describe "Emil:Type:Arrow:adjusted-arguments"
      (it "fixed arguments"
        (expect (Emil:Type:Arrow:adjusted-arguments
                 (Emil:Type:-read '(-> (a b ) c))
                 2)
                :to-equal
                '((Emil:Type:Basic :name a)
                  (Emil:Type:Basic :name b))))

      (it "fixed arguments / to few"
        (expect (Emil:Type:Arrow:adjusted-arguments
                 (Emil:Type:-read '(-> (a b) c))
                 1)
                :to-throw))

      (it "fixed arguments / to many"
        (expect (Emil:Type:Arrow:adjusted-arguments
                 (Emil:Type:-read '(-> (a b) c))
                 3)
                :to-throw))

      (it "optional argument"
        (expect (Emil:Type:Arrow:adjusted-arguments
                 (Emil:Type:-read '(-> (a &optional b c) d))
                 3)
                :to-equal
                '((Emil:Type:Basic :name a)
                  (Emil:Type:Basic :name b)
                  (Emil:Type:Basic :name c))))

      (it "optional argument / to few"
        (expect (Emil:Type:Arrow:adjusted-arguments
                 (Emil:Type:-read '(-> (a &optional b c) d))
                 0)
                :to-throw))

      (it "optional argument / to many"
        (expect (Emil:Type:Arrow:adjusted-arguments
                 (Emil:Type:-read '(-> (a &optional b c) d))
                 4)
                :to-throw))

      (it "rest argument"
        (expect (Emil:Type:Arrow:adjusted-arguments
                 (Emil:Type:-read '(-> (a b &rest c) d))
                 4)
                :to-equal
                '((Emil:Type:Basic :name a)
                  (Emil:Type:Basic :name b)
                  (Emil:Type:Basic :name c)
                  (Emil:Type:Basic :name c))))

      (it "rest argument / to few"
        (expect (Emil:Type:Arrow:adjusted-arguments
                 (Emil:Type:-read '(-> (a b &rest c) d))
                 1)
                :to-throw)))

    (describe "Emil:Type:Arrow:lambda-adjusted-arguments"
      (it "fixed arguments"
        (expect (Emil:Type:Arrow:lambda-adjusted-arguments
                 '(a b)
                 2)
                :to-equal
                '(a b)))

      (it "fixed arguments / to few"
        (expect (Emil:Type:Arrow:lambda-adjusted-arguments
                 '(a b)
                 1)
                :to-throw))

      (it "fixed arguments / to many"
        (expect (Emil:Type:Arrow:lambda-adjusted-arguments
                 '(a b)
                 3)
                :to-throw))

      (it "optional argument"
        (expect (Emil:Type:Arrow:lambda-adjusted-arguments
                 '(a &optional b c)
                 3)
                :to-equal
                '(a  b c)))

      (it "optional argument / to few"
        (expect (Emil:Type:Arrow:lambda-adjusted-arguments
                 '(a &optional b c)
                 0)
                :to-throw))

      (it "optional argument / to many"
        (expect (Emil:Type:Arrow:lambda-adjusted-arguments
                 '(a &optional b c)
                 4)
                :to-throw))

      (it "rest argument"
        (expect (Emil:Type:Arrow:lambda-adjusted-arguments
                 '(a b &rest c)
                 4)
                :to-equal
                '(a b c c)))

      (it "rest argument / to few"
        (expect (Emil:Type:Arrow:lambda-adjusted-arguments
                 '(a b &rest c)
                 1)
                :to-throw))))

  (describe "Emil:Type:resolve-alias"
    (it "aliased type"
      (expect (Emil:Type:resolve-alias
               (Emil:Type:Basic :name 'vector))
              :to-equal '(Emil:Type:Compound
                          :name Vector
                          :arguments
                          ((Emil:Type:Any)))))

    (it "non aliased type"
      (expect (Emil:Type:resolve-alias
               (Emil:Type:Basic :name 'integer))
              :to-equal nil)))

  (describe "Emil:Type:basic-subtype?"
    (it "reflexivity"
      (expect (Emil:Type:basic-subtype? 'integer 'integer)
              :to-be-truthy))
    
    (it "success"
      (expect (Emil:Type:basic-subtype? 'integer 'number)
              :to-be-truthy))

    (it "failure"
      (expect (Emil:Type:basic-subtype? 'number 'integer)
              :to-be nil)))

  (describe "Emil:Type:compound-subtype?"
    (it "reflexivity"
      (expect (Emil:Type:compound-subtype? 'Vector 'Vector)
              :to-be-truthy))

    (it "success"
      (expect (Emil:Type:compound-subtype? 'Vector 'Sequence)
              :to-be-truthy))

    (it "failure"
      (expect (Emil:Type:compound-subtype? 'Sequence 'Vector)
              :to-equal nil))))
