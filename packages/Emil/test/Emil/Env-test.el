;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'dash)
(require 'Emil/Env)
(require 'Emil/Type)

(describe "Emil:Env"
  (describe "Emil:Env:Alist"
    (it "basic variable"
      (expect (Emil:Env:lookup-variable
               (Emil:Env:Alist:read '((a . integer)) nil)
               'a nil)
              :to-equal (Emil:Type:Basic :name 'integer)))

    (it "basic function"
      (expect (Emil:Env:lookup-function
               (Emil:Env:Alist:read nil '((a . (-> (string) integer))))
               'a nil)
              :to-equal (Emil:Type:Arrow
                         :arguments (list (Emil:Type:Basic :name 'string))
                         :rest? nil
                         :returns (Emil:Type:Basic :name 'integer)
                         :min-arity 1)))

    (it "polymorph function"
      (expect (Emil:Env:lookup-function
               (Emil:Env:Alist:read nil '((a . (-> ('a) 'b))))
               'a nil)
              :to-equal
              '(Emil:Type:Forall
                :parameters ((Emil:Type:Variable :name a)
                             (Emil:Type:Variable :name b))
                :type (Emil:Type:Arrow
                       :arguments ((Emil:Type:Variable :name a))
                       :rest? nil
                       :returns (Emil:Type:Variable :name b)
                       :min-arity 1))))

    (it "variable and function"
      (expect (Emil:Type:print
               (Emil:Env:lookup-variable
                (Emil:Env:Alist:read '((a . integer)) '((a . (-> () integer))))
                'a nil))
              :to-equal 'integer)
      (expect (Emil:Type:print
               (Emil:Env:lookup-function
                (Emil:Env:Alist:read '((a . integer)) '((a . (-> () integer))))
                'a nil))
              :to-equal '(-> () integer)))

    (it "rejects malformed alists"
      (expect (Emil:Env:Alist:read 0 1)
              :to-throw)
      (expect (Emil:Env:Alist:read '((:keyword . string)) nil)
              :to-throw)
      (expect (Emil:Env:Alist:read nil '((nil . string)))
              :to-throw))

    (it "rejects non-function types as functions"
      (expect (Emil:Env:Alist:read nil '((a . integer)))
              :to-throw)))

  (describe "Emil:Env:Hierarchy"
    (it "empty"
      (expect (Emil:Env:lookup-variable
               (Emil:Env:Hierarchy
                :environments nil)
               'a nil)
              :to-equal nil)
      (expect (Emil:Env:lookup-function
               (Emil:Env:Hierarchy
                :environments nil)
               'a nil)
              :to-equal nil))

    (it "single environment"
      (expect (Emil:Type:print
               (Emil:Env:lookup-variable
                (Emil:Env:Hierarchy
                 :environments (list (Emil:Env:Alist:read
                                      '((a . integer)) '((a . (-> () integer))))))
                'a nil))
              :to-equal 'integer)
      (expect (Emil:Type:print
               (Emil:Env:lookup-function
                (Emil:Env:Hierarchy
                 :environments (list (Emil:Env:Alist:read
                                      '((a . integer)) '((a . (-> () integer))))))
                'a nil))
              :to-equal '(-> () integer)))

    (it "shadowing environment"
      (expect (Emil:Type:print
               (Emil:Env:lookup-variable
                (Emil:Env:Hierarchy
                 :environments (list (Emil:Env:Alist:read
                                      '((a . integer)) '((a . (-> () integer))))
                                     (Emil:Env:Alist:read
                                      '((a . string)) '((a . (-> () string))))))

                'a nil))
              :to-equal 'integer)
      (expect (Emil:Type:print
               (Emil:Env:lookup-function
                (Emil:Env:Hierarchy
                 :environments (list (Emil:Env:Alist:read
                                      '((a . integer)) '((a . (-> () integer))))
                                     (Emil:Env:Alist:read
                                      '((a . string)) '((a . (-> () string))))))

                'a nil))
              :to-equal '(-> () integer)))

    (it "falling back environment"
      (expect (Emil:Type:print
               (Emil:Env:lookup-variable
                (Emil:Env:Hierarchy
                 :environments (list (Emil:Env:Alist:read
                                      '((b . integer)) '((b . (-> () integer))))
                                     (Emil:Env:Alist:read
                                      '((a . string)) '((a . (-> () string))))))

                'a nil))
              :to-equal 'string)
      (expect (Emil:Type:print
               (Emil:Env:lookup-function
                (Emil:Env:Hierarchy
                 :environments (list (Emil:Env:Alist:read
                                      '((b . integer)) '((b . (-> () integer))))
                                     (Emil:Env:Alist:read
                                      '((a . string)) '((a . (-> () string))))))

                'a nil))
              :to-equal '(-> () string)))))