;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'Emil/Context)

(describe "Emil:Context"
  (describe "Emil:Context:hole"
    (it "empty"
      (expect (Emil:Context:hole (Emil:Context)
                                 (Emil:Type:Existential :name 'a))
              :to-equal nil))

    (it "singleton / hit"
      (expect (Emil:Context:hole
               (Emil:Context
                :entries (list (Emil:Type:Existential :name 'a)))
               (Emil:Type:Existential :name 'a))
              :to-equal (cons (Emil:Context)
                              (Emil:Context))))

    (it "singleton / miss"
      (expect (Emil:Context:hole
               (Emil:Context
                :entries (list (Emil:Type:Existential :name 'a)))
               (Emil:Type:Existential :name 'b))
              :to-equal nil))

    (it "triple / hit"
      (expect (Emil:Context:hole
               (Emil:Context
                :entries (list (Emil:Type:Existential :name 'a)
                               (Emil:Type:Existential :name 'b)
                               (Emil:Type:Existential :name 'c)))
               (Emil:Type:Existential :name 'b))
              :to-equal (cons
                         (Emil:Context
                          :entries (list (Emil:Type:Existential :name 'a)))
                         (Emil:Context
                          :entries (list (Emil:Type:Existential :name 'c))))))

    (it "triple / miss"
      (expect (Emil:Context:hole
               (Emil:Context
                :entries (list (Emil:Type:Existential :name 'a)
                               (Emil:Type:Existential :name 'b)
                               (Emil:Type:Existential :name 'c)))
               (Emil:Type:Existential :name 'd))
              :to-equal nil)))

  (describe "Emil:Context:double-hole"
    (it "empty"
      (expect (Emil:Context:double-hole
               (Emil:Context)
               (Emil:Type:Existential :name 'a)
               (Emil:Type:Existential :name 'b))
              :to-equal nil))

    (it "singleton"
      (expect (Emil:Context:double-hole
               (Emil:Context
                :entries (list (Emil:Type:Existential :name 'a)))
               (Emil:Type:Existential :name 'a)
               (Emil:Type:Existential :name 'b))
              :to-equal nil))

    (it "triple / miss"
      (expect (Emil:Context:double-hole
               (Emil:Context
                :entries (list (Emil:Type:Existential :name 'a)
                               (Emil:Type:Existential :name 'b)
                               (Emil:Type:Existential :name 'c)))
               (Emil:Type:Existential :name 'a)
               (Emil:Type:Existential :name 'd))
              :to-equal nil))

    (it "triple / hit"
      (expect (Emil:Context:double-hole
               (Emil:Context
                :entries (list (Emil:Type:Existential :name 'a)
                               (Emil:Type:Existential :name 'b)
                               (Emil:Type:Existential :name 'c)))
               (Emil:Type:Existential :name 'a)
               (Emil:Type:Existential :name 'c))
              :to-equal (list
                         (Emil:Context)
                         (Emil:Context
                          :entries (list (Emil:Type:Existential :name 'b)))
                         (Emil:Context))))

    (it "quintuple / miss"
      (expect (Emil:Context:double-hole
               (Emil:Context
                :entries (list (Emil:Type:Existential :name 'a)
                               (Emil:Type:Existential :name 'b)
                               (Emil:Type:Existential :name 'c)
                               (Emil:Type:Existential :name 'd)
                               (Emil:Type:Existential :name 'e)))
               (Emil:Type:Existential :name 'b)
               (Emil:Type:Existential :name 'f))
              :to-equal nil))

    (it "quintuple / hit"
      (expect (Emil:Context:double-hole
               (Emil:Context
                :entries (list (Emil:Type:Existential :name 'a)
                               (Emil:Type:Existential :name 'b)
                               (Emil:Type:Existential :name 'c)
                               (Emil:Type:Existential :name 'd)
                               (Emil:Type:Existential :name 'e)))
               (Emil:Type:Existential :name 'b)
               (Emil:Type:Existential :name 'd))
              :to-equal (list
                         (Emil:Context
                          :entries (list (Emil:Type:Existential :name 'a)))
                         (Emil:Context
                          :entries (list (Emil:Type:Existential :name 'c)))
                         (Emil:Context
                          :entries (list (Emil:Type:Existential :name 'e)))))))

  (describe "Emil:Context:lookup-binding"
    (it "empty"
      (expect (Emil:Context:lookup-binding
               (Emil:Context)
               'a)
              :to-equal nil))

    (it "singleton / miss"
      (expect (eval '(Emil:Context:lookup-binding
                      (Emil:Context
                       :entries (list (Emil:Context:Binding
                                       :variable 'b
                                       :type (Emil:Type:Never))))
                      'a))
              :to-equal nil))

    (it "singleton / hit"
      (expect (eval '(Emil:Context:lookup-binding
                      (Emil:Context
                       :entries (list (Emil:Context:Binding
                                       :variable 'a
                                       :type (Emil:Type:Never))))
                      'a))
              :to-equal (Emil:Type:Never)))

    (it "triple / hit"
      (expect (eval '(Emil:Context:lookup-binding
                      (Emil:Context
                       :entries (list (Emil:Context:Binding
                                       :variable 'a
                                       :type (Emil:Type:Any))
                                      (Emil:Context:Binding
                                       :variable 'b
                                       :type (Emil:Type:Never))
                                      (Emil:Type:Existential :name 'c)))
                      'b))
              :to-equal (Emil:Type:Never)))

    (it "triple / miss"
      (expect (eval '(Emil:Context:lookup-binding
                      (Emil:Context
                       :entries (list (Emil:Type:Existential :name 'a)
                                      (Emil:Context:Binding
                                       :variable 'b
                                       :type (Emil:Type:Never))
                                      (Emil:Type:Existential :name 'c)))
                      'a))
              :to-equal nil)))

  (describe "Emil:Context:lookup-solved"
    (it "empty"
      (expect (Emil:Context:lookup-solved
               (Emil:Context)
               (Emil:Type:Existential :name 'a))
              :to-equal nil))

    (it "singleton / miss"
      (expect (eval '(Emil:Context:lookup-solved
                      (Emil:Context
                       :entries (list (Emil:Context:Solution
                                       :variable (Emil:Type:Existential :name 'b)
                                       :type (Emil:Type:Never))))
                      (Emil:Type:Existential :name 'a)))
              :to-equal nil))

    (it "singleton / hit"
      (expect (eval '(Emil:Context:lookup-solved
                      (Emil:Context
                       :entries (list (Emil:Context:Solution
                                       :variable (Emil:Type:Existential :name 'a)
                                       :type (Emil:Type:Never))))
                      (Emil:Type:Existential :name 'a)))
              :to-equal (Emil:Type:Never)))

    (it "triple / hit"
      (expect (eval '(Emil:Context:lookup-solved
                      (Emil:Context
                       :entries (list (Emil:Context:Solution
                                       :variable (Emil:Type:Existential :name 'a)
                                       :type (Emil:Type:Any))
                                      (Emil:Context:Solution
                                       :variable (Emil:Type:Existential :name 'b)
                                       :type (Emil:Type:Never))
                                      (Emil:Type:Existential :name 'c)))
                      (Emil:Type:Existential :name 'a)))
              :to-equal (Emil:Type:Any)))

    (it "triple / miss"
      (expect (eval '(Emil:Context:lookup-solved
                      (Emil:Context
                       :entries (list (Emil:Type:Existential :name 'a)
                                      (Emil:Context:Solution
                                       :variable (Emil:Type:Existential :name 'b)
                                       :type (Emil:Type:Never))
                                      (Emil:Type:Existential :name 'c)))
                      (Emil:Type:Existential :name 'a)))
              :to-equal nil)))

  (describe "Emil:Context:drop-until-after"
    (it "empty"
      (expect (Emil:Context:drop-until-after
               (Emil:Context)
               (Emil:Type:Variable :name 'a))
              :to-equal (Emil:Context)))

    (it "singleton"
      (expect (Emil:Context:drop-until-after
               (Emil:Context
                :entries (list (Emil:Type:Variable :name 'a)))
               (Emil:Type:Variable :name 'a))
              :to-equal (Emil:Context)))

    (it "triple / match beginning"
      (expect (Emil:Context:drop-until-after
               (Emil:Context
                :entries (list (Emil:Type:Variable :name 'a)
                               (Emil:Type:Variable :name 'b)
                               (Emil:Type:Variable :name 'c)))
               (Emil:Type:Variable :name 'a))
              :to-equal (Emil:Context
                         :entries (list (Emil:Type:Variable :name 'b)
                                        (Emil:Type:Variable :name 'c)))))

    (it "triple / match center"
      (expect (Emil:Context:drop-until-after
               (Emil:Context
                :entries (list (Emil:Type:Variable :name 'a)
                               (Emil:Type:Variable :name 'b)
                               (Emil:Type:Variable :name 'c)))
               (Emil:Type:Variable :name 'b))
              :to-equal (Emil:Context
                         :entries (list (Emil:Type:Variable :name 'c)))))

    (it "triple / match end"
      (expect (Emil:Context:drop-until-after
               (Emil:Context
                :entries (list (Emil:Type:Variable :name 'a)
                               (Emil:Type:Variable :name 'b)
                               (Emil:Type:Variable :name 'c)))
               (Emil:Type:Variable :name 'c))
              :to-equal (Emil:Context))))

  (describe "Emil:Context:well-formed?"
    (describe "basic"
      (it "Emil:Type:Basic"
        (expect (Emil:Context:well-formed?
                 (Emil:Context)
                 (Emil:Type:Never))
                :to-equal t))

      (it "Emil:Type:Variable / true"
        (expect (Emil:Context:well-formed?
                 (Emil:Context
                  :entries (list (Emil:Type:Variable :name 'a)))
                 (Emil:Type:Variable :name 'a))
                :to-equal t))

      (it "Emil:Type:Variable / false"
        (expect (Emil:Context:well-formed?
                 (Emil:Context
                  :entries (list (Emil:Type:Variable :name 'a)))
                 (Emil:Type:Variable :name 'b))
                :to-equal nil))

      (it "Emil:Type:Existential / true"
        (expect (Emil:Context:well-formed?
                 (Emil:Context
                  :entries (list (Emil:Type:Existential :name 'a)))
                 (Emil:Type:Existential :name 'a))
                :to-equal t))

      (it "Emil:Type:Existential / false"
        (expect (Emil:Context:well-formed?
                 (Emil:Context
                  :entries (list (Emil:Type:Existential :name 'a)))
                 (Emil:Type:Existential :name 'b))
                :to-equal nil))

      (it "Emil:Type:Existential / true via lookup"
        (expect (eval '(Emil:Context:well-formed?
                        (Emil:Context
                         :entries (list (Emil:Context:Solution
                                         :variable (Emil:Type:Existential :name 'a)
                                         :type (Emil:Type:Never))))
                        (Emil:Type:Existential :name 'a)))
                :to-equal t))

      (it "Emil:Type:Arrow / false"
        (expect (eval '(Emil:Context:well-formed?
                        (Emil:Context
                         :entries (list (Emil:Context:Solution
                                         :variable (Emil:Type:Existential :name 'a)
                                         :type (Emil:Type:Never))))
                        (Emil:Type:Arrow
                         :arguments (list (Emil:Type:Existential :name 'b))
                         :returns (Emil:Type:Basic :name 'number)
                         :min-arity 1)))
                :to-equal nil))

      (it "Emil:Type:Arrow / true"
        (expect (eval '(Emil:Context:well-formed?
                        (Emil:Context
                         :entries (list (Emil:Context:Solution
                                         :variable (Emil:Type:Existential :name 'a)
                                         :type (Emil:Type:Never))))
                        (Emil:Type:Arrow
                         :arguments (list (Emil:Type:Existential :name 'a))
                         :returns (Emil:Type:Basic :name 'number)
                         :min-arity 1)))
                :to-equal t)
        )

      (it "Emil:Type:Forall / false"
        (expect (eval '(Emil:Context:well-formed?
                        (Emil:Context
                         :entries (list (Emil:Type:Variable :name 'a)))
                        (Emil:Type:Forall
                         :parameters (list (Emil:Type:Variable :name 'b))
                         :type (Emil:Type:Variable :name 'c))))
                :to-equal nil))

      (it "Emil:Type:Forall / true"
        (expect (eval '(Emil:Context:well-formed?
                        (Emil:Context
                         :entries (list (Emil:Type:Variable :name 'a)))
                        (Emil:Type:Forall
                         :parameters (list (Emil:Type:Variable :name 'b))
                         :type (Emil:Type:Variable :name 'b))))
                :to-equal t))))

  (describe "Emil:Context:resolve"
    (describe "basic"
      (it "Emil:Type:Basic"
        (expect (Emil:Context:resolve
                 (Emil:Context)
                 (Emil:Type:Never))
                :to-equal (Emil:Type:Never)))

      (it "Emil:Type:Variable"
        (expect (Emil:Context:resolve
                 (Emil:Context)
                 (Emil:Type:Variable :name 'a))
                :to-equal (Emil:Type:Variable :name 'a)))

      (it "Emil:Type:Existential"
        (expect (Emil:Context:resolve
                 (Emil:Context)
                 (Emil:Type:Existential :name 'a))
                :to-equal (Emil:Type:Existential :name 'a)))

      (it "Emil:Type:Existential / via lookup"
        (expect (eval '(Emil:Context:resolve
                        (Emil:Context
                         :entries (list (Emil:Context:Solution
                                         :variable (Emil:Type:Existential :name 'a)
                                         :type (Emil:Type:Never))))
                        (Emil:Type:Existential :name 'a)))
                :to-equal (Emil:Type:Never)))

      (it "Emil:Type:Arrow"
        (expect (eval '(Emil:Context:resolve
                        (Emil:Context
                         :entries (list (Emil:Context:Solution
                                         :variable (Emil:Type:Existential :name 'a)
                                         :type (Emil:Type:Any))
                                        (Emil:Context:Solution
                                         :variable (Emil:Type:Existential :name 'b)
                                         :type (Emil:Type:Never))
                                        (Emil:Context:Solution
                                         :variable (Emil:Type:Existential :name 'c)
                                         :type (Emil:Type:Void))))
                        (Emil:Type:Arrow
                         :arguments (list (Emil:Type:Existential :name 'a)
                                          (Emil:Type:Existential :name 'c))
                         :rest? t
                         :returns (Emil:Type:Existential :name 'b)
                         :min-arity 1)))
                :to-equal
                (Emil:Type:Arrow
                 :arguments (list (Emil:Type:Any)
                                  (Emil:Type:Void))
                 :rest? t
                 :returns (Emil:Type:Never)
                 :min-arity 1)))

      (it "Emil:Type:Forall"
        (expect (eval '(Emil:Context:resolve
                        (Emil:Context
                         :entries (list (Emil:Context:Solution
                                         :variable (Emil:Type:Existential :name 'a)
                                         :type (Emil:Type:Any))
                                        (Emil:Context:Solution
                                         :variable (Emil:Type:Existential :name 'b)
                                         :type (Emil:Type:Never))))
                        (Emil:Type:Forall
                         :parameters (list (Emil:Type:Variable :name 'a))
                         :type (Emil:Type:Existential :name 'b))))
                :to-equal
                (eval '(Emil:Type:Forall
                        :parameters (list (Emil:Type:Variable :name 'a))
                        :type (Emil:Type:Never)))))))

  (describe "Emil:Environment:lookup"
    (it "looks up a bound variable"
      (expect (eval '(Emil:Environment:lookup
                        (Emil:Context
                         :entries (list (Emil:Context:Binding
                                         :variable 'a
                                         :type (Emil:Type:Any))))
                        'a))
                :to-equal (Emil:Type:Any)))

    (it "resolves a bound variable"
      (expect (eval '(Emil:Environment:lookup
                        (Emil:Context
                         :entries (list (Emil:Context:Solution
                                         :variable (Emil:Type:Existential :name 'b)
                                         :type (Emil:Type:Any))
                                        (Emil:Context:Binding
                                         :variable 'a
                                         :type (Emil:Type:Existential :name 'b))))
                        'a))
                :to-equal (Emil:Type:Any)))

    (it "returns nil, if a variable is not bound"
      (expect (eval '(Emil:Environment:lookup
                        (Emil:Context
                         :entries (list (Emil:Context:Binding
                                         :variable 'a
                                         :type (Emil:Type:Any))))
                        'b))
                :to-equal nil))))
