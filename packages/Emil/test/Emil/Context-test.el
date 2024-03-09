;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'Emil/Context)

(describe "Emil:Context"
  (describe "Emil:Context:hole"
    (it "empty"
      (expect (Emil:Context:hole (Emil:Context)
                                 (Emil:Type:VarInst :name 'a))
              :to-equal nil))

    (it "singleton / hit"
      (expect (Emil:Context:hole
               (Emil:Context
                :entries (list (Emil:Type:VarInst :name 'a)))
               (Emil:Type:VarInst :name 'a))
              :to-equal (list (Emil:Context)
                              (Emil:Context))))

    (it "singleton / miss"
      (expect (Emil:Context:hole
               (Emil:Context
                :entries (list (Emil:Type:VarInst :name 'a)))
               (Emil:Type:VarInst :name 'b))
              :to-equal nil))

    (it "triple / hit"
      (expect (Emil:Context:hole
               (Emil:Context
                :entries (list (Emil:Type:VarInst :name 'a)
                               (Emil:Type:VarInst :name 'b)
                               (Emil:Type:VarInst :name 'c)))
               (Emil:Type:VarInst :name 'b))
              :to-equal (list
                         (Emil:Context
                          :entries (list (Emil:Type:VarInst :name 'a)))
                         (Emil:Context
                          :entries (list (Emil:Type:VarInst :name 'c))))))

    (it "triple / miss"
      (expect (Emil:Context:hole
               (Emil:Context
                :entries (list (Emil:Type:VarInst :name 'a)
                               (Emil:Type:VarInst :name 'b)
                               (Emil:Type:VarInst :name 'c)))
               (Emil:Type:VarInst :name 'd))
              :to-equal nil)))

  (describe "Emil:Context:double-hole"
    (it "empty"
      (expect (Emil:Context:double-hole
               (Emil:Context)
               (Emil:Type:VarInst :name 'a)
               (Emil:Type:VarInst :name 'b))
              :to-equal nil))

    (it "singleton"
      (expect (Emil:Context:double-hole
               (Emil:Context
                :entries (list (Emil:Type:VarInst :name 'a)))
               (Emil:Type:VarInst :name 'a)
               (Emil:Type:VarInst :name 'b))
              :to-equal nil))

    (it "triple / miss"
      (expect (Emil:Context:double-hole
               (Emil:Context
                :entries (list (Emil:Type:VarInst :name 'a)
                               (Emil:Type:VarInst :name 'b)
                               (Emil:Type:VarInst :name 'c)))
               (Emil:Type:VarInst :name 'a)
               (Emil:Type:VarInst :name 'd))
              :to-equal nil))

    (it "triple / hit"
      (expect (Emil:Context:double-hole
               (Emil:Context
                :entries (list (Emil:Type:VarInst :name 'a)
                               (Emil:Type:VarInst :name 'b)
                               (Emil:Type:VarInst :name 'c)))
               (Emil:Type:VarInst :name 'a)
               (Emil:Type:VarInst :name 'c))
              :to-equal (list
                         (Emil:Context)
                         (Emil:Context
                          :entries (list (Emil:Type:VarInst :name 'b)))
                         (Emil:Context))))

    (it "quintuple / miss"
      (expect (Emil:Context:double-hole
               (Emil:Context
                :entries (list (Emil:Type:VarInst :name 'a)
                               (Emil:Type:VarInst :name 'b)
                               (Emil:Type:VarInst :name 'c)
                               (Emil:Type:VarInst :name 'd)
                               (Emil:Type:VarInst :name 'e)))
               (Emil:Type:VarInst :name 'b)
               (Emil:Type:VarInst :name 'f))
              :to-equal nil))

    (it "quintuple / hit"
      (expect (Emil:Context:double-hole
               (Emil:Context
                :entries (list (Emil:Type:VarInst :name 'a)
                               (Emil:Type:VarInst :name 'b)
                               (Emil:Type:VarInst :name 'c)
                               (Emil:Type:VarInst :name 'd)
                               (Emil:Type:VarInst :name 'e)))
               (Emil:Type:VarInst :name 'b)
               (Emil:Type:VarInst :name 'd))
              :to-equal (list
                         (Emil:Context
                          :entries (list (Emil:Type:VarInst :name 'a)))
                         (Emil:Context
                          :entries (list (Emil:Type:VarInst :name 'c)))
                         (Emil:Context
                          :entries (list (Emil:Type:VarInst :name 'e)))))))

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
                                      (Emil:Type:VarInst :name 'c)))
                      'b))
              :to-equal (Emil:Type:Never)))

    (it "triple / miss"
      (expect (eval '(Emil:Context:lookup-binding
                      (Emil:Context
                       :entries (list (Emil:Type:VarInst :name 'a)
                                      (Emil:Context:Binding
                                       :variable 'b
                                       :type (Emil:Type:Never))
                                      (Emil:Type:VarInst :name 'c)))
                      'a))
              :to-equal nil)))

  (describe "Emil:Context:lookup-solved"
    (it "empty"
      (expect (Emil:Context:lookup-solved
               (Emil:Context)
               (Emil:Type:VarInst :name 'a))
              :to-equal nil))

    (it "singleton / miss"
      (expect (eval '(Emil:Context:lookup-solved
                      (Emil:Context
                       :entries (list (Emil:Context:SolvedVarInst
                                       :variable (Emil:Type:VarInst :name 'b)
                                       :type (Emil:Type:Never))))
                      (Emil:Type:VarInst :name 'a)))
              :to-equal nil))

    (it "singleton / hit"
      (expect (eval '(Emil:Context:lookup-solved
                      (Emil:Context
                       :entries (list (Emil:Context:SolvedVarInst
                                       :variable (Emil:Type:VarInst :name 'a)
                                       :type (Emil:Type:Never))))
                      (Emil:Type:VarInst :name 'a)))
              :to-equal (Emil:Type:Never)))

    (it "triple / hit"
      (expect (eval '(Emil:Context:lookup-solved
                      (Emil:Context
                       :entries (list (Emil:Context:SolvedVarInst
                                       :variable (Emil:Type:VarInst :name 'a)
                                       :type (Emil:Type:Any))
                                      (Emil:Context:SolvedVarInst
                                       :variable (Emil:Type:VarInst :name 'b)
                                       :type (Emil:Type:Never))
                                      (Emil:Type:VarInst :name 'c)))
                      (Emil:Type:VarInst :name 'a)))
              :to-equal (Emil:Type:Any)))

    (it "triple / miss"
      (expect (eval '(Emil:Context:lookup-solved
                      (Emil:Context
                       :entries (list (Emil:Type:VarInst :name 'a)
                                      (Emil:Context:SolvedVarInst
                                       :variable (Emil:Type:VarInst :name 'b)
                                       :type (Emil:Type:Never))
                                      (Emil:Type:VarInst :name 'c)))
                      (Emil:Type:VarInst :name 'a)))
              :to-equal nil)))

  (describe "Emil:Context:drop-until-after"
    (it "empty"
      (expect (Emil:Context:drop-until-after
               (Emil:Context)
               (Emil:Type:Var :name 'a))
              :to-equal (Emil:Context)))

    (it "singleton"
      (expect (Emil:Context:drop-until-after
               (Emil:Context
                :entries (list (Emil:Type:Var :name 'a)))
               (Emil:Type:Var :name 'a))
              :to-equal (Emil:Context)))

    (it "triple / match beginning"
      (expect (Emil:Context:drop-until-after
               (Emil:Context
                :entries (list (Emil:Type:Var :name 'a)
                               (Emil:Type:Var :name 'b)
                               (Emil:Type:Var :name 'c)))
               (Emil:Type:Var :name 'a))
              :to-equal (Emil:Context
                         :entries (list (Emil:Type:Var :name 'b)
                                        (Emil:Type:Var :name 'c)))))

    (it "triple / match center"
      (expect (Emil:Context:drop-until-after
               (Emil:Context
                :entries (list (Emil:Type:Var :name 'a)
                               (Emil:Type:Var :name 'b)
                               (Emil:Type:Var :name 'c)))
               (Emil:Type:Var :name 'b))
              :to-equal (Emil:Context
                         :entries (list (Emil:Type:Var :name 'c)))))

    (it "triple / match end"
      (expect (Emil:Context:drop-until-after
               (Emil:Context
                :entries (list (Emil:Type:Var :name 'a)
                               (Emil:Type:Var :name 'b)
                               (Emil:Type:Var :name 'c)))
               (Emil:Type:Var :name 'c))
              :to-equal (Emil:Context))))

  (describe "Emil:Context:well-formed?"
    (describe "basic"
      (it "Emil:Type:Basic"
        (expect (Emil:Context:well-formed?
                 (Emil:Context)
                 (Emil:Type:Never))
                :to-equal t))

      (it "Emil:Type:Var / true"
        (expect (Emil:Context:well-formed?
                 (Emil:Context
                  :entries (list (Emil:Type:Var :name 'a)))
                 (Emil:Type:Var :name 'a))
                :to-equal t))

      (it "Emil:Type:Var / false"
        (expect (Emil:Context:well-formed?
                 (Emil:Context
                  :entries (list (Emil:Type:Var :name 'a)))
                 (Emil:Type:Var :name 'b))
                :to-equal nil))

      (it "Emil:Type:VarInst / true"
        (expect (Emil:Context:well-formed?
                 (Emil:Context
                  :entries (list (Emil:Type:VarInst :name 'a)))
                 (Emil:Type:VarInst :name 'a))
                :to-equal t))

      (it "Emil:Type:VarInst / false"
        (expect (Emil:Context:well-formed?
                 (Emil:Context
                  :entries (list (Emil:Type:VarInst :name 'a)))
                 (Emil:Type:VarInst :name 'b))
                :to-equal nil))

      (it "Emil:Type:VarInst / true via lookup"
        (expect (eval '(Emil:Context:well-formed?
                        (Emil:Context
                         :entries (list (Emil:Context:SolvedVarInst
                                         :variable (Emil:Type:VarInst :name 'a)
                                         :type (Emil:Type:Never))))
                        (Emil:Type:VarInst :name 'a)))
                :to-equal t))

      (it "Emil:Type:Fn / false"
        (expect (eval '(Emil:Context:well-formed?
                        (Emil:Context
                         :entries (list (Emil:Context:SolvedVarInst
                                         :variable (Emil:Type:VarInst :name 'a)
                                         :type (Emil:Type:Never))))
                        (Emil:Type:Fn
                         :argument-types (list (Emil:Type:VarInst :name 'b))
                         :return-type (Emil:Type:Basic :name 'number)
                         :min-arity 1)))
                :to-equal nil))

      (it "Emil:Type:Fn / true"
        (expect (eval '(Emil:Context:well-formed?
                        (Emil:Context
                         :entries (list (Emil:Context:SolvedVarInst
                                         :variable (Emil:Type:VarInst :name 'a)
                                         :type (Emil:Type:Never))))
                        (Emil:Type:Fn
                         :argument-types (list (Emil:Type:VarInst :name 'a))
                         :return-type (Emil:Type:Basic :name 'number)
                         :min-arity 1)))
                :to-equal t)
        )

      (it "Emil:Type:Forall / false"
        (expect (eval '(Emil:Context:well-formed?
                        (Emil:Context
                         :entries (list (Emil:Type:Var :name 'a)))
                        (Emil:Type:Forall
                         :variables (list (Emil:Type:Var :name 'b))
                         :type (Emil:Type:Var :name 'c))))
                :to-equal nil))

      (it "Emil:Type:Forall / true"
        (expect (eval '(Emil:Context:well-formed?
                        (Emil:Context
                         :entries (list (Emil:Type:Var :name 'a)))
                        (Emil:Type:Forall
                         :variables (list (Emil:Type:Var :name 'b))
                         :type (Emil:Type:Var :name 'b))))
                :to-equal t))))

  (describe "Emil:Context:resolve"
    (describe "basic"
      (it "Emil:Type:Basic"
        (expect (Emil:Context:resolve
                 (Emil:Context)
                 (Emil:Type:Never))
                :to-equal (Emil:Type:Never)))

      (it "Emil:Type:Var"
        (expect (Emil:Context:resolve
                 (Emil:Context)
                 (Emil:Type:Var :name 'a))
                :to-equal (Emil:Type:Var :name 'a)))

      (it "Emil:Type:VarInst"
        (expect (Emil:Context:resolve
                 (Emil:Context)
                 (Emil:Type:VarInst :name 'a))
                :to-equal (Emil:Type:VarInst :name 'a)))

      (it "Emil:Type:VarInst / via lookup"
        (expect (eval '(Emil:Context:resolve
                        (Emil:Context
                         :entries (list (Emil:Context:SolvedVarInst
                                         :variable (Emil:Type:VarInst :name 'a)
                                         :type (Emil:Type:Never))))
                        (Emil:Type:VarInst :name 'a)))
                :to-equal (Emil:Type:Never)))

      (it "Emil:Type:Fn"
        (expect (eval '(Emil:Context:resolve
                        (Emil:Context
                         :entries (list (Emil:Context:SolvedVarInst
                                         :variable (Emil:Type:VarInst :name 'a)
                                         :type (Emil:Type:Any))
                                        (Emil:Context:SolvedVarInst
                                         :variable (Emil:Type:VarInst :name 'b)
                                         :type (Emil:Type:Never))
                                        (Emil:Context:SolvedVarInst
                                         :variable (Emil:Type:VarInst :name 'c)
                                         :type (Emil:Type:Void))))
                        (Emil:Type:Fn
                         :argument-types (list (Emil:Type:VarInst :name 'a))
                         :rest-type (Emil:Type:VarInst :name 'c)
                         :return-type (Emil:Type:VarInst :name 'b)
                         :min-arity 1)))
                :to-equal
                (Emil:Type:Fn
                 :argument-types (list (Emil:Type:Any))
                 :rest-type (Emil:Type:Void)
                 :return-type (Emil:Type:Never)
                 :min-arity 1)))

      (it "Emil:Type:ForAll"
        (expect (eval '(Emil:Context:resolve
                        (Emil:Context
                         :entries (list (Emil:Context:SolvedVarInst
                                         :variable (Emil:Type:VarInst :name 'a)
                                         :type (Emil:Type:Any))
                                        (Emil:Context:SolvedVarInst
                                         :variable (Emil:Type:VarInst :name 'b)
                                         :type (Emil:Type:Never))))
                        (Emil:Type:Forall
                         :variables (list (Emil:Type:Var :name 'a))
                         :type (Emil:Type:VarInst :name 'b))))
                :to-equal
                (eval '(Emil:Type:Forall
                        :variables (list (Emil:Type:Var :name 'a))
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
                         :entries (list (Emil:Context:SolvedVarInst
                                         :variable (Emil:Type:VarInst :name 'b)
                                         :type (Emil:Type:Any))
                                        (Emil:Context:Binding
                                         :variable 'a
                                         :type (Emil:Type:VarInst :name 'b))))
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
