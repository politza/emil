;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'Emil)

(describe "Emil:transform*"
  (describe "basic values"
    (it "nil"
      (expect (Emil:transform* nil)
              :to-equal
              '(Emil:TypedForm:Atom
                :value nil
                :type (Emil:Type:Null)
                :environment
                (Emil:Env:Alist
                 :variables nil :functions nil :macros nil :parent nil))))

    (it "t"
      (expect (Emil:transform* t)
              :to-equal
              '(Emil:TypedForm:Atom
                :value t
                :type (Emil:Type:Basic :name symbol)
                :environment
                (Emil:Env:Alist
                 :variables nil :functions nil :macros nil :parent nil))))

    (it "keyword"
      (expect (Emil:transform* :keyword)
              :to-equal
              '(Emil:TypedForm:Atom
                :value :keyword
                :type (Emil:Type:Basic :name symbol)
                :environment
                (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil))))

    (it "string"
      (expect (Emil:transform* "string")
              :to-equal
              '(Emil:TypedForm:Atom
                :value "string"
                :type (Emil:Type:Basic :name string)
                :environment
                (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil))))

    (it "integer"
      (expect (Emil:transform* 0)
              :to-equal
              '(Emil:TypedForm:Atom
                :value 0
                :type (Emil:Type:Basic :name integer)
                :environment
                (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil))))

    (it "float"
      (expect (Emil:transform* 0.0)
              :to-equal
              '(Emil:TypedForm:Atom
                :value 0.0
                :type (Emil:Type:Basic :name float)
                :environment
                (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil))))

    (it "vector"
      (expect (Emil:transform* [])
              :to-equal
              '(Emil:TypedForm:Atom
                :value []
                :type
                (Emil:Type:Basic :name vector)
                :environment
                (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil)))))

  (describe "lambda"
    (it "identity"
      (expect (Emil:transform* '(lambda (x) x))
              :to-equal
              '(Emil:TypedForm:Function
                :value (Emil:TypedForm:Lambda
                        :arguments (x)
                        :body ((Emil:TypedForm:Atom
                                :value x
                                :type (Emil:Type:Existential :name a)
                                :environment (Emil:Env:Alist
                                              :variables ((x Emil:Type:Existential :name a))
                                              :functions nil :macros nil
                                              :parent (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil)))))
                :type (Emil:Type:Arrow :arguments
                                       ((Emil:Type:Existential :name a))
                                       :rest? nil :returns
                                       (Emil:Type:Existential :name a)
                                       :min-arity 1)
                :environment
                (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil)))))

  (describe "let"
    (it "basic"
      (expect (Emil:transform* '(let ((a 0)) a))
              :to-equal
              '(Emil:TypedForm:Let
                :kind let
                :bindings ((Emil:TypedForm:Binding
                            :name a
                            :value (Emil:TypedForm:Atom
                                    :value 0
                                    :type (Emil:Type:Basic :name integer)
                                    :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil))))
                :body ((Emil:TypedForm:Atom
                        :value a
                        :type (Emil:Type:Basic :name integer)
                        :environment (Emil:Env:Alist
                                      :variables ((a Emil:Type:Basic :name integer))
                                      :functions nil :macros nil
                                      :parent (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil))))
                :type (Emil:Type:Basic :name integer)
                :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil)))))

  (describe "let*"
    (it "basic"
      (expect (Emil:transform* '(let* ((a 0)) a))
              :to-equal
              '(Emil:TypedForm:Let
                :kind let*
                :bindings ((Emil:TypedForm:Binding
                            :name a
                            :value (Emil:TypedForm:Atom
                                    :value 0
                                    :type (Emil:Type:Basic :name integer)
                                    :environment (Emil:Env:Alist
                                                  :variables nil
                                                  :functions nil :macros nil
                                                  :parent (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil)))))
                :body ((Emil:TypedForm:Atom
                        :value a
                        :type (Emil:Type:Basic :name integer)
                        :environment (Emil:Env:Alist
                                      :variables ((a Emil:Type:Basic :name integer))
                                      :functions nil :macros nil
                                      :parent (Emil:Env:Alist
                                               :variables nil
                                               :functions nil :macros nil
                                               :parent (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil)))))
                :type (Emil:Type:Basic :name integer)
                :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil)))))

  (describe "application"
    (it "identity"
      (expect (Emil:transform* '((lambda (x) x) 0))
              :to-equal
              '(Emil:TypedForm:Application
                :function (Emil:TypedForm:Function
                           :value (Emil:TypedForm:Lambda
                                   :arguments (x)
                                   :body ((Emil:TypedForm:Atom
                                           :value x
                                           :type (Emil:Type:Existential :name a)
                                           :environment
                                           (Emil:Env:Alist
                                            :variables
                                            ((x Emil:Type:Existential :name a))
                                            :functions nil :macros nil
                                            :parent
                                            (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil)))))
                           :type (Emil:Type:Arrow
                                  :arguments ((Emil:Type:Existential :name a))
                                  :rest? nil
                                  :returns (Emil:Type:Existential :name b)
                                  :min-arity 1)
                           :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil))
                :arguments ((Emil:TypedForm:Atom
                             :value 0
                             :type (Emil:Type:Existential :name a)
                             :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil)))
                :type (Emil:Type:Basic :name integer)
                :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil)))))

  (describe "and"
    (it "basic"
      (expect (Emil:transform* '(and 0))
              :to-equal
              '(Emil:TypedForm:And
                :conditions ((Emil:TypedForm:Atom
                              :value 0
                              :type (Emil:Type:Basic :name integer)
                              :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil)))
                :type (Emil:Type:Any)
                :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil)))))

  (describe "catch"
    (it "basic"
      (expect (Emil:transform* '(catch 'tag 0))
              :to-equal
              '(Emil:TypedForm:Catch
                :tag (Emil:TypedForm:Quote
                      :value tag
                      :type (Emil:Type:Any)
                      :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil))
                :body ((Emil:TypedForm:Atom
                        :value 0
                        :type (Emil:Type:Basic :name integer)
                        :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil)))
                :type (Emil:Type:Any)
                :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil)))))

  (describe "cond"
    (it "basic"
      (expect (Emil:transform* '(cond (nil 0)))
              :to-equal
              '(Emil:TypedForm:Cond
                :clauses ((Emil:TypedForm:Clause
                           :condition (Emil:TypedForm:Atom
                                       :value nil
                                       :type (Emil:Type:Null)
                                       :environment
                                       (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil))
                           :body ((Emil:TypedForm:Atom
                                   :value 0
                                   :type (Emil:Type:Basic :name integer)
                                   :environment
                                   (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil)))))
                :type (Emil:Type:Any)
                :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil)))))

  (describe "defconst"
    (it "basic"
      (expect (Emil:transform* '(defconst a 0 "0"))
              :to-equal
              '(Emil:TypedForm:DefConst
                :symbol a
                :init-value (Emil:TypedForm:Atom
                             :value 0
                             :type (Emil:Type:Basic :name integer)
                             :environment
                             (Emil:Env:Alist :variables nil
                                             :functions nil :macros nil :parent nil))
                :documentation "0"
                :type (Emil:Type:Basic :name symbol)
                :environment (Emil:Env:Alist :variables nil
                                             :functions nil :macros nil :parent nil)))))

  (describe "defvar"
    (it "basic"
      (expect (Emil:transform* '(defvar a 0 "0"))
              :to-equal
              '(Emil:TypedForm:DefVar
                :symbol a
                :init-value (Emil:TypedForm:Atom
                             :value 0
                             :type (Emil:Type:Basic :name integer)
                             :environment
                             (Emil:Env:Alist :variables nil
                                             :functions nil :macros nil :parent nil))
                :documentation "0"
                :type (Emil:Type:Basic :name symbol)
                :environment (Emil:Env:Alist :variables nil
                                             :functions nil :macros nil :parent nil)))))

  (describe "if"
    (it "basic"
      (expect (Emil:transform* '(if 0 1 2))
              :to-equal
              '(Emil:TypedForm:If
                :condition (Emil:TypedForm:Atom
                            :value 0
                            :type (Emil:Type:Basic :name integer)
                            :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil))
                :then (Emil:TypedForm:Atom
                       :value 1
                       :type (Emil:Type:Basic :name integer)
                       :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil))
                :else ((Emil:TypedForm:Atom
                        :value 2
                        :type (Emil:Type:Basic :name integer)
                        :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil)))
                :type (Emil:Type:Any)
                :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil)))))

  (describe "interactive"
    (it "basic"
      (expect (Emil:transform* '(interactive "p"))
              :to-equal
              '(Emil:TypedForm:Interactive
                :forms ("p")
                :type (Emil:Type:Any)
                :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil)))))

  (describe "or"
    (it "basic"
      (expect (Emil:transform* '(or 0))
              :to-equal
              '(Emil:TypedForm:Or
                :conditions ((Emil:TypedForm:Atom
                              :value 0
                              :type (Emil:Type:Basic :name integer)
                              :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil)))
                :type (Emil:Type:Any)
                :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil)))))

  (describe "prog1"
    (it "basic"
      (expect (Emil:transform* '(prog1 0 1))
              :to-equal
              '(Emil:TypedForm:Prog1
                :first (Emil:TypedForm:Atom
                        :value 0
                        :type (Emil:Type:Basic :name integer)
                        :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil))
                :body ((Emil:TypedForm:Atom
                        :value 1
                        :type (Emil:Type:Basic :name integer)
                        :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil)))
                :type (Emil:Type:Basic :name integer)
                :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil)))))

  (describe "progn"
    (it "basic"
      (expect (Emil:transform* '(progn 0 1))
              :to-equal
              '(Emil:TypedForm:PrognLike
                :kind progn
                :body ((Emil:TypedForm:Atom
                        :value 0
                        :type (Emil:Type:Basic :name integer)
                        :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil))
                       (Emil:TypedForm:Atom
                        :value 1
                        :type (Emil:Type:Basic :name integer)
                        :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil)))
                :type (Emil:Type:Basic :name integer)
                :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil)))))

  (describe "quote"
    (it "basic"
      (expect (Emil:transform* '(quote x))
              :to-equal
              '(Emil:TypedForm:Quote
                :value x
                :type (Emil:Type:Any)
                :environment
                (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil)))))

  (describe "save-current-buffer"
    (it "basic"
      (expect (Emil:transform* '(save-current-buffer 0 1))
              :to-equal
              '(Emil:TypedForm:PrognLike
                :kind save-current-buffer
                :body ((Emil:TypedForm:Atom
                        :value 0
                        :type (Emil:Type:Basic :name integer)
                        :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil))
                       (Emil:TypedForm:Atom
                        :value 1
                        :type (Emil:Type:Basic :name integer)
                        :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil)))
                :type (Emil:Type:Basic :name integer)
                :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil)))))

  (describe "save-excursion"
    (it "basic"
      (expect (Emil:transform* '(save-excursion 0 1))
              :to-equal
              '(Emil:TypedForm:PrognLike
                :kind save-excursion
                :body ((Emil:TypedForm:Atom
                        :value 0
                        :type (Emil:Type:Basic :name integer)
                        :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil))
                       (Emil:TypedForm:Atom
                        :value 1
                        :type (Emil:Type:Basic :name integer)
                        :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil)))
                :type (Emil:Type:Basic :name integer)
                :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil)))))

  (describe "save-restriction"
    (it "basic"
      (expect (Emil:transform* '(save-restriction 0 1))
              :to-equal
              '(Emil:TypedForm:PrognLike
                :kind save-restriction
                :body ((Emil:TypedForm:Atom
                        :value 0
                        :type (Emil:Type:Basic :name integer)
                        :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil))
                       (Emil:TypedForm:Atom
                        :value 1
                        :type (Emil:Type:Basic :name integer)
                        :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil)))
                :type (Emil:Type:Basic :name integer)
                :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil)))))

  (describe "setq"
    (it "basic"
      (expect (Emil:transform* '(setq a 0 b 1))
              :to-equal
              '(Emil:TypedForm:Setq
                :bindings ((Emil:TypedForm:Binding
                            :name a
                            :value (Emil:TypedForm:Atom
                                    :value 0
                                    :type (Emil:Type:Basic :name integer)
                                    :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil)))
                           (Emil:TypedForm:Binding
                            :name b
                            :value (Emil:TypedForm:Atom
                                    :value 1
                                    :type (Emil:Type:Basic :name integer)
                                    :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil))))
                :type (Emil:Type:Any)
                :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil)))))

  (describe "unwind-protect"
    (it "basic"
      (expect (Emil:transform* '(unwind-protect 0 1))
              :to-equal
              '(Emil:TypedForm:UnwindProtect
                :body-form (Emil:TypedForm:Atom
                            :value 0
                            :type (Emil:Type:Basic :name integer)
                            :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil))
                :unwind-forms ((Emil:TypedForm:Atom
                                :value 1
                                :type (Emil:Type:Basic :name integer)
                                :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil)))
                :type (Emil:Type:Basic :name integer)
                :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil)))))

  (describe "while"
    (it "basic"
      (expect (Emil:transform* '(while 0 1))
              :to-equal
              '(Emil:TypedForm:While
                :condition (Emil:TypedForm:Atom
                            :value 0
                            :type (Emil:Type:Basic :name integer)
                            :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil))
                :body ((Emil:TypedForm:Atom
                        :value 1
                        :type (Emil:Type:Basic :name integer)
                        :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil)))
                :type (Emil:Type:Null)
                :environment (Emil:Env:Alist :variables nil :functions nil :macros nil :parent nil))))))
