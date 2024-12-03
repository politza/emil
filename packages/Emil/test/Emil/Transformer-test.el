;; -*- lexical-binding: t -*-

;; Copyright (C) 2024  Andreas Politz

;; Author: Andreas Politz

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

(require 'buttercup)
(require 'Emil)

(describe "Emil:transform*"
  (describe "basic values"
    (it "nil"
      (expect (Emil:transform* nil)
              :to-equal
              '(Emil:Form:Atom
                :value nil
                :type (Emil:Type:Null))))

    (it "t"
      (expect (Emil:transform* t)
              :to-equal
              '(Emil:Form:Atom
                :value t
                :type (Emil:Type:Basic :name symbol))))

    (it "keyword"
      (expect (Emil:transform* :keyword)
              :to-equal
              '(Emil:Form:Atom
                :value :keyword
                :type (Emil:Type:Basic :name symbol))))

    (it "string"
      (expect (Emil:transform* "string")
              :to-equal
              '(Emil:Form:Atom
                :value "string"
                :type (Emil:Type:Basic :name string))))

    (it "integer"
      (expect (Emil:transform* 0)
              :to-equal
              '(Emil:Form:Atom
                :value 0
                :type (Emil:Type:Basic :name integer))))

    (it "float"
      (expect (Emil:transform* 0.0)
              :to-equal
              '(Emil:Form:Atom
                :value 0.0
                :type (Emil:Type:Basic :name float))))

    (it "vector"
      (expect (Emil:transform* [])
              :to-equal
              '(Emil:Form:Atom
                :value []
                :type
                (Emil:Type:Basic :name vector))))

    (it "record"
      (expect (Emil:transform* (record 'test 0 1 2))
              :to-equal
              `(Emil:Form:Atom
                :value ,(record 'test 0 1 2)
                :type
                (Emil:Type:Basic :name test)))))

  (describe "lambda"
    (it "identity"
      (expect (Emil:transform* '(lambda (x) x))
              :to-equal
              '(Emil:Form:Function
                :value (Emil:Form:Lambda
                        :arguments (x)
                        :body ((Emil:Form:Atom
                                :value x
                                :type (Emil:Type:Existential :name a)))
                        :type (Emil:Type:Arrow :arguments
                                       ((Emil:Type:Existential :name a))
                                       :rest? nil :returns
                                       (Emil:Type:Existential :name a)
                                       :min-arity 1))
                :type (Emil:Type:Arrow :arguments
                                       ((Emil:Type:Existential :name a))
                                       :rest? nil :returns
                                       (Emil:Type:Existential :name a)
                                       :min-arity 1))))
    (it "applied identity"
      (expect (Emil:transform*
               '(f (lambda (x) x) 0)
               (Emil:Env:Alist:read nil '((f . (-> ((-> ('a) 'a) 'a) 'a)))))
              
              :to-equal
              '(Emil:Form:Application
                :function
                (Emil:Form:Function
                 :value (Emil:Form:Atom
                         :value f
                         :type (Emil:Type:Forall
                                :parameters ((Emil:Type:Variable :name a))
                                :type (Emil:Type:Arrow
                                       :arguments
                                       ((Emil:Type:Arrow
                                         :arguments ((Emil:Type:Variable :name a))
                                         :rest? nil
                                         :returns (Emil:Type:Variable :name a)
                                         :min-arity 1)
                                        (Emil:Type:Variable :name a))
                                       :rest? nil
                                       :returns (Emil:Type:Variable :name a)
                                       :min-arity 2)))
                 :type
                 (Emil:Type:Forall
                  :parameters ((Emil:Type:Variable :name a))
                  :type (Emil:Type:Arrow
                         :arguments
                         ((Emil:Type:Arrow
                           :arguments ((Emil:Type:Variable :name a))
                           :rest? nil
                           :returns (Emil:Type:Variable :name a)
                           :min-arity 1)
                          (Emil:Type:Variable :name a))
                         :rest? nil
                         :returns (Emil:Type:Variable :name a)
                         :min-arity 2)))
                :arguments ((Emil:Form:Function
                             :value (Emil:Form:Lambda
                                     :arguments (x)
                                     :body ((Emil:Form:Atom
                                             :value x
                                             :type (Emil:Type:Existential :name a)))
                                     :type (Emil:Type:Arrow
                                            :arguments ((Emil:Type:Existential :name a))
                                            :rest? nil
                                            :returns (Emil:Type:Existential :name a)
                                            :min-arity 1))
                             :type (Emil:Type:Arrow
                                    :arguments ((Emil:Type:Existential :name a))
                                    :rest? nil
                                    :returns (Emil:Type:Existential :name a)
                                    :min-arity 1))
                            (Emil:Form:Atom
                             :value 0
                             :type (Emil:Type:Existential :name a)))
                :type (Emil:Type:Basic :name integer)))))

  (describe "let"
    (it "basic"
      (expect (Emil:transform* '(let ((a 0)) a))
              :to-equal
              '(Emil:Form:Let
                :kind let
                :bindings ((Emil:Form:Binding
                            :name a
                            :value (Emil:Form:Atom
                                    :value 0
                                    :type (Emil:Type:Basic :name integer))))
                :body ((Emil:Form:Atom
                        :value a
                        :type (Emil:Type:Basic :name integer)))
                :type (Emil:Type:Basic :name integer))))

    (it "default initializer"
      (expect (Emil:transform* '(let (a) a))
              :to-equal
              '(Emil:Form:Let
                :kind let
                :bindings ((Emil:Form:Binding
                            :name a
                            :value (Emil:Form:Atom
                                    :value nil
                                    :type (Emil:Type:Null))))
                :body ((Emil:Form:Atom
                        :value a
                        :type (Emil:Type:Null)))
                :type (Emil:Type:Null)))))

  (describe "let*"
    (it "basic"
      (expect (Emil:transform* '(let* ((a 0)) a))
              :to-equal
              '(Emil:Form:Let
                :kind let*
                :bindings ((Emil:Form:Binding
                            :name a
                            :value (Emil:Form:Atom
                                    :value 0
                                    :type (Emil:Type:Basic :name integer))))
                :body ((Emil:Form:Atom
                        :value a
                        :type (Emil:Type:Basic :name integer)))
                :type (Emil:Type:Basic :name integer)))))

  (describe "application"
    (it "identity"
      (expect (Emil:transform* '((lambda (x) x) 0))
              :to-equal
              '(Emil:Form:Application
                :function (Emil:Form:Function
                           :value (Emil:Form:Lambda
                                   :arguments (x)
                                   :body ((Emil:Form:Atom
                                           :value x
                                           :type (Emil:Type:Existential :name a)))
                                   :type (Emil:Type:Arrow
                                          :arguments ((Emil:Type:Existential :name a))
                                          :rest? nil
                                          :returns (Emil:Type:Existential :name a)
                                          :min-arity 1))
                           :type (Emil:Type:Arrow
                                  :arguments ((Emil:Type:Existential :name a))
                                  :rest? nil
                                  :returns (Emil:Type:Existential :name a)
                                  :min-arity 1))
                :arguments ((Emil:Form:Atom
                             :value 0
                             :type (Emil:Type:Existential :name a)))
                :type (Emil:Type:Basic :name integer)))))

  (describe "and"
    (it "basic"
      (expect (Emil:transform* '(and 0))
              :to-equal
              '(Emil:Form:And
                :conditions ((Emil:Form:Atom
                              :value 0
                              :type (Emil:Type:Basic :name integer)))
                :type (Emil:Type:Any)))))

  (describe "catch"
    (it "basic"
      (expect (Emil:transform* '(catch 'tag 0))
              :to-equal
              '(Emil:Form:Catch
                :tag (Emil:Form:Quote
                      :value tag
                      :type (Emil:Type:Basic :name symbol))
                :body ((Emil:Form:Atom
                        :value 0
                        :type (Emil:Type:Basic :name integer)))
                :type (Emil:Type:Any)))))

  (describe "cond"
    (it "basic"
      (expect (Emil:transform* '(cond (nil 0)))
              :to-equal
              '(Emil:Form:Cond
                :clauses (((Emil:Form:Atom
                            :value nil
                            :type (Emil:Type:Null))
                           (Emil:Form:Atom
                            :value 0
                            :type (Emil:Type:Basic :name integer))))
                :type (Emil:Type:Any)))))

  (describe "condition-case"
    (it "basic"
      (expect (Emil:transform* '(condition-case variable
                                    0
                                  (error variable)))
              :to-equal
              '(Emil:Form:ConditionCase
                :variable variable
                :body-form (Emil:Form:Atom
                            :value 0
                            :type (Emil:Type:Basic :name integer))
                :handlers ((Emil:Form:ConditionCaseHandler
                            :condition error
                            :body ((Emil:Form:Atom
                                    :value variable
                                    :type (Emil:Type:Any)))))
                :type (Emil:Type:Basic :name integer)))))

  (describe "defconst"
    (it "basic"
      (expect (Emil:transform* '(defconst a 0 "0"))
              :to-equal
              '(Emil:Form:DefConst
                :symbol a
                :init-value (Emil:Form:Atom
                             :value 0
                             :type (Emil:Type:Basic :name integer))
                :documentation "0"
                :type (Emil:Type:Basic :name symbol)))))

  (describe "defvar"
    (it "basic"
      (expect (Emil:transform* '(defvar a 0 "0"))
              :to-equal
              '(Emil:Form:DefVar
                :symbol a
                :init-value (Emil:Form:Atom
                             :value 0
                             :type (Emil:Type:Basic :name integer))
                :documentation "0"
                :type (Emil:Type:Basic :name symbol)))))

  (describe "if"
    (it "basic"
      (expect (Emil:transform* '(if 0 1 2))
              :to-equal
              '(Emil:Form:If
                :condition (Emil:Form:Atom
                            :value 0
                            :type (Emil:Type:Basic :name integer))
                :then (Emil:Form:Atom
                       :value 1
                       :type (Emil:Type:Basic :name integer))
                :else ((Emil:Form:Atom
                        :value 2
                        :type (Emil:Type:Basic :name integer)))
                :type (Emil:Type:Any)))))

  (describe "interactive"
    (it "basic"
      (expect (Emil:transform* '(interactive "p"))
              :to-equal
              '(Emil:Form:Interactive
                :forms ("p")
                :type (Emil:Type:Any)))))

  (describe "or"
    (it "basic"
      (expect (Emil:transform* '(or 0))
              :to-equal
              '(Emil:Form:Or
                :conditions ((Emil:Form:Atom
                              :value 0
                              :type (Emil:Type:Basic :name integer)))
                :type (Emil:Type:Any)))))

  (describe "prog1"
    (it "basic"
      (expect (Emil:transform* '(prog1 0 1))
              :to-equal
              '(Emil:Form:Prog1
                :first (Emil:Form:Atom
                        :value 0
                        :type (Emil:Type:Basic :name integer))
                :body ((Emil:Form:Atom
                        :value 1
                        :type (Emil:Type:Basic :name integer)))
                :type (Emil:Type:Basic :name integer)))))

  (describe "progn"
    (it "basic"
      (expect (Emil:transform* '(progn 0 1))
              :to-equal
              '(Emil:Form:PrognLike
                :kind progn
                :body ((Emil:Form:Atom
                        :value 0
                        :type (Emil:Type:Basic :name integer))
                       (Emil:Form:Atom
                        :value 1
                        :type (Emil:Type:Basic :name integer)))
                :type (Emil:Type:Basic :name integer)))))

  (describe "quote"
    (it "basic"
      (expect (Emil:transform* '(quote x))
              :to-equal
              '(Emil:Form:Quote
                :value x
                :type (Emil:Type:Basic :name symbol)))))

  (describe "save-current-buffer"
    (it "basic"
      (expect (Emil:transform* '(save-current-buffer 0 1))
              :to-equal
              '(Emil:Form:PrognLike
                :kind save-current-buffer
                :body ((Emil:Form:Atom
                        :value 0
                        :type (Emil:Type:Basic :name integer))
                       (Emil:Form:Atom
                        :value 1
                        :type (Emil:Type:Basic :name integer)))
                :type (Emil:Type:Basic :name integer)))))

  (describe "save-excursion"
    (it "basic"
      (expect (Emil:transform* '(save-excursion 0 1))
              :to-equal
              '(Emil:Form:PrognLike
                :kind save-excursion
                :body ((Emil:Form:Atom
                        :value 0
                        :type (Emil:Type:Basic :name integer))
                       (Emil:Form:Atom
                        :value 1
                        :type (Emil:Type:Basic :name integer)))
                :type (Emil:Type:Basic :name integer)))))

  (describe "save-restriction"
    (it "basic"
      (expect (Emil:transform* '(save-restriction 0 1))
              :to-equal
              '(Emil:Form:PrognLike
                :kind save-restriction
                :body ((Emil:Form:Atom
                        :value 0
                        :type (Emil:Type:Basic :name integer))
                       (Emil:Form:Atom
                        :value 1
                        :type (Emil:Type:Basic :name integer)))
                :type (Emil:Type:Basic :name integer)))))

  (describe "setq"
    (it "basic"
      (expect (Emil:transform* '(setq a 0 b 1))
              :to-equal
              '(Emil:Form:Setq
                :bindings ((Emil:Form:Binding
                            :name a
                            :value (Emil:Form:Atom
                                    :value 0
                                    :type (Emil:Type:Basic :name integer)))
                           (Emil:Form:Binding
                            :name b
                            :value (Emil:Form:Atom
                                    :value 1
                                    :type (Emil:Type:Basic :name integer))))
                :type (Emil:Type:Any)))))

  (describe "unwind-protect"
    (it "basic"
      (expect (Emil:transform* '(unwind-protect 0 1))
              :to-equal
              '(Emil:Form:UnwindProtect
                :body-form (Emil:Form:Atom
                            :value 0
                            :type (Emil:Type:Basic :name integer))
                :unwind-forms ((Emil:Form:Atom
                                :value 1
                                :type (Emil:Type:Basic :name integer)))
                :type (Emil:Type:Basic :name integer)))))

  (describe "while"
    (it "basic"
      (expect (Emil:transform* '(while 0 1))
              :to-equal
              '(Emil:Form:While
                :condition (Emil:Form:Atom
                            :value 0
                            :type (Emil:Type:Basic :name integer))
                :body ((Emil:Form:Atom
                        :value 1
                        :type (Emil:Type:Basic :name integer)))
                :type (Emil:Type:Null))))))
