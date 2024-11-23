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

(require 'Emil/Form)
(require 'buttercup)

(describe "Emil:Form"
  (describe "Emil:Form:each-child"
    (describe "basic"
      :var ((env (list (cons 'variable (Emil:Type:Never)))))

      (before-each
        (fset 'Emil:Form:Test:callback #'ignore)
        (spy-on 'Emil:Form:Test:callback))

      (it "Emil:Form:Invalid"
        (Emil:Form:each-child (Emil:Form:Invalid :form [0 1 2] :type (Emil:Type:Never))
                              'Emil:Form:Test:callback env)
        (expect (spy-calls-count 'Emil:Form:Test:callback) :to-equal 0))
      (it "Emil:Form:Atom"
        (Emil:Form:each-child (Emil:Form:Atom :value [0 1 2] :type (Emil:Type:Never))
                              'Emil:Form:Test:callback env)
        (expect (spy-calls-count 'Emil:Form:Test:callback) :to-equal 0))

      (it "Emil:Form:Application"
        (let ((function (Emil:Form:Function
                         :value (Emil:Form:Atom :value 'fn :type (Emil:Type:Never))
                         :type (Emil:Type:Never)))
              (arguments (list (Emil:Form:Atom :value 'variable :type (Emil:Type:Never)))))
          (Emil:Form:each-child (Emil:Form:Application
                                 :function function
                                 :arguments arguments
                                 :type (Emil:Type:Never))
                                'Emil:Form:Test:callback env)
          (expect (spy-calls-count 'Emil:Form:Test:callback) :to-equal 2)
          (expect (spy-calls-args-for 'Emil:Form:Test:callback 0)
                  :to-equal (list function env))
          (expect (spy-calls-args-for 'Emil:Form:Test:callback 1)
                  :to-equal (list (car arguments) env))))

      (it "Emil:Form:And"
        (let ((conditions (list (Emil:Form:Atom :value 0 :type (Emil:Type:Never))
                                (Emil:Form:Atom :value 1 :type (Emil:Type:Never)))))
          (Emil:Form:each-child (Emil:Form:And :conditions conditions
                                               :type (Emil:Type:Never))
                                'Emil:Form:Test:callback env)
          (expect (spy-calls-count 'Emil:Form:Test:callback) :to-equal 2)
          (expect (spy-calls-args-for 'Emil:Form:Test:callback 0)
                  :to-equal
                  (list (nth 0 conditions) env))
          (expect (spy-calls-args-for 'Emil:Form:Test:callback 1)
                  :to-equal
                  (list (nth 1 conditions) env))))

      (it "Emil:Form:Catch"
        (let ((tag (Emil:Form:Atom :value 0 :type (Emil:Type:Never)))
              (body (list (Emil:Form:Atom :value 1 :type (Emil:Type:Never))
                          (Emil:Form:Atom :value 2 :type (Emil:Type:Never)))))

          (Emil:Form:each-child (Emil:Form:Catch :tag tag
                                                 :body body
                                                 :type (Emil:Type:Never))
                                'Emil:Form:Test:callback env)
          (expect (spy-calls-count 'Emil:Form:Test:callback) :to-equal 3)
          (expect (spy-calls-args-for 'Emil:Form:Test:callback 0)
                  :to-equal (list tag env))
          (expect (spy-calls-args-for 'Emil:Form:Test:callback 1)
                  :to-equal (list (nth 0 body) env))
          (expect (spy-calls-args-for 'Emil:Form:Test:callback 2)
                  :to-equal (list (nth 1 body) env))))

      (it "Emil:Form:Cond"
        (let ((clauses (list (list (Emil:Form:Atom :value 0 :type (Emil:Type:Never))
                                   (Emil:Form:Atom :value 1 :type (Emil:Type:Never)))
                             (list (Emil:Form:Atom :value 2 :type (Emil:Type:Never))
                                   (Emil:Form:Atom :value 3 :type (Emil:Type:Never))))))
          (Emil:Form:each-child (Emil:Form:Cond :clauses clauses :type (Emil:Type:Never))
                                'Emil:Form:Test:callback env)
          (expect (spy-calls-count 'Emil:Form:Test:callback) :to-equal 4)
          (expect (spy-calls-args-for 'Emil:Form:Test:callback 0)
                  :to-equal (list (nth 0 (nth 0 clauses)) env))
          (expect (spy-calls-args-for 'Emil:Form:Test:callback 1)
                  :to-equal (list (nth 1 (nth 0 clauses)) env))
          (expect (spy-calls-args-for 'Emil:Form:Test:callback 2)
                  :to-equal (list (nth 0 (nth 1 clauses)) env))
          (expect (spy-calls-args-for 'Emil:Form:Test:callback 3)
                  :to-equal (list (nth 1 (nth 1 clauses)) env))))

      (it "Emil:Form:ConditionCase"
        (let* ((body-form (Emil:Form:Atom :value 0 :type (Emil:Type:Never)))
               (handler-form (Emil:Form:Atom :value 0 :type (Emil:Type:Never)))
               (handlers (list (Emil:Form:ConditionCaseHandler
                                :condition 'error
                                :body (list handler-form)))))
          (Emil:Form:each-child (Emil:Form:ConditionCase
                                 :variable 'variable
                                 :body-form body-form
                                 :handlers handlers
                                 :type (Emil:Type:Never))
                                'Emil:Form:Test:callback env)
          (expect (spy-calls-count 'Emil:Form:Test:callback) :to-equal 2)
          (expect (spy-calls-args-for 'Emil:Form:Test:callback 0)
                  :to-equal (list body-form env))
          (expect (spy-calls-args-for 'Emil:Form:Test:callback 1)
                  :to-equal (list handler-form (cons (cons 'variable (Emil:Type:Any))
                                                     env)))))

      (it "Emil:Form:DefConst"
        (let ((init-value (Emil:Form:Atom :value 0 :type (Emil:Type:Never))))
          (Emil:Form:each-child (Emil:Form:DefConst :symbol 'name :init-value init-value :type (Emil:Type:Never))
                                'Emil:Form:Test:callback env)
          (expect (spy-calls-count 'Emil:Form:Test:callback) :to-equal 1)
          (expect (spy-calls-args-for 'Emil:Form:Test:callback 0)
                  :to-equal (list init-value env))))

      (it "Emil:Form:DefVar"
        (let ((init-value (Emil:Form:Atom :value 0 :type (Emil:Type:Never))))
          (Emil:Form:each-child (Emil:Form:DefVar :symbol 'name :init-value init-value :type (Emil:Type:Never))
                                'Emil:Form:Test:callback env)
          (expect (spy-calls-count 'Emil:Form:Test:callback) :to-equal 1)
          (expect (spy-calls-args-for 'Emil:Form:Test:callback 0)
                  :to-equal (list init-value env))))

      (it "Emil:Form:Function"
        (let* ((lambda-form (Emil:Form:Atom :value 0 :type (Emil:Type:Never)))
               (value (Emil:Form:Lambda
                       :arguments (list 'variable)
                       :body (list lambda-form)
                       :type (Emil:Type:Arrow
                              :arguments (list (Emil:Type:Never))
                              :returns (Emil:Type:Never)
                              :min-arity 1))))
          (Emil:Form:each-child (Emil:Form:Function :value value :type (Emil:Type:Never))
                                'Emil:Form:Test:callback env)
          (expect (spy-calls-count 'Emil:Form:Test:callback) :to-equal 1)
          (expect (spy-calls-args-for 'Emil:Form:Test:callback 0)
                  :to-equal (list value env))))

      (it "Emil:Form:Lambda"
        (let* ((body-form (Emil:Form:Atom :value 0 :type (Emil:Type:Never)))
               (arguments (list 'variable))
               (body (list body-form))
               (type (Emil:Type:Arrow
                      :arguments (list (Emil:Type:Never))
                      :returns (Emil:Type:Never)
                      :min-arity 1)))
          (Emil:Form:each-child (Emil:Form:Lambda :arguments arguments :body body :type type)
                                'Emil:Form:Test:callback env)
          (expect (spy-calls-count 'Emil:Form:Test:callback) :to-equal 1)
          (expect (spy-calls-args-for 'Emil:Form:Test:callback 0)
                  :to-equal (list body-form (cons (cons 'variable (Emil:Type:Never))
                                                  env)))))

      (it "Emil:Form:If"
        (let ((condition (Emil:Form:Atom :value 0 :type (Emil:Type:Never)))
              (then (Emil:Form:Atom :value 1 :type (Emil:Type:Never)))
              (else (list (Emil:Form:Atom :value 2 :type (Emil:Type:Never)))))
          (Emil:Form:each-child (Emil:Form:If
                                 :condition condition
                                 :then then
                                 :else else
                                 :type (Emil:Type:Never))
                                'Emil:Form:Test:callback env)
          (expect (spy-calls-count 'Emil:Form:Test:callback) :to-equal 3)
          (expect (spy-calls-args-for 'Emil:Form:Test:callback 0)
                  :to-equal (list condition env))
          (expect (spy-calls-args-for 'Emil:Form:Test:callback 1)
                  :to-equal (list then env))
          (expect (spy-calls-args-for 'Emil:Form:Test:callback 2)
                  :to-equal (list (nth 0 else) env))))

      (it "Emil:Form:Interactive"
        (Emil:Form:each-child (Emil:Form:Interactive :forms (list 0 1 2) :type (Emil:Type:Never))
                              'Emil:Form:Test:callback env)
        (expect (spy-calls-count 'Emil:Form:Test:callback) :to-equal 0))

      (it "Emil:Form:Let (let)"
        (let* ((values (list (Emil:Form:Atom :value 0 :type (Emil:Type:Never))
                             (Emil:Form:Atom :value 1 :type (Emil:Type:Never))))
               (bindings (list (Emil:Form:Binding
                                :name 'variable-0
                                :value (nth 0 values))
                               (Emil:Form:Binding
                                :name 'variable-1
                                :value (nth 1 values))))
               (body-form (Emil:Form:Atom :value 2 :type (Emil:Type:Never))))
          (Emil:Form:each-child (Emil:Form:Let
                                 :kind 'let
                                 :bindings bindings
                                 :body (list body-form)
                                 :type (Emil:Type:Never))
                                'Emil:Form:Test:callback env)
          (expect (spy-calls-count 'Emil:Form:Test:callback) :to-equal 3)
          (expect (spy-calls-args-for 'Emil:Form:Test:callback 0)
                  :to-equal (list (nth 0 values) env))
          (expect (spy-calls-args-for 'Emil:Form:Test:callback 1)
                  :to-equal (list (nth 1 values) env))
          (expect (spy-calls-args-for 'Emil:Form:Test:callback 2)
                  :to-equal (list body-form (append (list (cons 'variable-1 (Emil:Type:Never))
                                                          (cons 'variable-0 (Emil:Type:Never)))
                                                    env)))))

      (it "Emil:Form:Let (let*)"
        (let* ((values (list (Emil:Form:Atom :value 0 :type (Emil:Type:Never))
                             (Emil:Form:Atom :value 1 :type (Emil:Type:Never))))
               (bindings (list (Emil:Form:Binding
                                :name 'variable-0
                                :value (nth 0 values))
                               (Emil:Form:Binding
                                :name 'variable-1
                                :value (nth 1 values))))
               (body-form (Emil:Form:Atom :value 2 :type (Emil:Type:Never))))
          (Emil:Form:each-child (Emil:Form:Let
                                 :kind 'let*
                                 :bindings bindings
                                 :body (list body-form)
                                 :type (Emil:Type:Never))
                                'Emil:Form:Test:callback env)
          (expect (spy-calls-count 'Emil:Form:Test:callback) :to-equal 3)
          (expect (spy-calls-args-for 'Emil:Form:Test:callback 0)
                  :to-equal (list (nth 0 values) env))
          (expect (spy-calls-args-for 'Emil:Form:Test:callback 1)
                  :to-equal (list (nth 1 values) (cons (cons 'variable-0 (Emil:Type:Never)) env)))
          (expect (spy-calls-args-for 'Emil:Form:Test:callback 2)
                  :to-equal (list body-form (append (list (cons 'variable-1 (Emil:Type:Never))
                                                          (cons 'variable-0 (Emil:Type:Never)))
                                                    env)))))

      (it "Emil:Form:Or"
        (let ((conditions (list (Emil:Form:Atom :value 0 :type (Emil:Type:Never))
                                (Emil:Form:Atom :value 1 :type (Emil:Type:Never)))))
          (Emil:Form:each-child (Emil:Form:Or :conditions conditions
                                              :type (Emil:Type:Never))
                                'Emil:Form:Test:callback env)
          (expect (spy-calls-count 'Emil:Form:Test:callback) :to-equal 2)
          (expect (spy-calls-args-for 'Emil:Form:Test:callback 0)
                  :to-equal
                  (list (nth 0 conditions) env))
          (expect (spy-calls-args-for 'Emil:Form:Test:callback 1)
                  :to-equal
                  (list (nth 1 conditions) env))))

      (it "Emil:Form:Prog1"
        (let ((first (Emil:Form:Atom :value 0 :type (Emil:Type:Never)))
              (body-form (Emil:Form:Atom :value 1 :type (Emil:Type:Never))))
          (Emil:Form:each-child (Emil:Form:Prog1 :first first :body (list body-form) :type (Emil:Type:Never))
                                'Emil:Form:Test:callback env)
          (expect (spy-calls-count 'Emil:Form:Test:callback) :to-equal 2)
          (expect (spy-calls-args-for 'Emil:Form:Test:callback 0)
                  :to-equal (list first env))
          (expect (spy-calls-args-for 'Emil:Form:Test:callback 1)
                  :to-equal (list body-form env))))

      (it "Emil:Form:PrognLike"
        (let ((body (list (Emil:Form:Atom :value 0 :type (Emil:Type:Never))
                          (Emil:Form:Atom :value 1 :type (Emil:Type:Never)))))
          (Emil:Form:each-child (Emil:Form:PrognLike :kind 'progn :body body :type (Emil:Type:Never))
                                'Emil:Form:Test:callback env)
          (expect (spy-calls-count 'Emil:Form:Test:callback) :to-equal 2)
          (expect (spy-calls-args-for 'Emil:Form:Test:callback 0)
                  :to-equal (list (nth 0 body) env))
          (expect (spy-calls-args-for 'Emil:Form:Test:callback 1)
                  :to-equal (list (nth 1 body) env))))

      (it "Emil:Form:Quote"
        (Emil:Form:each-child (Emil:Form:Quote :value 'symbol :type (Emil:Type:Never))
                              'Emil:Form:Test:callback env)
        (expect (spy-calls-count 'Emil:Form:Test:callback) :to-equal 0))

      (it "Emil:Form:Setq"
        (let* ((values (list (Emil:Form:Atom :value 0 :type (Emil:Type:Never))
                             (Emil:Form:Atom :value 1 :type (Emil:Type:Never))))
               (bindings (list (Emil:Form:Binding
                                :name 'variable-0
                                :value (nth 0 values))
                               (Emil:Form:Binding
                                :name 'variable-1
                                :value (nth 1 values)))))
          (Emil:Form:each-child (Emil:Form:Setq
                                 :bindings bindings
                                 :type (Emil:Type:Never))
                                'Emil:Form:Test:callback env)
          (expect (spy-calls-count 'Emil:Form:Test:callback) :to-equal 2)
          (expect (spy-calls-args-for 'Emil:Form:Test:callback 0)
                  :to-equal (list (nth 0 values) env))
          (expect (spy-calls-args-for 'Emil:Form:Test:callback 1)
                  :to-equal (list (nth 1 values) env))))

      (it "Emil:Form:UnwindProtect"
        (let ((body-form (Emil:Form:Atom :value 0 :type (Emil:Type:Never)))
              (unwind-form (Emil:Form:Atom :value 1 :type (Emil:Type:Never))))
          (Emil:Form:each-child (Emil:Form:UnwindProtect
                                 :body-form body-form
                                 :unwind-forms (list unwind-form)
                                 :type (Emil:Type:Never))
                                'Emil:Form:Test:callback env)
          (expect (spy-calls-count 'Emil:Form:Test:callback) :to-equal 2)
          (expect (spy-calls-args-for 'Emil:Form:Test:callback 0)
                  :to-equal (list body-form env))
          (expect (spy-calls-args-for 'Emil:Form:Test:callback 1)
                  :to-equal (list unwind-form env))))

      (it "Emil:Form:While"
        (let ((condition (Emil:Form:Atom :value 1 :type (Emil:Type:Never)))
              (body-form (Emil:Form:Atom :value 0 :type (Emil:Type:Never))))
          (Emil:Form:each-child (Emil:Form:While
                                 :condition condition
                                 :body (list body-form)
                                 :type (Emil:Type:Never))
                                'Emil:Form:Test:callback env)
          (expect (spy-calls-count 'Emil:Form:Test:callback) :to-equal 2)
          (expect (spy-calls-args-for 'Emil:Form:Test:callback 0)
                  :to-equal (list condition env))
          (expect (spy-calls-args-for 'Emil:Form:Test:callback 1)
                  :to-equal (list body-form env)))))))
