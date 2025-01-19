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
              :to-equal '(-> () string))))

  (describe "Emil:Env:Global"
      (it "Emil:Env:declare-function"
        (Emil:Env:declare-function 'Emil:Env:Global:test-symbol '(-> () Void))
        (expect (Emil:Env:lookup-function
                 (Emil:Env:Global)
                 'Emil:Env:Global:test-symbol)
                :to-equal
                (Emil:Type:Arrow :arguments nil :rest? nil
                                 :returns (Emil:Type:Void) :min-arity 0))

        (Emil:Env:declare-function 'Emil:Env:Global:test-symbol nil)
        (expect (Emil:Env:lookup-function
                 (Emil:Env:Global)
                 'Emil:Env:Global:test-symbol)
                :to-equal
                nil))

      (it "Emil:Env:declare-alias"
        (Emil:Env:declare-function 'Emil:Env:Global:test-symbol '(-> () Void))
        (Emil:Env:declare-alias 'Emil:Env:Global:test-alias 'Emil:Env:Global:test-symbol)
        (expect (Emil:Env:lookup-function
                 (Emil:Env:Global)
                 'Emil:Env:Global:test-alias)
                :to-equal
                (Emil:Type:Arrow :arguments nil :rest? nil
                                 :returns (Emil:Type:Void) :min-arity 0))

        (Emil:Env:declare-function 'Emil:Env:Global:test-symbol nil)
        (expect (Emil:Env:declare-alias 'Emil:Env:Global:test-alias 'Emil:Env:Global:test-symbol)
                :to-throw 'Emil:error)
        (Emil:Env:declare-alias 'Emil:Env:Global:test-alias nil)
        (expect (Emil:Env:lookup-function
                 (Emil:Env:Global)
                 'Emil:Env:Global:test-alias)
                :to-equal nil))

      (it "Emil:Env:declare-variable"
        (Emil:Env:declare-variable 'Emil:Env:Global:test-symbol 'integer)
        (expect (Emil:Env:lookup-variable
                 (Emil:Env:Global)
                 'Emil:Env:Global:test-symbol)
                :to-equal
                (Emil:Type:Basic :name 'integer))

        (Emil:Env:declare-variable 'Emil:Env:Global:test-symbol nil)
        (expect (Emil:Env:lookup-variable
                 (Emil:Env:Global)
                 'Emil:Env:Global:test-symbol)
                :to-equal
                nil))))
