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
(require 'Struct)

;; Shutup "Optimization failure for cl-typep" warnings.
;; (put 'cl-typep 'compiler-macro nil)

(describe "Struct:Pcase"
  (before-each
    (eval '(Struct:define TestStruct
             (property-0) (property-1)))
    (eval '(Struct:define OtherTestStruct
             (property-0) (property-1))))
  
  (after-each
    (Struct:undefine 'TestStruct)
    (Struct:undefine 'OtherTestStruct))

  (it "can match a single property"
    (expect (eval '(pcase (TestStruct :property-0 0)
                     ((Struct TestStruct :property-0 result) result)))
            :to-equal 0))

  (it "can match a single property with implicit variables"
    (expect (eval '(pcase (TestStruct :property-0 0)
                     ((Struct TestStruct property-0) property-0)))
            :to-equal 0))

  (it "can match multiple properties"
    (expect (eval '(pcase (TestStruct :property-0 0 :property-1 1)
                     ((Struct TestStruct :property-1 second :property-0 first) (list first second))))
            :to-equal '(0 1)))
  
  (it "can match multiple properties with implicit variables"
    (expect (eval '(pcase (TestStruct :property-0 0 :property-1 1)
                     ((Struct TestStruct property-1 property-0) (list property-0 property-1))))
            :to-equal '(0 1)))

  (it "can mix variables with keyword/variables"
    (expect (eval '(pcase (TestStruct :property-0 0 :property-1 1)
                     ((Struct TestStruct property-1 :property-0 first) (list first property-1))))
            :to-equal '(0 1)))
  
  (it "can not match a non-struct"
    (expect (eval '(pcase (list :property-0 0)
                     ((Struct TestStruct property-0) property-0)))
            :to-equal nil))

  (it "can not match a struct of different type"
    (expect (eval '(pcase (OtherTestStruct :property-0 0)
                     ((Struct TestStruct property-0) property-0)))
            :to-equal nil))

  (it "throws an error for non-member properties"
    (expect (eval '(pcase (OtherTestStruct :property-0 0)
                     ((Struct TestStruct no-such-property))))
            :to-throw))

  (it "throws an error for invalid properties"
    (expect (eval '(pcase (OtherTestStruct :property-0 0)
                     ((Struct TestStruct (:property-0 property-0)))))
            :to-throw))

  (it "throws an error for missing variable after keywords"
    (expect (eval '(pcase (OtherTestStruct :property-0 0)
                     ((Struct TestStruct :property-0 property-0 :property-1))))
            :to-throw)))

