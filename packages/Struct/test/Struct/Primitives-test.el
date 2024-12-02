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

(require 'Struct/Primitives)
(require 'Struct)
(require 'buttercup)

(describe "Struct:Primitives"
  (before-each
    (eval '(Struct:define TestStruct
             (property-0) (property-1)))
    (eval '(Struct:define OtherTestStruct
             (property-0) (property-1))))
  
  (after-each
    (Struct:undefine 'TestStruct)
    (Struct:undefine 'OtherTestStruct))

  (describe "Struct:name"
    (it "returns the name of a struct value"
      (expect (Struct:name (TestStruct))
              :to-equal 'TestStruct))

    (it "does check if value is not a defined struct"
      (expect (Struct:name (list 'FakeTestStruct :property 0))
              :to-equal nil))

    (it "returns nil if value is not a struct"
      (expect (Struct:name 42)
              :to-equal nil)
      (expect (Struct:name (list :FakeTestStruct :property 0))
              :to-equal nil))))

