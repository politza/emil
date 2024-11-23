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
(require 'Struct/Argument)

(describe "Struct:Argument"
  (describe "read"
    (it "symbol only"
      (expect (Struct:Argument:read 'argument)
              :to-equal
              '(Struct:Argument
                :name argument
                :type nil
                :default nil
                :kind nil)))

    (it "name only"
      (expect (Struct:Argument:read '(argument))
              :to-equal
              '(Struct:Argument
                :name argument
                :type nil
                :default nil
                :kind nil)))

    (it "name and type"
      (expect (Struct:Argument:read '(argument number))
              :to-equal
              '(Struct:Argument
                :name argument
                :type number
                :default nil
                :kind nil)))

    (it "name, type and default"
      (expect (Struct:Argument:read '(argument number (+ 1 2)))
              :to-equal
              '(Struct:Argument
                :name argument
                :type number
                :default (+ 1 2)
                :kind nil)))

    (it "name, type and default of rest-kind"
      (expect (Struct:Argument:read '(argument number (+ 1 2)) '&rest)
              :to-equal
              '(Struct:Argument
                :name argument
                :type number
                :default (+ 1 2)
                :kind &rest)))
    
    (it "name, type and default of optional-kind"
      (expect (Struct:Argument:read '(argument number (+ 1 2)) '&optional)
              :to-equal
              '(Struct:Argument
                :name argument
                :type number
                :default (+ 1 2)
                :kind &optional)))

    (it "not a cons"
      (expect (Struct:Argument:read [])
              :to-throw))

    (it "to many arguments"
      (expect (Struct:Argument:read '(argument number 0 1))
              :to-throw))

    (it "name, type and default of invalid kind"
      (expect (Struct:Argument:read '(argument number 0) '&plist)
              :to-throw))))
