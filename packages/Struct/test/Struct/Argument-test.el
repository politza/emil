;; -*- lexical-binding: t -*-

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

    (it "name, type and default of struct-kind"
      (expect (Struct:Argument:read '(argument number (+ 1 2)) '&struct)
              :to-equal
              '(Struct:Argument
                :name argument
                :type number
                :default (+ 1 2)
                :kind &struct)))

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
