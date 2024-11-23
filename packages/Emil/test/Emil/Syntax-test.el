;; -*- lexical-binding: t -*-

(require 'Emil)

(describe "Struct tests"
  (describe "Emil:Syntax:Function:type"
    (it "basic"
      (expect (Emil:Type:print
               (Emil:Syntax:Function:type
                (Struct:Function:read '(fn a ((a number) -> string)))))
              :to-equal
              '(-> (number) string))))

  (describe "Emil:Syntax:Function:bindings"
    (it "basic"
      (expect (Emil:Syntax:Function:bindings
               (Struct:Function:read '(fn a ((a number) (b string)) -> string)))
              :to-equal
              '((a . (Emil:Type:Basic :name number))
                (b . (Emil:Type:Basic :name string)))))))
