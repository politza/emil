;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'Struct/Function)

(describe "Function"
  (after-each
    (Struct:undefine 'TestStruct))

  (describe "read"
    (it "basic"
      (expect
       (Struct:Function:read
         '(fn add ((a number) &optional (b number 0) -> number)
              "Returns A plus B."
              (+ a b))
         'Test)
       :to-equal
       '(Struct:Function
         :name add
         :qualified-name Test:add
         :arguments
         ((Struct:Argument
           :name a
           :type number
           :default nil
           :kind nil)
          (Struct:Argument
           :name b
           :type number
           :default 0
           :kind &optional))
         :return-type number
         :documentation "Returns A plus B."
         :body ((+ a b))
         :filename nil)))

    (it "documentation, but no body"
      (expect
       (Struct:Function:read
         '(fn test () "documentation")
         'Test)
       :to-equal
       '(Struct:Function
         :name test
         :qualified-name Test:test
         :arguments nil
         :return-type nil
         :documentation "documentation"
         :body nil
         :filename nil)))

    (it "documentation and nil body"
      (expect
       (Struct:Function:read
         '(fn test () "documentation" nil)
         'Test)
       :to-equal
       '(Struct:Function
         :name test
         :qualified-name Test:test
         :arguments nil
         :return-type nil
         :documentation "documentation"
         :body (nil)
         :filename nil)))

    (it "no documentation and no body"
      (expect
       (Struct:Function:read
         '(fn test ())
         'Test)
       :to-equal
       '(Struct:Function
         :name test
         :qualified-name Test:test
         :arguments nil
         :return-type nil
         :documentation nil
         :body nil
         :filename nil)))

    (it "no documentation and nil body"
      (expect
       (Struct:Function:read
         '(fn test () nil)
         'Test)
       :to-equal
       '(Struct:Function
         :name test
         :qualified-name Test:test
         :arguments nil
         :return-type nil
         :documentation nil
         :body (nil)
         :filename nil)))

    (it "no documentation and double-nil body"
      (expect
       (Struct:Function:read
         '(fn test () nil nil)
         'Test)
       :to-equal
       '(Struct:Function
         :name test
         :qualified-name Test:test
         :arguments nil
         :return-type nil
         :documentation nil
         :body (nil nil)
         :filename nil)))

    (it "invalid keyword"
      (expect
       (Struct:Function:read
         '(defun test ())
         'Test)
       :to-throw))

    (it "invalid name"
      (expect
       (Struct:Function:read
         '(fn (test) ())
         'Test)
       :to-throw))

    (it "invalid arguments"
      (expect
       (Struct:Function:read
         '(fn test [])
         'Test)
       :to-throw))

    (it "-> used without argument"
      (expect
       (Struct:Function:read
         '(fn test (a ->))
         'Test)
       :to-throw))

    (it "-> in wrong position"
      (expect
       (Struct:Function:read
         '(fn test (a -> b c))
         'Test)
       :to-throw))
    
    (it "specifier used multiple times"
      (expect
       (Struct:Function:read
         '(fn test (&optional a &optional b))
         'Test)
       :to-throw))

    (it "specifier used without argument"
      (expect
       (Struct:Function:read
         '(fn test (a &optional))
         'Test)
       :to-throw))

    (it "&optional used after &rest"
      (expect
       (Struct:Function:read
         '(fn test (&rest a &optional b))
         'Test)
       :to-throw))

    (it "declare used"
      (expect
       (Struct:Function:read
         '(fn test () (declare (debug t)))
         'Test)
       :to-throw)))

  (describe "emit-arguments"
    (it "regular arguments"
      (expect (Struct:Function:emit-arguments
               (Struct:Function:read
                 '(fn test (a b c))
                 'Test))
              :to-equal
              '(a b c)))

    (it "optional arguments"
      (expect (Struct:Function:emit-arguments
               (Struct:Function:read
                 '(fn test (&optional a b c))
                 'Test))
              :to-equal
              '(&optional a b c)))

    (it "rest argument"
      (expect (Struct:Function:emit-arguments
               (Struct:Function:read
                 '(fn test (&rest a))
                 'Test))
              :to-equal
              '(&rest a)))
    
    (it "mixed arguments"
      (expect (Struct:Function:emit-arguments
               (Struct:Function:read
                 '(fn test (a &optional b &rest c))
                 'Test))
              :to-equal
              '(a &optional b &rest c)))

    (describe "Struct:Function:arity"
      (it "no arguments"
        (expect (Struct:Function:arity
                 (Struct:Function:read '(fn a ())))
                :to-equal '(0 . 0)))

      (it "single argument"
        (expect (Struct:Function:arity
                 (Struct:Function:read '(fn a (a))))
                :to-equal '(1 . 1)))

      (it "optional argument"
        (expect (Struct:Function:arity
                 (Struct:Function:read '(fn a (a &optional b))))
                :to-equal '(1 . 2)))

      (it "rest argument"
        (expect (Struct:Function:arity
                 (Struct:Function:read '(fn a (a &rest b))))
                :to-equal (cons 1 most-positive-fixnum))))

    (describe "Emil:Function:type"
      (it "no arguments"
        (expect (Struct:Function:type
                 (Struct:Function:read '(fn f ())))
                :to-equal '(-> () Any)))

      (it "single argument"
        (expect (Struct:Function:type
                 (Struct:Function:read '(fn f (a))))
                :to-equal '(-> (Any) Any)))

      (it "optional argument"
        (expect (Struct:Function:type
                 (Struct:Function:read '(fn f (a &optional b))))
                :to-equal '(-> (Any &optional Any) Any)))

      (it "rest argument"
        (expect (Struct:Function:type
                 (Struct:Function:read '(fn f (a &rest b))))
                :to-equal '(-> (Any &rest Any) Any)))

      (it "with provided types"
        (expect (Struct:Function:type
                 (Struct:Function:read '(fn f ((a number) &optional (b string) &rest (c symbol) -> integer))))
                :to-equal '(-> (number &optional string &rest symbol) integer))))))
