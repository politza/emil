;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'Struct/Function)

(describe "Function"
  (describe "read"
    (it "basic"
      (expect
       (Struct:Function:read 'Test
         '(fn add ((a number) &optional (b number 0) -> number)
              "Returns A plus B."
              (+ a b)))
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
         :body ((+ a b)))))

    (it "custom namespace-separator"
      (expect
       (Struct:Function:read 'emacs-convention
         '(fn test ()) '-)
       :to-equal
       '(Struct:Function
         :name test
         :qualified-name emacs-convention-test
         :arguments nil
         :return-type nil
         :documentation nil
         :body nil)))

    (it "documentation, but no body"
      (expect
       (Struct:Function:read 'Test
         '(fn test () "documentation"))
       :to-equal
       '(Struct:Function
         :name test
         :qualified-name Test:test
         :arguments nil
         :return-type nil
         :documentation "documentation"
         :body nil)))

    (it "documentation and nil body"
      (expect
       (Struct:Function:read 'Test
         '(fn test () "documentation" nil))
       :to-equal
       '(Struct:Function
         :name test
         :qualified-name Test:test
         :arguments nil
         :return-type nil
         :documentation "documentation"
         :body (nil))))

    (it "no documentation and no body"
      (expect
       (Struct:Function:read 'Test
         '(fn test ()))
       :to-equal
       '(Struct:Function
         :name test
         :qualified-name Test:test
         :arguments nil
         :return-type nil
         :documentation nil
         :body nil)))

    (it "no documentation and nil body"
      (expect
       (Struct:Function:read 'Test
         '(fn test () nil))
       :to-equal
       '(Struct:Function
         :name test
         :qualified-name Test:test
         :arguments nil
         :return-type nil
         :documentation nil
         :body (nil))))

    (it "no documentation and double-nil body"
      (expect
       (Struct:Function:read 'Test
         '(fn test () nil nil))
       :to-equal
       '(Struct:Function
         :name test
         :qualified-name Test:test
         :arguments nil
         :return-type nil
         :documentation nil
         :body (nil nil))))

    (it "invalid keyword"
      (expect
       (Struct:Function:read 'Test
         '(defun test ()))
       :to-throw))

    (it "invalid name"
      (expect
       (Struct:Function:read 'Test
         '(fn (test) ()))
       :to-throw))

    (it "invalid arguments"
      (expect
       (Struct:Function:read 'Test
         '(fn test []))
       :to-throw))

    (it "-> used without argument"
      (expect
       (Struct:Function:read 'Test
         '(fn test (a ->)))
       :to-throw))

    (it "-> in wrong position"
      (expect
       (Struct:Function:read 'Test
         '(fn test (a -> b c)))
       :to-throw))

    (it "&struct and &rest used together"
      (expect
       (Struct:Function:read 'Test
         '(fn test (&struct a &rest b)))
       :to-throw))

    (it "specifier used multiple times"
      (expect
       (Struct:Function:read 'Test
         '(fn test (&optional a &optional b)))
       :to-throw))

    (it "specifier used without argument"
      (expect
       (Struct:Function:read 'Test
         '(fn test (a &optional)))
       :to-throw))

    (it "&optional used after &rest"
      (expect
       (Struct:Function:read 'Test
         '(fn test (&rest a &optional b)))
       :to-throw)))

  (describe "lambda-arguments"
    (it "regular arguments"
      (expect (Struct:Function:lambda-arguments
               (Struct:Function:read 'Test
                 '(fn test (a b c))))
              :to-equal
              '(a b c)))

    (it "optional arguments"
      (expect (Struct:Function:lambda-arguments
               (Struct:Function:read 'Test
                 '(fn test (&optional a b c))))
              :to-equal
              '(&optional a b c)))

    (it "rest argument"
      (expect (Struct:Function:lambda-arguments
               (Struct:Function:read 'Test
                 '(fn test (&rest a))))
              :to-equal
              '(&rest a)))

    (it "struct argument"
      (expect (Struct:Function:lambda-arguments
               (Struct:Function:read 'Test
                 '(fn test (&struct a))))
              :to-equal
              '(&rest a)))

    (it "mixed arguments"
      (expect (Struct:Function:lambda-arguments
               (Struct:Function:read 'Test
                 '(fn test (a &optional b &rest c))))
              :to-equal
              '(a &optional b &rest c)))))
