;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'Emil)

(describe "Emil"
  (describe "Emil:infer-form"
    (describe "basic values"
      (it "nil"
        (expect (Emil:infer-form nil)
                :to-equal 'Null))

      (it "t"
        (expect (Emil:infer-form t)
                :to-equal 'symbol))

      (it "keyword"
        (expect (Emil:infer-form :keyword)
                :to-equal 'symbol))

      (it "string"
        (expect (Emil:infer-form "string")
                :to-equal 'string))

      (it "integer"
        (expect (Emil:infer-form 0)
                :to-equal 'integer))

      (it "float"
        (expect (Emil:infer-form 0.0)
                :to-equal 'float))

      (it "vector"
        (expect (Emil:infer-form [])
                :to-equal 'vector)))

    (describe "lambda"
      (it "constant"
        (expect (Emil:infer-form '(lambda () 0))
                :to-equal '(-> () integer)))

      (it "identity"
        (expect (Emil:infer-form '(lambda (x) x))
                :to-equal '(-> ('a) 'a)))

      (it "two arguments"
        (expect (Emil:infer-form '(lambda (x y) y))
                :to-equal '(-> ('a 'b) 'b)))

      (it "convoluted identity"
        (expect (Emil:infer-form '(lambda (x) ((lambda (y) y) x)))
                :to-equal '(-> ('a) 'a))))

    (describe "let"
      (it "empty"
        (expect (Emil:infer-form '(let () 0))
                :to-equal 'integer))

      (it "single binding"
        (expect (Emil:infer-form '(let ((a 0)) a))
                :to-equal 'integer))

      (it "multiple bindings"
        (expect (Emil:infer-form '(let ((a 0) (b [])) b))
                :to-equal 'vector))
t
      (it "shadowed binding"
        (expect (Emil:infer-form '(let ((a 0))
                                    (let ((a [])
                                          (b a))
                                      b)))
                :to-equal 'integer))

      (it "lambda binding"
        (expect (Emil:infer-form '(let ((fn (lambda (a) 0)))
                                    fn))
                :to-equal '(-> ('a) integer))))

    (describe "let*"
      (it "empty"
        (expect (Emil:infer-form '(let* () 0))
                :to-equal 'integer))

      (it "single binding"
        (expect (Emil:infer-form '(let* ((a 0)) a))
                :to-equal 'integer))

      (it "multiple bindings"
        (expect (Emil:infer-form '(let* ((a 0) (b a)) b))
                :to-equal 'integer))

      (it "shadowed binding"
        (expect (Emil:infer-form '(let ((a 0))
                                    (let* ((a [])
                                           (b a))
                                      b)))
                :to-equal 'vector))

      (it "lambda binding"
        (expect (Emil:infer-form '(let* ((fn (lambda (a) 0)))
                                    fn))
                :to-equal '(-> ('a) integer))))

    (describe "application"
      (it "identity"
        (expect (Emil:infer-form '((lambda (x) x) 0))
                :to-equal 'integer))

      (it "convoluted identity"
        (expect (Emil:infer-form '((lambda (x) ((lambda (y) y) x))
                                   (lambda (z) z)))
                :to-equal '(-> ('a) 'a)))

      (it "two arguments"
        (expect (Emil:infer-form '((lambda (x y) y) [] 0))
                :to-equal 'integer)))))
