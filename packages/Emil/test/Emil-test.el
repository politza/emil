;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'Emil)

(describe "Emil"
  (describe "Emil:infer-type"
    (describe "basic values"
      (it "nil"
        (expect (Emil:infer-type nil)
                :to-equal 'Null))

      (it "t"
        (expect (Emil:infer-type t)
                :to-equal 'symbol))

      (it "keyword"
        (expect (Emil:infer-type :keyword)
                :to-equal 'symbol))

      (it "string"
        (expect (Emil:infer-type "string")
                :to-equal 'string))

      (it "integer"
        (expect (Emil:infer-type 0)
                :to-equal 'integer))

      (it "float"
        (expect (Emil:infer-type 0.0)
                :to-equal 'float))

      (it "vector"
        (expect (Emil:infer-type [])
                :to-equal 'vector)))

    (describe "lambda"
      (it "constant"
        (expect (Emil:infer-type '(lambda () 0))
                :to-equal '(-> () integer)))

      (it "identity"
        (expect (Emil:infer-type '(lambda (x) x))
                :to-equal '(-> ('a?) 'a?)))

      (it "two arguments"
        (expect (Emil:infer-type '(lambda (x y) y))
                :to-equal '(-> ('a? 'b?) 'b?)))

      (it "convoluted identity"
        (expect (Emil:infer-type '(lambda (x) ((lambda (y) y) x)))
                :to-equal '(-> ('a?) 'a?))))

    (describe "let"
      (it "empty"
        (expect (Emil:infer-type '(let () 0))
                :to-equal 'integer))

      (it "single binding"
        (expect (Emil:infer-type '(let ((a 0)) a))
                :to-equal 'integer))

      (it "multiple bindings"
        (expect (Emil:infer-type '(let ((a 0) (b [])) b))
                :to-equal 'vector))
      t
      (it "shadowed binding"
        (expect (Emil:infer-type '(let ((a 0))
                                    (let ((a [])
                                          (b a))
                                      b)))
                :to-equal 'integer))

      (it "lambda binding"
        (expect (Emil:infer-type '(let ((f (lambda (a) 0)))
                                    f))
                :to-equal '(-> ('a?) integer))))

    (describe "let*"
      (it "empty"
        (expect (Emil:infer-type '(let* () 0))
                :to-equal 'integer))

      (it "single binding"
        (expect (Emil:infer-type '(let* ((a 0)) a))
                :to-equal 'integer))

      (it "multiple bindings"
        (expect (Emil:infer-type '(let* ((a 0) (b a)) b))
                :to-equal 'integer))

      (it "shadowed nested binding"
        (expect (Emil:infer-type '(let ((a 0))
                                    (let* ((a [])
                                           (b a))
                                      b)))
                :to-equal 'vector))

      (it "shadowed bindings"
        (expect (Emil:infer-type '(let* ((a 0)
                                         (a []))
                                    a))
                :to-equal 'vector))

      (it "lambda binding"
        (expect (Emil:infer-type '(let* ((f (lambda (a) 0)))
                                    f))
                :to-equal '(-> ('a?) integer))))

    (describe "application"
      (it "identity"
        (expect (Emil:infer-type '((lambda (x) x) 0))
                :to-equal 'integer))

      (it "convoluted identity"
        (expect (Emil:infer-type '((lambda (x) ((lambda (y) y) x))
                                   (lambda (z) z)))
                :to-equal '(-> ('a?) 'a?)))

      (it "two arguments"
        (expect (Emil:infer-type '((lambda (x y) y) [] 0))
                :to-equal 'integer)))

    (describe "and"
      (it "basic"
        (expect (Emil:infer-type '(and 0 1 2))
                :to-equal 'Any)))

    (describe "catch"
      (it "basic"
        (expect (Emil:infer-type '(catch 'tag 0 1 2))
                :to-equal 'Any)))

    (describe "cond"
      (it "basic"
        (expect (Emil:infer-type '(cond (nil 0 1) (t 2 3)))
                :to-equal 'Any)))

    (describe "defconst"
      (it "basic"
        (expect (Emil:infer-type '(defconst a (prog1 nil t) "doc"))
                :to-equal 'symbol)))

    (describe "defvar"
      (it "basic"
        (expect (Emil:infer-type '(defvar a (prog1 nil t) "doc"))
                :to-equal 'symbol)))

    (describe "if"
      (it "basic"
        (expect (Emil:infer-type '(if 0 1 2 3))
                :to-equal 'Any)))

    (describe "interactive"
      (it "basic"
        (expect (Emil:infer-type '(interactive "p"))
                :to-equal 'Any)))

    (describe "or"
      (it "basic"
        (expect (Emil:infer-type '(or 0 1 2))
                :to-equal 'Any)))

    (describe "prog1"
      (it "basic"
        (expect (Emil:infer-type '(prog1 0 [] []))
                :to-equal 'integer)))

    (describe "progn"
      (it "basic"
        (expect (Emil:infer-type '(progn 0 1 []))
                :to-equal 'vector)))

    (describe "quote"
      (it "basic"
        (expect (Emil:infer-type '(quote x))
                :to-equal 'Any)))

    (describe "save-current-buffer"
      (it "basic"
        (expect (Emil:infer-type '(save-current-buffer 0 1 []))
                :to-equal 'vector)))

    (describe "save-excursion"
      (it "basic"
        (expect (Emil:infer-type '(save-excursion 0 1 []))
                :to-equal 'vector)))

    (describe "save-restriction"
      (it "basic"
        (expect (Emil:infer-type '(save-restriction 0 1 []))
                :to-equal 'vector)))

    (describe "setq"
      (it "basic"
        (expect (Emil:infer-type '(setq a 0 b 1))
                :to-equal 'Any)))

    (describe "unwind-protect"
      (it "basic"
        (expect (Emil:infer-type '(unwind-protect [] 1 2))
                :to-equal 'vector)))

    (describe "while"
      (it "basic"
        (expect (Emil:infer-type '(while 0 1 []))
                :to-equal 'vector)))

    (describe "with environment"
      (it "basic variable"
        (expect (Emil:infer-type
                 'a
                 (Emil:Env:Alist:read '((a . integer)) nil))
                :to-equal 'integer))

      (it "basic function"
        (expect (Emil:infer-type
                 '#'f
                 (Emil:Env:Alist:read nil '((f . (-> (integer) string)))))
                :to-equal '(-> (integer) string)))

      (it "funcall let-bound function"
        (expect
         (Emil:infer-type
          '(let ((f (lambda (x) (length x))))
             (funcall f "string"))
          (Emil:Env:Alist:read
           nil
           '((funcall . (-> ((-> ('a) 'b) 'a) 'b))
             (length . (-> (string) integer)))))
         :to-equal 'integer))

      (it "funcall let*-bound function"
        (expect
         (Emil:infer-type
          '(let* ((f (lambda (x) (length x))))
             (funcall f "string"))
          (Emil:Env:Alist:read
           nil
           '((funcall . (-> ((-> ('a) 'b) 'a) 'b))
             (length . (-> (string) integer)))))
         :to-equal 'integer))

      (it "let shadows environment"
        (expect
         (Emil:infer-type
          '(let ((fill-column "string"))
             fill-column)
          (Emil:Env:Alist:read
           '((fill-column . integer))
           nil))
         :to-equal 'string))

      (it "let* shadows environment"
        (expect
         (Emil:infer-type
          '(let* ((fill-column "string")
                  (a (length fill-column)))
             a)
          (Emil:Env:Alist:read
           '((fill-column . integer))
           '((length . (-> (string) integer)))))
         :to-equal 'integer)))

    (describe "Emil:is"
      (it "basic type"
        (expect (Emil:infer-type '(Emil:is integer 0))
                :to-equal 'integer))

      (it "lambda"
        (expect (Emil:infer-type '(Emil:is (-> (string) string)
                                    (lambda (x) x)))
                :to-equal '(-> (string) string))))

    (describe "variable arguments"
      (describe "application"
        (it "&optional"
          (expect (Emil:infer-type '((lambda (a &optional b) a) 0))
                  :to-equal 'integer))

        (it "&optional / non provided"
          (expect (Emil:infer-type '((lambda (&optional a b) 0)))
                  :to-equal 'integer))

        (it "&rest"
          (expect (Emil:infer-type '((lambda (a &rest b) a) 0 "1" "2"))
                  :to-equal 'integer))

        (it "&rest / non provided"
          (expect (Emil:infer-type '((lambda (&rest a b) 0)))
                  :to-equal 'integer))

        (it "&optional and &rest"
          (expect (Emil:infer-type '((lambda (&optional a &rest b) a) 0))
                  :to-equal 'integer))

        (it "to many arguments"
          (expect (Emil:infer-type '((lambda (a &optional b) a) 0 1 2))
                  :to-throw))

        (it "to few arguments"
          (expect (Emil:infer-type '((lambda (a b &optional) a)))
                  :to-throw)))

      (describe "subtype"
        (it "&optional in source"
          (expect (Emil:infer-type
                   '(f (lambda (a &optional b) a))
                   (Emil:Env:Alist:read
                    nil
                    '((f . (-> ((-> ('a) 'b)) Null)))))
                  :to-equal 'Null))

        (it "&optional in target"
          (expect (Emil:infer-type
                   '(f (lambda (a &optional b) a))
                   (Emil:Env:Alist:read
                    nil
                    '((f . (-> ((-> ('a &optional 'a) 'b)) Null)))))
                  :to-equal 'Null))

        (it "to few arguments"
          (expect (Emil:infer-type
                   '(f (lambda () a))
                   (Emil:Env:Alist:read
                    nil
                    '((f . (-> ((-> ('a) 'b)) Null)))))
                  :to-throw))

        (it "&optional / to many"
          (expect (Emil:infer-type
                   '(f (lambda (a b &optional c) a))
                   (Emil:Env:Alist:read
                    nil
                    '((f . (-> ((-> ('a) 'b)) Null)))))
                  :to-throw))

        (it "&rest in source"
          (expect (Emil:infer-type
                   '(f (lambda (a &rest b) a))
                   (Emil:Env:Alist:read
                    nil
                    '((f . (-> ((-> ('a 'b 'c) 'd)) Null)))))
                  :to-equal 'Null))

        (it "&rest in target"
          (expect (Emil:infer-type
                   '(f (lambda (&rest a)))
                   (Emil:Env:Alist:read
                    nil
                    '((f . (-> ((-> ('a 'b &rest 'c) 'd)) Null)))))
                  :to-equal 'Null))

        (it "&rest / to many"
          (expect (Emil:infer-type
                   '(f (lambda (a b c d &rest e) a))
                   (Emil:Env:Alist:read
                    nil
                    '((f . (-> ((-> ('a 'b 'c) 'd)) Null)))))
                  :to-throw))

        (it "&optional and &rest in source"
          (expect (Emil:infer-type
                   '(f (lambda (a &optional b &rest c) a))
                   (Emil:Env:Alist:read
                    nil
                    '((f . (-> ((-> ('a 'b 'c 'd) 'e)) Null)))))
                  :to-equal 'Null))

        (it "&optional and &rest in target"
          (expect (Emil:infer-type
                   '(f (lambda (&optional a &rest b) a))
                   (Emil:Env:Alist:read
                    nil
                    '((f . (-> ((-> ('a &optional 'b &rest 'c) 'e)) Null)))))
                  :to-equal 'Null)))

      (describe "check"
        (it "&optional in source"
          (expect (Emil:infer-type '(Emil:is (-> ('a) 'a)
                                      (lambda (a &optional b) a)))
                  :to-equal '(-> ('a) 'a)))

        (it "&optional in target"
          (expect (Emil:infer-type '(Emil:is (-> (&optional 'a) 'a)
                                      (lambda (&optional a b) a)))
                  :to-equal '(-> (&optional 'a) 'a)))

        (it "&optional / to few"
          (expect (Emil:infer-type '(Emil:is (-> ('a 'b 'c) 'a)
                                      (lambda (a &optional b) a)))
                  :to-throw))

        (it "&optional / to many"
          (expect (Emil:infer-type '(Emil:is (-> () 'a)
                                      (lambda (a &optional b) a)))
                  :to-throw))

        (it "&rest in source"
          (expect (Emil:infer-type '(Emil:is (-> ('a 'b 'b) 'a)
                                      (lambda (a &rest b) a)))
                  :to-equal '(-> ('a 'b 'b) 'a)))

        (it "&rest in target"
          (expect (Emil:infer-type '(Emil:is (-> ('a &rest 'b) 'a)
                                      (lambda (a &rest b) a)))
                  :to-equal '(-> ('a &rest 'b) 'a)))))

    (describe "generalizing variables"
      (it "without annotation and symbol function"
        (expect (Emil:infer-type
                 '(list 0.0 0)
                 (Emil:Env:Alist:read
                  nil
                  '((list . (-> ('a 'a) 'a)))))
                :to-throw 'Emil:type-error))

      (it "with annotation and symbol function"
        (expect (Emil:infer-type
                 '(list (Emil:is number 0.0) 0)
                 (Emil:Env:Alist:read
                  nil
                  '((list . (-> ('a 'a) 'a)))))
                :to-equal 'number)))

    (describe "compound types"
      (it "can be inferred"
        (expect (Emil:infer-type
                 'list
                 (Emil:Env:Alist:read '((list . (List string))) nil))
                :to-equal '(List string)))

      (it "can be checked"
        (expect (Emil:infer-type
                 '(map (lambda (a) 0) list)
                 (Emil:Env:Alist:read
                  '((list . (List string)))
                  '((map . (-> ((-> ('a) 'b) (List 'a)) (List 'b))))))
                :to-equal '(List integer)))

      (it "are covariant"
        (expect (Emil:infer-type
                 '(sum list)
                 (Emil:Env:Alist:read
                  '((list . (List float)))
                  '((sum . (-> ((List number)) number)))))
                :to-equal 'number))

      (it "are rejected if different constructor"
        (expect (Emil:infer-type
                 '(sum list)
                 (Emil:Env:Alist:read
                  '((list . (Sequence number)))
                  '((sum . (-> ((List number)) number)))))
                :to-throw 'Emil:type-error))

      (it "are rejected if different parameter count"
        (expect (Emil:infer-type
                 '(sum list)
                 (Emil:Env:Alist:read
                  '((list . (List number string)))
                  '((sum . (-> ((List number)) number)))))
                :to-throw 'Emil:type-error)))

    (xdescribe "existing issues"
      (xit "not all applied arguments are checked for &rest functions"
        (expect (Emil:infer-type
                 '((lambda (&rest x) 0) 0.0 0))
                :to-throw 'Emil:type-error))

      (it "&rest variable should have list type in function"
        (expect (Emil:infer-type
                 '((lambda (&rest x) x) 0 1 2))
                :to-equal '(List integer))))))
