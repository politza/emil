;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'Emil)

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
              :to-equal 'vector))

    (it "record"
      (expect (Emil:infer-type (record 'test-record 0 1 2))
              :to-equal 'test-record)))

  (describe "lambda"
    (it "constant"
      (expect (Emil:infer-type '(lambda () 0))
              :to-equal '(-> () integer)))

    (it "identity"
      (expect (Emil:infer-type '(lambda (x) x))
              :to-equal '(-> ('a) 'a)))

    (it "two arguments"
      (expect (Emil:infer-type '(lambda (x y) y))
              :to-equal '(-> ('a 'b) 'b)))

    (it "convoluted identity"
      (expect (Emil:infer-type '(lambda (x) ((lambda (y) y) x)))
              :to-equal '(-> ('a) 'a)))

    (it "&rest arguments"
      (expect (Emil:infer-type '(lambda (&rest x) x))
              :to-equal '(-> (&rest 'a) (List 'a)))))

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
              :to-equal '(-> ('a) integer))))

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
              :to-equal '(-> ('a) integer))))

  (describe "application"
    (it "identity"
      (expect (Emil:infer-type '((lambda (x) x) 0))
              :to-equal 'integer))

    (it "convoluted identity"
      (expect (Emil:infer-type '((lambda (x) ((lambda (y) y) x))
                                 (lambda (z) z)))
              :to-equal '(-> ('a) 'a)))

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
              :to-equal 'Null)))

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
       :to-equal 'integer))

    (it "check with &rest arg and lambda"
      (expect
       (Emil:infer-type
        '(f (lambda (&rest x) x))
        (Emil:Env:Alist:read
         nil
         '((f . (-> ((-> ('a) (List 'a))) (List 'a))))))
       :to-equal '(List 'a)))

    (it "check with &rest arg against existing type"
      (expect
       (Emil:infer-type
        '(f g)
        (Emil:Env:Alist:read
         '((g . (-> (&rest 'a) (List 'a))))
         '((f . (-> ((-> ('a) (List 'a))) (List 'a))))))
       :to-equal '(List 'a))))

  (describe "annotations"
    (it "basic type"
      (expect (Emil:infer-type '(Emil:is 0 integer))
              :to-equal 'integer))

    (it "lambda"
      (expect (Emil:infer-type '(Emil:is (lambda (x) x)
                                  (-> (string) string)))
              :to-equal '(-> (string) string)))

    (it "required annotation not provided"
      (expect (Emil:infer-type
               '(list 0.0 0)
               (Emil:Env:Alist:read
                nil
                '((list . (-> ('a 'a) 'a)))))
              :to-throw 'Emil:type-error))

    (it "required annotation provided"
      (expect (Emil:infer-type
               '(list (Emil:is 0.0 number) 0)
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
                '((sum . (-> ((List2 number)) number)))))
              :to-throw 'Emil:type-error))

    (it "are rejected if different parameter count"
      (expect (Emil:infer-type
               '(sum list)
               (Emil:Env:Alist:read
                '((list . (List2 number string)))
                '((sum . (-> ((List2 number)) number)))))
              :to-throw 'Emil:type-error))

    (describe "Sequences"
      (it "cons into list"
        (expect (Emil:infer-type
                 '(let ((pair (cons 0 1)))
                    (list (car pair) (cdr pair)))
                 (Emil:Env:Alist:read
                  nil
                  '((cons . (-> ('a 'b) (Cons 'a 'b)))
                    (car . (-> ((Cons 'a 'b)) 'a))
                    (cdr . (-> ((Cons 'a 'b)) 'b))
                    (list . (-> ('a 'a) (List 'a))))))
                :to-equal '(List integer)))

      (it "cons into sequence"
        (expect (Emil:infer-type
                 '(let ((sequence (cons 0 (list 1 2))))
                    (elt sequence 0))
                 (Emil:Env:Alist:read
                  nil
                  '((cons . (-> ('a 'b) (Cons 'a 'b)))
                    (elt . (-> ((Sequence 'a) integer) 'a))
                    (list . (-> ('a 'a) (List 'a))))))
                :to-equal 'integer))

      (it "cons as list"
        (expect (Emil:infer-type
                 '(let ((list (cons 0 (list 1 2))))
                    (length list))
                 (Emil:Env:Alist:read
                  nil
                  '((cons . (-> ('a 'b) (Cons 'a 'b)))
                    (car . (-> ((Cons 'a 'b)) 'a))
                    (cdr . (-> ((Cons 'a 'b)) 'b))
                    (list . (-> ('a 'a) (List 'a)))
                    (length . (-> ((List 'a)) integer)))))
                :to-equal 'integer))

      (it "list as and into cons"
        (expect (Emil:infer-type
                 '(let ((list (list 1 2)))
                    (cons (car list) (cdr list)))
                 (Emil:Env:Alist:read
                  nil
                  '((cons . (-> ('a 'b) (Cons 'a 'b)))
                    (car . (-> ((Cons 'a 'b)) 'a))
                    (cdr . (-> ((Cons 'a 'b)) 'b))
                    (list . (-> ('a 'a) (List 'a))))))
                :to-equal '(Cons integer (List integer))))

      (it "strings as sequences"
        (expect (Emil:infer-type
                 '(string-prefix-p "foo" (concat "foo" "bar"))
                 (Emil:Env:Alist:read
                  nil
                  '((concat . (-> ((Sequence integer)
                                   (Sequence integer))
                                  string))
                    (string-prefix-p . (-> (string string) boolean)))))
                :to-equal 'boolean))))

  (describe "Emil:Analyzer:lambda-bindings"
    (describe "no optional and no rest"
      (it "equal argument count"
        (expect (Emil:Analyzer:lambda-bindings
                 (Emil:Type:read '(-> (string number integer) Any))
                 '(lambda (a b c)))
                :to-equal '((a . (Emil:Type:Basic :name string))
                            (b . (Emil:Type:Basic :name number))
                            (c . (Emil:Type:Basic :name integer)))))
      (it "to few arguments"
        (expect (Emil:Analyzer:lambda-bindings
                 (Emil:Type:read '(-> (string number integer) Any))
                 '(lambda (a b)))
                :to-throw 'Emil:type-error))

      (it "to many arguments"
        (expect (Emil:Analyzer:lambda-bindings
                 (Emil:Type:read '(-> (string number integer) Any))
                 '(lambda (a b c d)))
                :to-throw 'Emil:type-error)))

    (describe "optional and no rest"
      (it "equal optional arguments"
        (expect (Emil:Analyzer:lambda-bindings
                 (Emil:Type:read '(-> (string &optional number integer) Any))
                 '(lambda (a &optional b c)))
                :to-equal '((a . (Emil:Type:Basic :name string))
                            (b . (Emil:Type:Basic :name number))
                            (c . (Emil:Type:Basic :name integer)))))
      (it "fewer optional arguments"
        (expect (Emil:Analyzer:lambda-bindings
                 (Emil:Type:read '(-> (string &optional number integer) Any))
                 '(lambda (&optional a b c)))
                :to-equal '((a . (Emil:Type:Basic :name string))
                            (b . (Emil:Type:Basic :name number))
                            (c . (Emil:Type:Basic :name integer)))))

      (it "more optional arguments"
        (expect (Emil:Analyzer:lambda-bindings
                 (Emil:Type:read '(-> (string &optional number integer) Any))
                 '(lambda (a b &optional c)))
                :to-throw 'Emil:type-error))

      (it "additional optional arguments"
        (expect (Emil:Analyzer:lambda-bindings
                 (Emil:Type:read '(-> (string &optional number integer) Any))
                 '(lambda (a &optional b c d)))
                :to-equal '((a . (Emil:Type:Basic :name string))
                            (b . (Emil:Type:Basic :name number))
                            (c . (Emil:Type:Basic :name integer))
                            (d . (Emil:Type:Null))))))

    (describe "no rest in type"
      (it "fewer positional arguments"
        (expect (Emil:Analyzer:lambda-bindings
                 (Emil:Type:read '(-> (string &optional number integer) Any))
                 '(lambda (&optional a &rest b)))
                :to-equal '((a . (Emil:Type:Basic :name string))
                            (b . (Emil:Type:Compound
                                  :name List
                                  :arguments ((Emil:Type:Any)))))))
      (it "equal positional arguments"
        (expect (Emil:Analyzer:lambda-bindings
                 (Emil:Type:read '(-> (string &optional number integer) Any))
                 '(lambda (&optional a b &rest c)))
                :to-equal '((a . (Emil:Type:Basic :name string))
                            (b . (Emil:Type:Basic :name number))
                            (c . (Emil:Type:Compound
                                  :name List
                                  :arguments ((Emil:Type:Basic :name integer)))))))

      (it "more positional arguments"
        (expect (Emil:Analyzer:lambda-bindings
                 (Emil:Type:read '(-> (string &optional number) Any))
                 '(lambda (&optional a b c &rest d)))
                :to-equal '((a . (Emil:Type:Basic :name string))
                            (b . (Emil:Type:Basic :name number))
                            (c . (Emil:Type:Null))
                            (d . (Emil:Type:Compound
                                  :name List
                                  :arguments ((Emil:Type:Null))))))))

    (describe "rest in type"
      (it "fewer positional arguments"
        (expect (Emil:Analyzer:lambda-bindings
                 (Emil:Type:read '(-> (string number &rest integer) Any))
                 '(lambda (a &rest b)))
                :to-equal '((a . (Emil:Type:Basic :name string))
                            (b . (Emil:Type:Compound
                                  :name List
                                  :arguments ((Emil:Type:Any)))))))
      (it "equal positional arguments"
        (expect (Emil:Analyzer:lambda-bindings
                 (Emil:Type:read '(-> (string number &rest integer) Any))
                 '(lambda (a b &rest c)))
                :to-equal '((a . (Emil:Type:Basic :name string))
                            (b . (Emil:Type:Basic :name number))
                            (c . (Emil:Type:Compound
                                  :name List
                                  :arguments ((Emil:Type:Basic :name integer)))))))

      (it "more positional arguments"
        (expect (Emil:Analyzer:lambda-bindings
                 (Emil:Type:read '(-> (string number &rest integer) Any))
                 '(lambda (a &optional b c &rest d)))
                :to-equal '((a . (Emil:Type:Basic :name string))
                            (b . (Emil:Type:Basic :name number))
                            (c . (Emil:Type:Basic :name integer))
                            (d . (Emil:Type:Compound
                                  :name List
                                  :arguments ((Emil:Type:Basic :name integer)))))))))

  (describe "Emil:Analyzer:subtype-arrow-pairs"
    (describe "no rest"
      (it "equal argument count"
        (expect (Emil:Analyzer:subtype-arrow-pairs
                 (Emil:Type:read '(-> (a &optional b) Any))
                 (Emil:Type:read '(-> (c &optional d) Any)))
                :to-equal
                '(((Emil:Type:Basic :name a) . (Emil:Type:Basic :name c))
                  ((Emil:Type:Basic :name b) . (Emil:Type:Basic :name d)))))

      (it "fewer arguments"
        (expect (Emil:Analyzer:subtype-arrow-pairs
                 (Emil:Type:read '(-> (a) Any))
                 (Emil:Type:read '(-> (c &optional d) Any)))
                :to-throw 'Emil:type-error))

      (it "more arguments"
        (expect (Emil:Analyzer:subtype-arrow-pairs
                 (Emil:Type:read '(-> (a &optional b e) Any))
                 (Emil:Type:read '(-> (c &optional d) Any)))
                :to-equal '(((Emil:Type:Basic :name a) . (Emil:Type:Basic :name c))
                            ((Emil:Type:Basic :name b) . (Emil:Type:Basic :name d))))))

    (describe "left rest only"
      (it "equal positional argument count"
        (expect (Emil:Analyzer:subtype-arrow-pairs
                 (Emil:Type:read '(-> (a &rest b) Any))
                 (Emil:Type:read '(-> (c &optional d) Any)))
                :to-equal
                '(((Emil:Type:Basic :name a) . (Emil:Type:Basic :name c))
                  ((Emil:Type:Basic :name b) . (Emil:Type:Basic :name d)))))

      (it "fewer arguments"
        (expect (Emil:Analyzer:subtype-arrow-pairs
                 (Emil:Type:read '(-> (&rest a) Any))
                 (Emil:Type:read '(-> (c &optional d) Any)))
                :to-equal
                '(((Emil:Type:Basic :name a) . (Emil:Type:Basic :name c))
                  ((Emil:Type:Basic :name a) . (Emil:Type:Basic :name d)))))

      (it "more arguments"
        (expect (Emil:Analyzer:subtype-arrow-pairs
                 (Emil:Type:read '(-> (a &optional b &rest e) Any))
                 (Emil:Type:read '(-> (c &optional d) Any)))
                :to-equal
                '(((Emil:Type:Basic :name a) . (Emil:Type:Basic :name c))
                  ((Emil:Type:Basic :name b) . (Emil:Type:Basic :name d))))))

    (describe "left and right rest"
      (it "equal positional argument count"
        (expect (Emil:Analyzer:subtype-arrow-pairs
                 (Emil:Type:read '(-> (a &rest b) Any))
                 (Emil:Type:read '(-> (c &rest d) Any)))
                :to-equal
                '(((Emil:Type:Basic :name a) . (Emil:Type:Basic :name c))
                  ((Emil:Type:Basic :name b) . (Emil:Type:Basic :name d)))))

      (it "fewer arguments"
        (expect (Emil:Analyzer:subtype-arrow-pairs
                 (Emil:Type:read '(-> (&rest a) Any))
                 (Emil:Type:read '(-> (c &rest d) Any)))
                :to-equal
                '(((Emil:Type:Basic :name a) . (Emil:Type:Basic :name c))
                  ((Emil:Type:Basic :name a) . (Emil:Type:Basic :name d)))))

      (it "more arguments"
        (expect (Emil:Analyzer:subtype-arrow-pairs
                 (Emil:Type:read '(-> (a &optional b &rest e) Any))
                 (Emil:Type:read '(-> (c &rest d) Any)))
                :to-equal
                '(((Emil:Type:Basic :name a) . (Emil:Type:Basic :name c))
                  ((Emil:Type:Basic :name b) . (Emil:Type:Basic :name d))
                  ((Emil:Type:Basic :name e) . (Emil:Type:Basic :name d)))))))

  (describe "Emil:infer-application-arrrow-pairs"
    (it "equal arguments and types"
      (expect (Emil:infer-application-arrrow-pairs
               (Emil:Type:Arrow
                :arguments (list (Emil:Type:Existential :name 'a)
                                 (Emil:Type:Existential :name 'b))
                :returns (Emil:Type:Existential :name 'c)
                :min-arity 2)
               (list 0 1))
              :to-equal
              '(((Emil:Type:Existential :name a) . 0)
                ((Emil:Type:Existential :name b) . 1))))

    (it "fewer argument"
      (expect (Emil:infer-application-arrrow-pairs
               (Emil:Type:Arrow
                :arguments (list (Emil:Type:Existential :name 'a)
                                 (Emil:Type:Existential :name 'b))
                :returns (Emil:Type:Existential :name 'c)
                :rest? t
                :min-arity 1)
               (list 0))
              :to-equal
              '(((Emil:Type:Existential :name a) . 0))))

    (it "fewer types"
      (expect (Emil:infer-application-arrrow-pairs
               (Emil:Type:Arrow
                :arguments (list (Emil:Type:Existential :name 'a))
                :returns (Emil:Type:Existential :name 'b)
                :rest? t
                :min-arity 0)
               (list 0 1))
              :to-equal
              '(((Emil:Type:Existential :name a) . 0)
                ((Emil:Type:Existential :name a) . 1)))))

  (describe "Structs and Traits"
    (before-each
      (eval '(progn
               (Struct:define SomeStruct)
               (Struct:define OtherStruct)
               (Trait:define SuperTrait ())
               (Trait:define SomeTrait (SuperTrait))
               (Trait:implement SuperTrait SomeStruct)
               (Trait:implement SomeTrait SomeStruct))))

    (after-each
      (Struct:undefine 'SomeStruct)
      (Struct:undefine 'OtherStruct)
      (Trait:undefine 'SuperTrait)
      (Trait:undefine 'SomeTrait))

    (it "can assign a struct to itself"
      (expect (Emil:infer-type
               '(f a)
               (Emil:Env:Alist:read
                '((a . SomeStruct))
                '((f . (-> (SomeStruct) SomeStruct)))))
              :to-equal 'SomeStruct))

    (it "can not assign a struct to a different one"
      (expect (Emil:infer-type
               '(f a)
               (Emil:Env:Alist:read
                '((a . OtherStruct))
                '((f . (-> (SomeStruct) SomeStruct)))))
              :to-throw 'Emil:type-error))

    (it "can assign a trait to itself"
      (expect (Emil:infer-type
               '(f a)
               (Emil:Env:Alist:read
                '((a . (Trait SomeTrait)))
                '((f . (-> ((Trait SomeTrait)) (Trait SomeTrait))))))
              :to-equal '(Trait SomeTrait)))

    (it "can assign a sub-trait to a super-trait"
      (expect (Emil:infer-type
               '(f a)
               (Emil:Env:Alist:read
                '((a . (Trait SomeTrait)))
                '((f . (-> ((Trait SuperTrait)) (Trait SuperTrait))))))
              :to-equal '(Trait SuperTrait)))

    (it "can not assign a super-trait to a sub-trait"
      (expect (Emil:infer-type
               '(f a)
               (Emil:Env:Alist:read
                '((a . (Trait SuperTrait)))
                '((f . (-> ((Trait SomeTrait)) (Trait SomeTrait))))))
              :to-throw 'Emil:type-error))

    (it "can assign a struct to an implemented trait"
      (expect (Emil:infer-type
               '(f a)
               (Emil:Env:Alist:read
                '((a . SomeStruct))
                '((f . (-> ((Trait SomeTrait)) (Trait SomeTrait))))))
              :to-equal '(Trait SomeTrait)))

    (it "can not assign a struct to an unimplemented trait"
      (expect (Emil:infer-type
               '(f a)
               (Emil:Env:Alist:read
                '((a . OtherStruct))
                '((f . (-> ((Trait SomeTrait)) (Trait SomeTrait))))))
              :to-throw 'Emil:type-error))))
