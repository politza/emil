;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'Emil)

(describe "Emil:transform"
  (describe "basic values"
    (it "nil"
      (expect (Emil:transform nil)
              :to-equal
              '(Emil:TypedForm
                :form nil
                :type (Emil:Type:Null)
                :environment
                (Emil:Env:Alist
                 :variables nil :functions nil :parent nil))))

    (it "t"
      (expect (Emil:transform t)
              :to-equal
              '(Emil:TypedForm
                :form t
                :type (Emil:Type:Basic :name symbol)
                :environment
                (Emil:Env:Alist
                 :variables nil :functions nil :parent nil))))

    (it "keyword"
      (expect (Emil:transform :keyword)
              :to-equal
              '(Emil:TypedForm
                :form :keyword
                :type (Emil:Type:Basic :name symbol)
                :environment
                (Emil:Env:Alist :variables nil :functions nil :parent nil))))

    (it "string"
      (expect (Emil:transform "string")
              :to-equal
              '(Emil:TypedForm
                :form "string"
                :type (Emil:Type:Basic :name string)
                :environment
                (Emil:Env:Alist :variables nil :functions nil :parent nil))))

    (it "integer"
      (expect (Emil:transform 0)
              :to-equal
              '(Emil:TypedForm
                :form 0
                :type (Emil:Type:Basic :name integer)
                :environment
                (Emil:Env:Alist :variables nil :functions nil :parent nil))))

    (it "float"
      (expect (Emil:transform 0.0)
              :to-equal
              '(Emil:TypedForm
                :form 0.0
                :type (Emil:Type:Basic :name float)
                :environment
                (Emil:Env:Alist :variables nil :functions nil :parent nil))))

    (it "vector"
      (expect (Emil:transform [])
              :to-equal
              '(Emil:TypedForm
                :form []
                :type
                (Emil:Type:Basic :name vector)
                :environment
                (Emil:Env:Alist :variables nil :functions nil :parent nil)))))

  (describe "lambda"
    (it "identity"
      (expect (Emil:transform '(lambda (x) x))
              :to-equal
              '(Emil:TypedForm
                :form
                #'(lambda
                    (x)
                    (Emil:TypedForm :form x
                                    :type (Emil:Type:Existential :name a)
                                    :environment
                                    (Emil:Env:Alist
                                     :variables
                                     ((x Emil:Type:Existential :name a))
                                     :functions nil :parent
                                     (Emil:Env:Alist :variables nil :functions nil :parent nil))))
                :type
                (Emil:Type:Arrow :arguments
                                 ((Emil:Type:Existential :name a))
                                 :rest? nil :returns
                                 (Emil:Type:Existential :name a)
                                 :min-arity 1)
                :environment
                (Emil:Env:Alist :variables nil :functions nil :parent nil)))))

  (describe "let"
    (it "basic"
      (expect (Emil:transform '(let ((a 0)) a))
              :to-equal
              '(Emil:TypedForm
                :form
                (let
                    ((a
                      (Emil:TypedForm
                       :form 0 :type
                       (Emil:Type:Basic :name integer)
                       :environment
                       (Emil:Env:Alist :variables nil :functions nil :parent nil))))
                  (Emil:TypedForm
                   :form a :type
                   (Emil:Type:Basic :name integer)
                   :environment
                   (Emil:Env:Alist :variables
                                   ((a Emil:Type:Basic :name integer))
                                   :functions nil :parent
                                   (Emil:Env:Alist :variables nil :functions nil :parent nil))))
                :type
                (Emil:Type:Basic :name integer)
                :environment
                (Emil:Env:Alist :variables nil :functions nil :parent nil)))))

  (describe "let*"
    (it "basic"
      (expect (Emil:transform '(let* ((a 0)) a))
              :to-equal
              '(Emil:TypedForm
                :form
                (let*
                    ((a
                      (Emil:TypedForm
                       :form 0 :type
                       (Emil:Type:Basic :name integer)
                       :environment
                       (Emil:Env:Alist
                        :variables nil :functions nil :parent
                        (Emil:Env:Alist :variables nil :functions nil :parent nil)))))
                  (Emil:TypedForm
                   :form a :type
                   (Emil:Type:Basic :name integer)
                   :environment
                   (Emil:Env:Alist :variables
                                   ((a Emil:Type:Basic :name integer))
                                   :functions nil :parent
                                   (Emil:Env:Alist
                                    :variables nil :functions nil :parent
                                    (Emil:Env:Alist :variables nil :functions nil :parent nil)))))
                :type
                (Emil:Type:Basic :name integer)
                :environment
                (Emil:Env:Alist :variables nil :functions nil :parent nil)))))

  (describe "application"
    (it "identity"
      (expect (Emil:transform '((lambda (x) x) 0))
              :to-equal
              '(Emil:TypedForm
                :form
                ((lambda
                   (x)
                   (Emil:TypedForm
                    :form x
                    :type
                    (Emil:Type:Existential :name a)
                    :environment
                    (Emil:Env:Alist :variables
                                    ((x Emil:Type:Existential :name a))
                                    :functions nil :parent
                                    (Emil:Env:Alist :variables nil :functions nil :parent nil))))
                 (Emil:TypedForm
                  :form 0
                  :type
                  (Emil:Type:Existential :name a)
                  :environment
                  (Emil:Env:Alist :variables nil :functions nil :parent nil)))
                :type
                (Emil:Type:Basic :name integer)
                :environment
                (Emil:Env:Alist :variables nil :functions nil :parent nil)))))

  (describe "and"
    (it "basic"
      (expect (Emil:transform '(and 0 1 2))
              :to-equal
              '(Emil:TypedForm
                :form
                (and
                 (Emil:TypedForm :form 0
                                 :type (Emil:Type:Basic :name integer)
                                 :environment
                                 (Emil:Env:Alist :variables nil :functions nil :parent nil))
                 (Emil:TypedForm :form 1
                                 :type (Emil:Type:Basic :name integer)
                                 :environment
                                 (Emil:Env:Alist :variables nil :functions nil :parent nil))
                 (Emil:TypedForm :form 2
                                 :type (Emil:Type:Basic :name integer)
                                 :environment
                                 (Emil:Env:Alist :variables nil :functions nil :parent nil)))
                :type
                (Emil:Type:Any)
                :environment
                (Emil:Env:Alist :variables nil :functions nil :parent nil)))))

  (describe "catch"
    (it "basic"
      (expect (Emil:transform '(catch 'tag 0 1 2))
              :to-equal
              '(Emil:TypedForm
                :form
                (catch
                    (Emil:TypedForm :form 'tag
                                    :type (Emil:Type:Any)
                                    :environment
                                    (Emil:Env:Alist :variables nil :functions nil :parent nil))
                  (Emil:TypedForm :form 0
                                  :type (Emil:Type:Basic :name integer)
                                  :environment
                                  (Emil:Env:Alist :variables nil :functions nil :parent nil))
                  (Emil:TypedForm :form 1
                                  :type (Emil:Type:Basic :name integer)
                                  :environment
                                  (Emil:Env:Alist :variables nil :functions nil :parent nil))
                  (Emil:TypedForm :form 2
                                  :type (Emil:Type:Basic :name integer)
                                  :environment
                                  (Emil:Env:Alist :variables nil :functions nil :parent nil)))
                :type
                (Emil:Type:Any)
                :environment
                (Emil:Env:Alist :variables nil :functions nil :parent nil)))))

  (describe "cond"
    (it "basic"
      (expect (Emil:transform '(cond (nil 0 1) (t 2 3)))
              :to-equal
              '(Emil:TypedForm
                :form
                (cond
                 ((Emil:TypedForm :form nil
                                  :type (Emil:Type:Null)
                                  :environment
                                  (Emil:Env:Alist :variables nil :functions nil :parent nil))
                  (Emil:TypedForm :form 0
                                  :type (Emil:Type:Basic :name integer)
                                  :environment
                                  (Emil:Env:Alist :variables nil :functions nil :parent nil))
                  (Emil:TypedForm :form 1
                                  :type (Emil:Type:Basic :name integer)
                                  :environment
                                  (Emil:Env:Alist :variables nil :functions nil :parent nil)))
                 ((Emil:TypedForm :form t
                                  :type (Emil:Type:Basic :name symbol)
                                  :environment
                                  (Emil:Env:Alist :variables nil :functions nil :parent nil))
                  (Emil:TypedForm :form 2
                                  :type (Emil:Type:Basic :name integer)
                                  :environment
                                  (Emil:Env:Alist :variables nil :functions nil :parent nil))
                  (Emil:TypedForm :form 3
                                  :type (Emil:Type:Basic :name integer)
                                  :environment
                                  (Emil:Env:Alist :variables nil :functions nil :parent nil))))
                :type
                (Emil:Type:Any)
                :environment
                (Emil:Env:Alist :variables nil :functions nil :parent nil)))))

  (describe "defconst"
    (it "basic"
      (expect (Emil:transform '(defconst a (prog1 nil t) "doc"))
              :to-equal
              '(Emil:TypedForm
                :form
                (defconst a
                  (Emil:TypedForm
                   :form
                   (prog1
                       (Emil:TypedForm :form nil
                                       :type (Emil:Type:Null)
                                       :environment
                                       (Emil:Env:Alist :variables nil :functions nil :parent nil))
                     (Emil:TypedForm :form t
                                     :type (Emil:Type:Basic :name symbol)
                                     :environment
                                     (Emil:Env:Alist :variables nil :functions nil :parent nil)))
                   :type
                   (Emil:Type:Null)
                   :environment
                   (Emil:Env:Alist :variables nil :functions nil :parent nil))
                  "doc")
                :type
                (Emil:Type:Basic :name symbol)
                :environment
                (Emil:Env:Alist :variables nil :functions nil :parent nil)))))

  (describe "defvar"
    (it "basic"
      (expect (Emil:transform '(defvar a (prog1 nil t) "doc"))
              :to-equal
              '(Emil:TypedForm
                :form
                (defvar a
                  (Emil:TypedForm
                   :form
                   (prog1
                       (Emil:TypedForm :form nil
                                       :type (Emil:Type:Null)
                                       :environment
                                       (Emil:Env:Alist :variables nil :functions nil :parent nil))
                     (Emil:TypedForm :form t
                                     :type (Emil:Type:Basic :name symbol)
                                     :environment
                                     (Emil:Env:Alist :variables nil :functions nil :parent nil)))
                   :type
                   (Emil:Type:Null)
                   :environment
                   (Emil:Env:Alist :variables nil :functions nil :parent nil))
                  "doc")
                :type
                (Emil:Type:Basic :name symbol)
                :environment
                (Emil:Env:Alist :variables nil :functions nil :parent nil)))))

  (describe "if"
    (it "basic"
      (expect (Emil:transform '(if 0 1 2 3))
              :to-equal
              '(Emil:TypedForm
                :form
                (if
                    (Emil:TypedForm :form 0
                                    :type (Emil:Type:Basic :name integer)
                                    :environment
                                    (Emil:Env:Alist :variables nil :functions nil :parent nil))
                    (Emil:TypedForm :form 1
                                    :type (Emil:Type:Basic :name integer)
                                    :environment
                                    (Emil:Env:Alist :variables nil :functions nil :parent nil))
                  (Emil:TypedForm :form 2
                                  :type (Emil:Type:Basic :name integer)
                                  :environment
                                  (Emil:Env:Alist :variables nil :functions nil :parent nil))
                  (Emil:TypedForm :form 3
                                  :type (Emil:Type:Basic :name integer)
                                  :environment
                                  (Emil:Env:Alist :variables nil :functions nil :parent nil)))
                :type
                (Emil:Type:Any)
                :environment
                (Emil:Env:Alist :variables nil :functions nil :parent nil)))))

  (describe "interactive"
    (it "basic"
      (expect (Emil:transform '(interactive "p"))
              :to-equal
              '(Emil:TypedForm
                :form
                (interactive "p")
                :type
                (Emil:Type:Any)
                :environment
                (Emil:Env:Alist :variables nil :functions nil :parent nil)))))

  (describe "or"
    (it "basic"
      (expect (Emil:transform '(or 0 1 2))
              :to-equal
              '(Emil:TypedForm
                :form
                (or
                 (Emil:TypedForm :form 0
                                 :type (Emil:Type:Basic :name integer)
                                 :environment
                                 (Emil:Env:Alist :variables nil :functions nil :parent nil))
                 (Emil:TypedForm :form 1
                                 :type (Emil:Type:Basic :name integer)
                                 :environment
                                 (Emil:Env:Alist :variables nil :functions nil :parent nil))
                 (Emil:TypedForm :form 2
                                 :type (Emil:Type:Basic :name integer)
                                 :environment
                                 (Emil:Env:Alist :variables nil :functions nil :parent nil)))
                :type
                (Emil:Type:Any)
                :environment
                (Emil:Env:Alist :variables nil :functions nil :parent nil)))))

  (describe "prog1"
    (it "basic"
      (expect (Emil:transform '(prog1 0 [] []))
              :to-equal
              '(Emil:TypedForm
                :form
                (prog1
                    (Emil:TypedForm :form 0
                                    :type (Emil:Type:Basic :name integer)
                                    :environment
                                    (Emil:Env:Alist :variables nil :functions nil :parent nil))
                  (Emil:TypedForm :form
                                  []
                                  :type
                                  (Emil:Type:Basic :name vector)
                                  :environment
                                  (Emil:Env:Alist :variables nil :functions nil :parent nil))
                  (Emil:TypedForm :form
                                  []
                                  :type
                                  (Emil:Type:Basic :name vector)
                                  :environment
                                  (Emil:Env:Alist :variables nil :functions nil :parent nil)))
                :type
                (Emil:Type:Basic :name integer)
                :environment
                (Emil:Env:Alist :variables nil :functions nil :parent nil)))))

  (describe "progn"
    (it "basic"
      (expect (Emil:transform '(progn 0 1 []))
              :to-equal
              '(Emil:TypedForm
                :form
                (progn
                  (Emil:TypedForm :form 0
                                  :type (Emil:Type:Basic :name integer)
                                  :environment
                                  (Emil:Env:Alist :variables nil :functions nil :parent nil))
                  (Emil:TypedForm :form 1
                                  :type (Emil:Type:Basic :name integer)
                                  :environment
                                  (Emil:Env:Alist :variables nil :functions nil :parent nil))
                  (Emil:TypedForm :form
                                  []
                                  :type
                                  (Emil:Type:Basic :name vector)
                                  :environment
                                  (Emil:Env:Alist :variables nil :functions nil :parent nil)))
                :type
                (Emil:Type:Basic :name vector)
                :environment
                (Emil:Env:Alist :variables nil :functions nil :parent nil)))))

  (describe "quote"
    (it "basic"
      (expect (Emil:transform '(quote x))
              :to-equal
              '(Emil:TypedForm
                :form 'x
                :type (Emil:Type:Any)
                :environment
                (Emil:Env:Alist :variables nil :functions nil :parent nil)))))

  (describe "save-current-buffer"
    (it "basic"
      (expect (Emil:transform '(save-current-buffer 0 1 []))
              :to-equal
              '(Emil:TypedForm
                :form
                (save-current-buffer
                  (Emil:TypedForm :form 0
                                  :type (Emil:Type:Basic :name integer)
                                  :environment
                                  (Emil:Env:Alist :variables nil :functions nil :parent nil))
                  (Emil:TypedForm :form 1
                                  :type (Emil:Type:Basic :name integer)
                                  :environment
                                  (Emil:Env:Alist :variables nil :functions nil :parent nil))
                  (Emil:TypedForm :form
                                  []
                                  :type
                                  (Emil:Type:Basic :name vector)
                                  :environment
                                  (Emil:Env:Alist :variables nil :functions nil :parent nil)))
                :type
                (Emil:Type:Basic :name vector)
                :environment
                (Emil:Env:Alist :variables nil :functions nil :parent nil)))))

  (describe "save-excursion"
    (it "basic"
      (expect (Emil:transform '(save-excursion 0 1 []))
              :to-equal
              '(Emil:TypedForm
                :form
                (save-excursion
                  (Emil:TypedForm :form 0
                                  :type (Emil:Type:Basic :name integer)
                                  :environment
                                  (Emil:Env:Alist :variables nil :functions nil :parent nil))
                  (Emil:TypedForm :form 1
                                  :type (Emil:Type:Basic :name integer)
                                  :environment
                                  (Emil:Env:Alist :variables nil :functions nil :parent nil))
                  (Emil:TypedForm :form
                                  []
                                  :type
                                  (Emil:Type:Basic :name vector)
                                  :environment
                                  (Emil:Env:Alist :variables nil :functions nil :parent nil)))
                :type
                (Emil:Type:Basic :name vector)
                :environment
                (Emil:Env:Alist :variables nil :functions nil :parent nil)))))

  (describe "save-restriction"
    (it "basic"
      (expect (Emil:transform '(save-restriction 0 1 []))
              :to-equal
              '(Emil:TypedForm
                :form
                (save-restriction
                  (Emil:TypedForm :form 0
                                  :type (Emil:Type:Basic :name integer)
                                  :environment
                                  (Emil:Env:Alist :variables nil :functions nil :parent nil))
                  (Emil:TypedForm :form 1
                                  :type (Emil:Type:Basic :name integer)
                                  :environment
                                  (Emil:Env:Alist :variables nil :functions nil :parent nil))
                  (Emil:TypedForm :form
                                  []
                                  :type
                                  (Emil:Type:Basic :name vector)
                                  :environment
                                  (Emil:Env:Alist :variables nil :functions nil :parent nil)))
                :type
                (Emil:Type:Basic :name vector)
                :environment
                (Emil:Env:Alist :variables nil :functions nil :parent nil)))))

  (describe "setq"
    (it "basic"
      (expect (Emil:transform '(setq a 0 b 1))
              :to-equal
              '(Emil:TypedForm
                :form
                (setq a 0 b 1)
                :type
                (Emil:Type:Any)
                :environment
                (Emil:Env:Alist :variables nil :functions nil :parent nil)))))

  (describe "unwind-protect"
    (it "basic"
      (expect (Emil:transform '(unwind-protect [] 1 2))
              :to-equal
              '(Emil:TypedForm
                :form
                (unwind-protect
                    (Emil:TypedForm :form
                                    []
                                    :type
                                    (Emil:Type:Basic :name vector)
                                    :environment
                                    (Emil:Env:Alist :variables nil :functions nil :parent nil))
                  (Emil:TypedForm :form 1
                                  :type (Emil:Type:Basic :name integer)
                                  :environment
                                  (Emil:Env:Alist :variables nil :functions nil :parent nil))
                  (Emil:TypedForm :form 2
                                  :type (Emil:Type:Basic :name integer)
                                  :environment
                                  (Emil:Env:Alist :variables nil :functions nil :parent nil)))
                :type
                (Emil:Type:Basic :name vector)
                :environment
                (Emil:Env:Alist :variables nil :functions nil :parent nil)))))

  (describe "while"
    (it "basic"
      (expect (Emil:transform '(while 0 1 []))
              :to-equal
              '(Emil:TypedForm
                :form
                (while
                    (Emil:TypedForm :form 0
                                    :type (Emil:Type:Basic :name integer)
                                    :environment
                                    (Emil:Env:Alist :variables nil :functions nil :parent nil))
                  (Emil:TypedForm :form 1
                                  :type (Emil:Type:Basic :name integer)
                                  :environment
                                  (Emil:Env:Alist :variables nil :functions nil :parent nil))
                  (Emil:TypedForm :form
                                  []
                                  :type
                                  (Emil:Type:Basic :name vector)
                                  :environment
                                  (Emil:Env:Alist :variables nil :functions nil :parent nil)))
                :type
                (Emil:Type:Basic :name vector)
                :environment
                (Emil:Env:Alist :variables nil :functions nil :parent nil))))))
