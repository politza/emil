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
(require 'Emil)

(describe "Emil:infer-type functions"
  (describe "inferred lambda"
    :var ((env (Emil:Env:Alist:read
                nil
                '((result . (-> ('a 'b 'c) (Result 'a 'b 'c)))))))

    (it "empty"
      (expect (Emil:infer-type '(lambda ()))
              :to-equal '(-> () Null)))

    (it "fixed arguments"
      (expect (Emil:infer-type
               '(lambda (a b c) (result a b c))
               env)
              :to-equal '(-> ('a 'b 'c) (Result 'a 'b 'c))))

    (it "optional arguments"
      (expect (Emil:infer-type
               '(lambda (a &optional b c) (result a b c))
               env)
              :to-equal '(-> ('a &optional 'b 'c)
                             (Result 'a 'b 'c))))

    (it "rest argument"
      (expect (Emil:infer-type
               '(lambda (&rest a) a)
               env)
              :to-equal '(-> (&rest 'a) (List 'a))))

    (it "optional and rest argument"
      (expect (Emil:infer-type
               '(lambda (a &optional b &rest c) (result a b c))
               env)
              :to-equal '(-> ('a &optional 'b &rest 'c)
                             (Result 'a 'b (List 'c))))))

  (describe "inferred assignment"
    (describe "empty with"
      :var ((env (Emil:Env:Alist:read
                  nil
                  '((f . (-> ((-> () 'a)) 'a))
                    (result . (-> ('a 'b 'c) (Result 'a 'b 'c)))))))

      (it "empty"
        (expect (Emil:infer-type
                 '(f (lambda () 0)) env)
                :to-equal 'integer))

      (it "fixed arguments"
        (expect (Emil:infer-type
                 '(f (lambda (a b c) a))
                 env)
                :to-throw 'Emil:type-error))

      (it "optional arguments"
        (expect (Emil:infer-type
                 '(f (lambda (a &optional b c) (result a b c)))
                 env)
                :to-throw 'Emil:type-error))

      (it "rest argument"
        (expect (Emil:infer-type
                 '(f (lambda (&rest a) a))
                 env)
                :to-equal '(List Null)))

      (it "optional and rest argument"
        (expect (Emil:infer-type
                 '(f (lambda (a &optional b &rest c) (result a b c)))
                 env)
                :to-throw 'Emil:type-error)))

    (describe "fixed arguments with"
      :var ((env (Emil:Env:Alist:read
                  nil
                  '((f . (-> ((-> ('a 'b 'c) 'd)) 'd))
                    (result . (-> ('a 'b 'c) (Result 'a 'b 'c)))))))

      (it "empty"
        (expect (Emil:infer-type '(f (lambda () 0)) env)
                :to-throw 'Emil:type-error))

      (it "fixed arguments"
        (expect (Emil:infer-type
                 '(f (lambda (a b c) (result a b c)))
                 env)
                :to-equal '(Result 'a 'b 'c)))

      (it "optional arguments"
        (expect (Emil:infer-type
                 '(f (lambda (a &optional b c) (result a b c)))
                 env)
                :to-equal '(Result 'a 'b 'c)))

      (it "rest argument"
        (expect (Emil:infer-type
                 '(f (lambda (&rest a) a))
                 env)
                :to-equal '(List Any)))

      (it "optional and rest argument"
        (expect (Emil:infer-type
                 '(f (lambda (a &optional b &rest c) (result a b c)))
                 env)
                :to-equal '(Result 'a 'b (List 'c)))))

    (describe "optional arguments with"
      :var ((env (Emil:Env:Alist:read
                  nil
                  '((f . (-> ((-> ('a &optional 'b 'c) 'd)) 'd))
                    (result . (-> ('a 'b 'c) (Result 'a 'b 'c)))))))

      (it "empty"
        (expect (Emil:infer-type '(f (lambda () 0)) env)
                :to-throw 'Emil:type-error))

      (it "fixed arguments"
        (expect (Emil:infer-type
                 '(f (lambda (a b c) (result a b c)))
                 env)
                :to-throw 'Emil:type-error))

      (it "optional arguments"
        (expect (Emil:infer-type
                 '(f (lambda (a &optional b c) (result a b c)))
                 env)
                :to-equal '(Result 'a 'b 'c)))

      (it "rest argument"
        (expect (Emil:infer-type
                 '(f (lambda (&rest a) a))
                 env)
                :to-equal '(List Any)))

      (it "optional and rest argument"
        (expect (Emil:infer-type
                 '(f (lambda (a &optional b &rest c) (result a b c)))
                 env)
                :to-equal '(Result 'a 'b (List 'c)))))

    (describe "rest argument with"
      :var ((env (Emil:Env:Alist:read
                  nil
                  '((f . (-> ((-> (&rest 'a) 'b)) 'b))
                    (result . (-> ('a 'b 'c) (Result 'a 'b 'c)))))))

      (it "empty"
        (expect (Emil:infer-type '(f (lambda () 0)) env)
                :to-throw 'Emil:type-error))

      (it "fixed arguments"
        (expect (Emil:infer-type
                 '(f (lambda (a b c) (result a b c)))
                 env)
                :to-throw 'Emil:type-error))

      (it "optional arguments"
        (expect (Emil:infer-type
                 '(f (lambda (a &optional b c) (result a b c)))
                 env)
                :to-throw 'Emil:type-error))

      (it "rest argument"
        (expect (Emil:infer-type
                 '(f (lambda (&rest a) a))
                 env)
                :to-equal '(List 'a)))

      (it "optional and rest argument"
        (expect (Emil:infer-type
                 '(f (lambda (a &optional b &rest c) (result a b c)))
                 env)
                :to-throw 'Emil:type-error)))

    (describe "optional and rest argument with"
      :var ((env (Emil:Env:Alist:read
                  nil
                  '((f . (-> ((-> ('a &optional 'b &rest 'c) 'd)) 'd))
                    (result . (-> ('a 'b 'c) (Result 'a 'b 'c)))))))

      (it "empty"
        (expect (Emil:infer-type '(f (lambda () 0)) env)
                :to-throw 'Emil:type-error))

      (it "fixed arguments"
        (expect (Emil:infer-type
                 '(f (lambda (a b c) (result a b c)))
                 env)
                :to-throw 'Emil:type-error))

      (it "optional arguments"
        (expect (Emil:infer-type
                 '(f (lambda (a &optional b c) (result a b c)))
                 env)
                :to-throw 'Emil:type-error))

      (it "rest argument"
        (expect (Emil:infer-type
                 '(f (lambda (&rest a) a))
                 env)
                :to-equal '(List Any)))

      (it "optional and rest argument"
        (expect (Emil:infer-type
                 '(f (lambda (a &optional b &rest c) (result a b c)))
                 env)
                :to-equal '(Result 'a 'b (List 'c))))))

  (describe "checking"
    (describe "empty with"
      (it "empty"
        (expect (Emil:infer-type
                 '(f #'g)
                 (Emil:Env:Alist:read
                  nil
                  '((f . (-> ((-> () 'a)) 'a))
                    (g . (-> () 'a)))))
                :to-equal ''a))

      (it "fixed arguments"
        (expect (Emil:infer-type
                 '(f #'g)
                 (Emil:Env:Alist:read
                  nil
                  '((f . (-> ((-> () 'a)) 'a))
                    (g . (-> ('a 'b 'c) (Result 'a 'b 'c))))))
                :to-throw 'Emil:type-error))

      (it "optional arguments"
        (expect (Emil:infer-type
                 '(f #'g)
                 (Emil:Env:Alist:read
                  nil
                  '((f . (-> ((-> () 'a)) 'a))
                    (g . (-> ('a &optional 'b 'c) (Result 'a 'b 'c))))))
                :to-throw 'Emil:type-error))

      (it "rest argument"
        (expect (Emil:infer-type
                 '(f #'g)
                 (Emil:Env:Alist:read
                  nil
                  '((f . (-> ((-> () 'a)) 'a))
                    (g . (-> (&rest 'a) 'a)))))
                :to-equal ''a))

      (it "optional and rest argument"
        (expect (Emil:infer-type
                 '(f #'g)
                 (Emil:Env:Alist:read
                  nil
                  '((f . (-> ((-> () 'a)) 'a))
                    (g . (-> ('a &optional 'b &rest 'c) (Result 'a 'b 'c))))))
                :to-throw 'Emil:type-error)))

    (describe "fixed arguments with"
      (it "empty"
        (expect (Emil:infer-type
                 '(f #'g)
                 (Emil:Env:Alist:read
                  nil
                  '((f . (-> ((-> ('a 'b 'c) 'd)) 'd))
                    (g . (-> () 'a)))))
                :to-throw 'Emil:type-error))

      (it "fixed arguments"
        (expect (Emil:infer-type
                 '(f #'g)
                 (Emil:Env:Alist:read
                  nil
                  '((f . (-> ((-> ('a 'b 'c) 'd)) 'd))
                    (g . (-> ('a 'b 'c) (Result 'a 'b 'c))))))
                :to-equal '(Result 'a 'b 'c)))

      (it "optional arguments"
        (expect (Emil:infer-type
                 '(f #'g)
                 (Emil:Env:Alist:read
                  nil
                  '((f . (-> ((-> ('a 'b 'c) 'd)) 'd))
                    (g . (-> ('a &optional 'b 'c) (Result 'a 'b 'c))))))
                :to-equal '(Result 'a 'b 'c)))

      (it "rest argument"
        (expect (Emil:infer-type
                 '(f #'g)
                 (Emil:Env:Alist:read
                  nil
                  '((f . (-> ((-> ('a 'b 'c) 'd)) 'd))
                    (g . (-> (&rest 'a) 'a)))))
                :to-equal ''a))

      (it "optional and rest argument"
        (expect (Emil:infer-type
                 '(f #'g)
                 (Emil:Env:Alist:read
                  nil
                  '((f . (-> ((-> ('a 'b 'c) 'd)) 'd))
                    (g . (-> ('a &optional 'b &rest 'c)
                             (Result 'a 'b (List 'c)))))))
                :to-equal '(Result 'a 'b (List 'c)))))

    (describe "optional arguments with"
      (it "empty"
        (expect (Emil:infer-type
                 '(f #'g)
                 (Emil:Env:Alist:read
                  nil
                  '((f . (-> ((-> ('a &optional 'b 'c) 'd)) 'd))
                    (g . (-> () 'a)))))
                :to-throw 'Emil:type-error))

      (it "fixed arguments"
        (expect (Emil:infer-type
                 '(f #'g)
                 (Emil:Env:Alist:read
                  nil
                  '((f . (-> ((-> ('a &optional 'b 'c) 'd)) 'd))
                    (g . (-> ('a 'b 'c)
                             (Result 'a 'b 'c))))))
                :to-throw 'Emil:type-error))

      (it "optional arguments"
        (expect (Emil:infer-type
                 '(f #'g)
                 (Emil:Env:Alist:read
                  nil
                  '((f . (-> ((-> ('a &optional 'b 'c) 'd)) 'd))
                    (g . (-> ('a &optional 'b 'c)
                             (Result 'a 'b 'c))))))
                :to-equal '(Result 'a 'b 'c)))

      (it "rest argument"
        (expect (Emil:infer-type
                 '(f #'g)
                 (Emil:Env:Alist:read
                  nil
                  '((f . (-> ((-> ('a &optional 'b 'c) 'd)) 'd))
                    (g . (-> (&rest 'a) 'a)))))
                :to-equal ''a))

      (it "optional and rest argument"
        (expect (Emil:infer-type
                 '(f #'g)
                 (Emil:Env:Alist:read
                  nil
                  '((f . (-> ((-> ('a &optional 'b 'c) 'd)) 'd))
                    (g . (-> ('a &optional 'b &rest 'c)
                             (Result 'a 'b 'c))))))
                :to-equal '(Result 'a 'b 'c))))

    (describe "rest argument with"
      (it "empty"
        (expect (Emil:infer-type
                 '(f #'g)
                 (Emil:Env:Alist:read
                  nil
                  '((f . (-> ((-> (&rest 'a) 'b)) 'b))
                    (g . (-> () 'a)))))
                :to-throw 'Emil:type-error))

      (it "fixed arguments"
        (expect (Emil:infer-type
                 '(f #'g)
                 (Emil:Env:Alist:read
                  nil
                  '((f . (-> ((-> (&rest 'a) 'b)) 'b))
                    (g . (-> ('a 'b 'c)
                             (Result 'a 'b 'c))))))
                :to-throw 'Emil:type-error))

      (it "optional arguments"
        (expect (Emil:infer-type
                 '(f #'g)
                 (Emil:Env:Alist:read
                  nil
                  '((f . (-> ((-> (&rest 'a) 'b)) 'b))
                    (g . (-> ('a &optional 'b 'c)
                             (Result 'a 'b 'c))))))
                :to-throw 'Emil:type-error))

      (it "rest argument"
        (expect (Emil:infer-type
                 '(f #'g)
                 (Emil:Env:Alist:read
                  nil
                  '((f . (-> ((-> (&rest 'a) 'b)) 'b))
                    (g . (-> (&rest 'a) 'a)))))
                :to-equal ''a))

      (it "optional and rest argument"
        (expect (Emil:infer-type
                 '(f #'g)
                 (Emil:Env:Alist:read
                  nil
                  '((f . (-> ((-> (&rest 'a) 'b)) 'b))
                    (g . (-> ('a &optional 'b &rest 'c)
                             (Result 'a 'b 'c))))))
                :to-throw 'Emil:type-error)))

    (describe "optional and rest argument with"
      (it "empty"
        (expect (Emil:infer-type
                 '(f #'g)
                 (Emil:Env:Alist:read
                  nil
                  '((f . (-> ((-> ('a &optional 'b &rest 'c) 'd)) 'd))
                    (g . (-> () 'a)))))
                :to-throw 'Emil:type-error))

      (it "fixed arguments"
        (expect (Emil:infer-type
                 '(f #'g)
                 (Emil:Env:Alist:read
                  nil
                  '((f . (-> ((-> ('a &optional 'b &rest 'c) 'd)) 'd))
                    (g . (-> ('a 'b 'c)
                             (Result 'a 'b 'c))))))
                :to-throw 'Emil:type-error))

      (it "optional arguments"
        (expect (Emil:infer-type
                 '(f #'g)
                 (Emil:Env:Alist:read
                  nil
                  '((f . (-> ((-> ('a &optional 'b &rest 'c) 'd)) 'd))
                    (g . (-> ('a &optional 'b 'c)
                             (Result 'a 'b 'c))))))
                :to-throw 'Emil:type-error))

      (it "rest argument"
        (expect (Emil:infer-type
                 '(f #'g)
                 (Emil:Env:Alist:read
                  nil
                  '((f . (-> ((-> ('a &optional 'b &rest 'c) 'd)) 'd))
                    (g . (-> (&rest 'a) 'a)))))
                :to-equal ''a))

      (it "optional and rest argument"
        (expect (Emil:infer-type
                 '(f #'g)
                 (Emil:Env:Alist:read
                  nil
                  '((f . (-> ((-> ('a &optional 'b &rest 'c) 'd)) 'd))
                    (g . (-> ('a &optional 'b &rest 'c)
                             (Result 'a 'b 'c))))))
                :to-equal '(Result 'a 'b 'c)))))

  (describe "application"
    :var ((env (Emil:Env:Alist:read
                nil
                '((f . (-> () 'a))))))

    (describe "empty with"
      (it "no arguments"
        (expect (Emil:infer-type
                 '(f)
                 env)
                :to-equal ''a))

      (it "one argument"
        (expect (Emil:infer-type
                 '(f 0)
                 env)
                :to-throw 'Emil:type-error)))

    (describe "fixed arguments with"
      :var ((env (Emil:Env:Alist:read
                  nil
                  '((f . (-> ('a 'b 'c) (Result 'a 'b 'c)))))))

      (it "no arguments"
        (expect (Emil:infer-type
                 '(f)
                 env)
                :to-throw 'Emil:type-error))

      (it "one argument"
        (expect (Emil:infer-type
                 '(f 0)
                 env)
                :to-throw 'Emil:type-error))

      (it "three arguments"
        (expect (Emil:infer-type
                 '(f 0 "1" [2])
                 env)
                :to-equal '(Result integer string (Vector Any)))))

    (describe "optional arguments with"
      :var ((env (Emil:Env:Alist:read
                  nil
                  '((f . (-> ('a &optional 'b 'c) (Result 'a 'b 'c)))))))

      (it "no arguments"
        (expect (Emil:infer-type
                 '(f)
                 env)
                :to-throw 'Emil:type-error))

      (it "one argument"
        (expect (Emil:infer-type
                 '(f 0)
                 env)
                :to-equal '(Result integer 'a 'b)))

      (it "three arguments"
        (expect (Emil:infer-type
                 '(f 0 "1" [2])
                 env)
                :to-equal '(Result integer string (Vector Any)))))

    (describe "rest argument with"
      :var ((env (Emil:Env:Alist:read
                  nil
                  '((f . (-> (&rest 'a) 'a))))))

      (it "no arguments"
        (expect (Emil:infer-type
                 '(f)
                 env)
                :to-equal ''a))

      (it "one argument"
        (expect (Emil:infer-type
                 '(f 0)
                 env)
                :to-equal 'integer))

      (it "three arguments"
        (expect (Emil:infer-type
                 '(f 0 "1" [2])
                 env)
                :to-throw 'Emil:type-error))

      (it "three arguments with annotation"
        (expect (Emil:infer-type
                 '(f "1" [2] (Emil:is 0 Any))
                 (Emil:Env:Alist:read
                  nil
                  '((f . (-> (&rest 'a) 'a)))))
                :to-equal 'Any)))

    (describe "optional and rest argument with"
      :var ((env (Emil:Env:Alist:read
                  nil
                  '((f . (-> ('a &optional 'b &rest 'c) (Result 'a 'b 'c)))))))

      (it "no arguments"
        (expect (Emil:infer-type
                 '(f)
                 env)
                :to-throw 'Emil:type-error))

      (it "one argument"
        (expect (Emil:infer-type
                 '(f 0)
                 env)
                :to-equal '(Result integer 'a 'b)))

      (it "three arguments"
        (expect (Emil:infer-type
                 '(f 0 "1" [2])
                 env)
                :to-equal '(Result integer string (Vector Any)))))

    (describe "fn keyword"
      (it "basic test"
        (expect (Emil:infer-type
                 '(fn ((a integer)) a))
                :to-equal '(-> (integer) integer))))))
