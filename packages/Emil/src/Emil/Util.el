;; -*- lexical-binding: t -*-

(require 'dash)
(require 'Struct)

(declare-function Emil:Type:Existential nil)

(Struct:define Emil:Util:NameGenerator
  "Generator for displayable (variable) names.

Creates the sequence of symbols \(a b c ... z aa bb cc ... zz ... aaa
bbb ...\)."
  (character :type (integer 97 122) :default ?a :mutable t)
  (repetition :type (integer 1 *) :default 1 :mutable t))

(defun Emil:Util:NameGenerator:next (self)
  "Returns the next name in the sequence as a symbol."
  (let* ((character (format "%c" (Struct:get self :character)))
         (name (intern (apply #'concat (-repeat (Struct:get self :repetition)
                                                character)))))
    (cond
     ((= (Struct:get self :character) ?z)
      (Struct:set self :character ?a)
      (Struct:update self :repetition #'1+))
     (t (Struct:update self :character #'1+)))
    name))

(Struct:define Emil:Util:ExistentialGenerator
  "Generator for instances of type `Emil:Type:Existential'."
  (generator :default (Emil:Util:NameGenerator)))

(defun Emil:Util:ExistentialGenerator:next (self)
  (require 'Emil/Type)
  (Emil:Type:Existential
   :name (Emil:Util:NameGenerator:next (Struct:get self :generator))))

(defun Emil:Util:map-reduce (fn init list)
  "Maps FN across list, while accumulating a value starting with INIT."
  (-let (((acc . results)
          (-reduce-from
           (-lambda ((acc . results) item)
             (-let (((acc . result)
                     (funcall fn acc item)))
               (cons acc (cons result results))))
           (list init)
           list)))
    (cons acc (nreverse results))))

(defun Emil:Util:lambda-variables (arguments)
  "Return ARGUMENTS excluding &optional and &rest keywords."
  (--filter (not (memq it '(&optional &rest))) arguments))

(provide 'Emil/Util)
