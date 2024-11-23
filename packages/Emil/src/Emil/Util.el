;; -*- lexical-binding: t -*-

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

(provide 'Emil/Util)
