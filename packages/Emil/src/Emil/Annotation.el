;; -*- lexical-binding: t -*-

(defconst Emil:Annotation:macros '(Emil:is Emil:as))

(defmacro Emil:is (form type)
  "Declare that FORM is of type TYPE.

Otherwise, this just expands to FORM."
  (declare (indent 1))
  (ignore type)
  form)

(defmacro Emil:as (form type)
  "Cast FORM to type TYPE.

Otherwise, this just expands to FORM."
  (declare (indent 1))
  (ignore type)
  form)

(provide 'Emil/Annotation)

