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

