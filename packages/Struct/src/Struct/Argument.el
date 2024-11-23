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

(require 'Struct)

(Struct:define Struct:Argument
  "Defines an argument of a `Struct:FunctionDeclaration'."
  (name
   "The name of this argument."
   :type symbol)
  (type
   "The type of this argument."
   :type nil)
  (default
   "The default value of this argument."
   :type nil)
  (kind
   "The kind of this argument.

This may be one of `&optional' and `&rest' for optional and rest
arguments; or nil for regular ones."
   :type (member &optional &rest nil)))

(defun Struct:Argument:equivalent? (argument other)
  (cl-check-type argument Struct:Argument)
  (cl-check-type other Struct:Argument)
  (and (equal (Struct:get argument :type)
              (Struct:get other :type))
       (eq (Struct:get argument :kind)
           (Struct:get other :kind))))

(defun Struct:Argument:read (form &optional kind)
  "Reads an argument from FORM.

KIND should be one of `&optional', '&rest' or nil and specifies the
kind of the read argument."
  (when (symbolp form)
    (setq form (list form)))
  (unless (and (consp form)
               (<= (length form) 3)
               (symbolp (car form)))
    (error "Form is not a valid argument: %s" form))
  
  (Struct:Argument
   :name (pop form)
   :type (pop form)
   :default (pop form)
   :kind kind))

(defun Struct:Argument:optional? (self)
  (eq '&optional (Struct:get self :kind)))

(defun Struct:Argument:rest? (self)
  (eq '&rest (Struct:get self :kind)))

(defun Struct:Argument:regular? (self)
  (null (Struct:get self :kind)))

(defun Struct:Argument:default? (self)
  (not (null (Struct:get self :default))))

(provide 'Struct/Argument)
