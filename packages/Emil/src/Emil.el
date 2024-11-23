;;; Emil.el --- Emacs mit Inferenz Logik.             -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Andreas Politz

;; Author: Andreas Politz
;; Keywords: extensions
;; Version: 1.0.0beta1
;; Package-Requires: ((emacs "29.1") (dash "2.19.1") (Struct "1.0.0beta1") (Commons "1.0.0beta1") (Transformer "1.0.0beta1"))

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

(require 'dash)
(require 'pcase)
(require 'bytecomp)
(require 'Struct)
(require 'Emil/Error)
(require 'Emil/Type)
(require 'Emil/Env)
(require 'Emil/Form)
(require 'Emil/Analyzer)
(require 'Emil/Transformer)
(require 'Emil/Annotation)
(require 'Emil/Syntax)
(require 'Emil/Builtin)

;; See "Complete and Easy Bidirectional Typechecking for Higher-Rank
;; Polymorphism"; Jana Dunfield, Neelakantan R. Krishnaswami .

(defun Emil:infer-type (form &optional environment)
  (-> (Emil:transform form environment)
      (Struct:get :type)
      Emil:Type:normalize
      Emil:Type:print))

(defun Emil:infer-type* (form &optional environment)
  (-> (Emil:transform* form environment)
      (Struct:get :type)
      Emil:Type:normalize
      Emil:Type:print))

(defun Emil:transform (form &optional environment no-error)
  (let ((standard-env (Emil:Env:Hierarchy
                       :environments
                       (list (Emil:Env:Global) (Emil:Env:Fallback)))))
    (Emil:transform*
     form
     (if environment
         (Emil:Env:Hierarchy
          :environments (list environment standard-env))
       standard-env)
     no-error)))

(defun Emil:transform* (form &optional environment no-error)
  (-let* ((analyzer (Emil:Analyzer))
          ((context . typed-form)
           (Emil:Analyzer:infer
            analyzer form (Emil:Context)
            (Emil:Env:Alist :parent environment))))
    (pcase (Commons:evaluation-context)
      ('compile
       (mapc #'Emil:Message:byte-compile-log
             (Struct:get analyzer :messages))))
    (if-let (error (unless no-error
                     (Emil:Analyzer:first-error analyzer)))
        (pcase (Commons:evaluation-context)
          ((or 'eval 'load)
           (signal (Struct:get error :error-condition)
                   (list (Struct:get error :content)))))
      (Emil:Form:with-type
       typed-form
       (Emil:Context:resolve context (Struct:get typed-form :type))))))

(provide 'Emil)
;;; Emil.el ends here
