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

(require 'dash)
(require 'cl-lib)
(require 'Struct)

(defvar Struct:Support:Imenu:trait-implement-regexp
  (rx line-start (* (syntax whitespace))
      "(" (* (syntax whitespace))
      (seq (group-n 1 "Trait:implement")
               (+ (syntax whitespace))
               (group-n 2
                 (+ (or (syntax word)
                        (syntax symbol)))
                 (+ (syntax whitespace))
                 (+ (or (syntax word)
                        (syntax symbol))))))
  "A regexp matching trait implementations.")

(defvar Struct:Support:Imenu:trait-define-regexp
  (rx line-start (* (syntax whitespace))
      "(" (* (syntax whitespace))
      (seq (group-n 1 "Trait:define")
           (+ (syntax whitespace))
           (group-n 2 (+ (or (syntax word)
                             (syntax symbol))))))
  "A regexp matching trait definitions.")

(defvar Struct:Support:Imenu:struct-implement-regexp
  (rx line-start (* (syntax whitespace))
      "(" (* (syntax whitespace))
      (seq (group-n 1 "Struct:implement")
               (+ (syntax whitespace))
               (group-n 2
                 (+ (or (syntax word)
                        (syntax symbol)))
                 (+ (syntax whitespace))
                 (+ (or (syntax word)
                        (syntax symbol))))))
  "A regexp matching struct implementations.")

(defvar Struct:Support:Imenu:struct-define-regexp
  (rx line-start (* (syntax whitespace))
      "(" (* (syntax whitespace))
      (seq (group-n 1 "Struct:define")
           (+ (syntax whitespace))
           (group-n 2 (+ (or (syntax word)
                             (syntax symbol))))))
  "A regexp matching struct definitions.")

(defvar Struct:Support:Imenu:fn-regexp
  (rx line-start (* (syntax whitespace))
      "(" (* (syntax whitespace))
      (seq (group-n 1 "fn")
           (+ (syntax whitespace))
           (group-n 2 (+ (or (syntax word)
                             (syntax symbol))))))
  "A regexp matching struct and trait function definitions.")

(defconst Struct:Support:Imenu:generic-expressions
  `(("Struct:implement" ,Struct:Support:Imenu:struct-implement-regexp 2)
    ("Struct:define" ,Struct:Support:Imenu:struct-define-regexp 2)
    ("Trait:implement" ,Struct:Support:Imenu:trait-implement-regexp 2)
    ("Trait:define" ,Struct:Support:Imenu:trait-define-regexp 2)
    ("fn" ,Struct:Support:Imenu:fn-regexp 2)))

;;;###autoload
(define-minor-mode Struct:Support:Imenu:mode
  "Enable `imenu' support for struct and trait-definitions."
  :group 'emacs
  (cond
   (Struct:Support:Imenu:mode
    (--each Struct:Support:Imenu:generic-expressions
      (cl-pushnew it imenu-generic-expression)))
   (t
    (setq imenu-generic-expression
          (--filter (memq it Struct:Support:Imenu:generic-expressions)
                    imenu-generic-expression)))))

(provide 'Struct/Support/Imenu)
