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


(def-edebug-elem-spec 'Struct:fn-argument
  '([&or symbolp (symbolp &optional sexp def-form)]))

(def-edebug-elem-spec 'Struct:fn-arguments
  '(([&rest Struct:fn-argument]
     [&optional ["&optional" Struct:fn-argument &rest Struct:fn-argument]]
     &optional ["&rest" Struct:fn-argument]
     &optional ["->" sexp])))

(def-edebug-spec Struct:implement
  (&define
   name
   [&rest [keywordp sexp]]
   [&rest ("fn" symbolp Struct:fn-arguments def-body)]))

(def-edebug-spec Trait:define
  (&define
   [&name "Trait@" symbolp] (&rest symbolp)
   [&rest [keywordp sexp]]
   [&rest ("fn" symbolp Struct:fn-arguments def-body)]))

(def-edebug-spec Trait:implement
  (&define
   [&name "Trait@" symbolp] name
   [&rest [keywordp sexp]]
   [&rest ("fn" symbolp Struct:fn-arguments def-body)]))

(provide 'Struct/Support/Edebug)
