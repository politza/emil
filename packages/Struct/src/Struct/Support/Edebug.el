;; -*- lexical-binding: t -*-

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
