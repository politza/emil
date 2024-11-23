;; -*- lexical-binding: t -*-

(require 'Commons)

(Commons:define-error Emil:error "Emil error")

(Commons:define-error Emil:syntax-error
  "Syntax error" Emil:error)

(Commons:define-error Emil:type-error
  "Type error" Emil:error)

(provide 'Emil/Error)
