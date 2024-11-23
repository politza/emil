;; -*- lexical-binding: t -*-

(require 'Commons)

(Commons:define-error Emil:error "Emil error")

(Commons:define-error Emil:invalid-type-form
  "Invalid type form" Emil:error)

(Commons:define-error Emil:type-error
  "Type error" Emil:error)

(provide 'Emil/Error)
