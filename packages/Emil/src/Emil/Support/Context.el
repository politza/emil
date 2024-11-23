;; -*- lexical-binding: t -*-

(require 'Struct/Impl)
(require 'Trait)
(require 'Emil/Parser)
(require 'Emil/Syntax)

(Struct:define Emil:Support:Context)

(defun Emil:Support:Context:at (&optional position))

(provide 'Emil/Support/Context)
