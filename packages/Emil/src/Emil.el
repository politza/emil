;;; Emil.el --- Emacs mit Inferenz Logik.             -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Andreas Politz

;; Author: Andreas Politz <mail@andreas-politz.de>
;; Keywords: extensions
;; Version: 1.0.0beta1
;; Package-Requires: ((emacs "29.1") (dash "2.19.1") (Struct "1.0.0beta1") (Commons "1.0.0beta1") (Transformer "1.0.0beta1"))

(require 'dash)
(require 'pcase)
(require 'Struct)
(require 'Emil/Type)
(require 'Emil/Env)
(require 'Emil/Error)
(require 'Emil/TypedForm)
(require 'Emil/Analyzer)
(require 'Emil/Transformer)

;; See "Complete and Easy Bidirectional Typechecking for Higher-Rank
;; Polymorphism"; Jana Dunfield, Neelakantan R. Krishnaswami .

(defun Emil:infer-type (form &optional environment)
  (-> (Emil:transform form environment)
      (Struct:get :type)
      Emil:Type:normalize
      Emil:Type:print))

(defun Emil:transform (form &optional environment)
  (-let* ((analyzer (Emil:Analyzer))
          ((context . typed-form)
           (Emil:Analyzer:infer
            analyzer form (Emil:Context)
            (Emil:Env:Alist :parent environment))))
    (when (Emil:Analyzer:has-errors? analyzer)
      (signal 'Emil:type-error (list (Struct:get analyzer :messages))))
    (Emil:TypedForm*
     ,@typed-form
     :type (Emil:Context:resolve context (Struct:get typed-form :type)))))

(provide 'Emil)
;;; Emil.el ends here
