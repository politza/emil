;; -*- lexical-binding: t -*-

(eval-and-compile (require 'dash))

(defun Commons:keyword-to-symbol (keyword)
  (unless (keywordp keyword)
    (error "Keyword expected: %s" keyword))
  (intern (substring (symbol-name keyword) 1)))

(defun Commons:symbol-to-keyword (symbol)
  (unless (and (symbolp symbol)
               (not (keywordp symbol)))
    (error "Symbol expected: %s" symbol))
  (intern (concat ":" (symbol-name symbol))))

(defun Commons:split-property-list-start (list)
  (let ((property-list nil))
    (while (and (cdr list)
                (keywordp (car list)))
      (push (pop list) property-list)
      (push (pop list) propert-list))
    (list (nreverse property-list) list)))

(defun Commons:split-property-list-end (list)
  (--split-with (not (keywordp it)) list))

(provide 'Commons)
