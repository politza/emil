;; -*- lexical-binding: t -*-

(defconst Struct:Support:syntax-highlight-keywords
  '(("(\\(Struct:define\\|Trait:define\\)[ \t]*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-type-face nil t))
    ("(\\(Struct:defmethod\\)[ \t]*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face nil t))
    ("(\\(Trait:implement\\)[ \t]*\\(\\(?:\\sw\\|\\s_\\)+\\)?[ \t]*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-type-face nil t)
     (3 font-lock-type-face nil t))))

(define-minor-mode Struct:Support:syntax-highlight-mode
  "Adds support for better syntax-highlighting of structs."
  :global nil
  (cond
   (Struct:Support:syntax-highlight-mode
    (font-lock-add-keywords
     nil
     Struct:Support:syntax-highlight-keywords)
    (advice-add 'lisp-string-after-doc-keyword-p
                :around #'Struct:Support:documentation-advice))
   (t
    (font-lock-remove-keywords
     nil 
     Struct:Support:syntax-highlight-keywords)
    (advice-remove 'lisp-string-after-doc-keyword-p
                   #'Struct:Support:documentation-advice)))
  (font-lock-flush))

(defun Struct:Support:documentation-advice (fn list-begin start-position)
  (or (save-excursion
        (goto-char start-position)
        (ignore-errors
          (backward-sexp)
          (backward-char)
          (and (eq list-begin (point))
               (progn
                 (backward-up-list)
                 (looking-at-p "(Struct:define\\_>")))))
      (funcall fn list-begin start-position)))

(provide 'Struct:Support)
;;; Struct:Support.el ends here
