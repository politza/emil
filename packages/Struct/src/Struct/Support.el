;; -*- lexical-binding: t -*-

(require 'Trait)

(defconst Struct:Support:complete-properties-symbol
  'Struct:Support:complete-properties-symbol)

(defconst Struct:Support:syntax-keywords
  '(("(\\(Struct:\\(?:implement\\|define\\)\\|Trait:define\\)[ \t]+\\(\\(?:\\sw\\|\\s_\\)+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-type-face nil t))
    ("(\\(fn\\)[ \t]+\\(\\(?:\\sw\\|\\s_\\)+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face nil t))
    ("(\\(Trait:implement\\)[ \t]+\\(\\(?:\\sw\\|\\s_\\)+\\)?[ \t]*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-type-face nil t)
     (3 font-lock-type-face nil t))
    ("\\(\\_<self\\)\\(?:\\.?\\|\\_>\\)"
     (1 font-lock-constant-face))))

(define-minor-mode Struct:Support:syntax-mode
  "Adds support for better syntaxing of structs."
  :global nil
  (cond
   (Struct:Support:syntax-mode
    (font-lock-add-keywords
     nil
     Struct:Support:syntax-keywords)
    (advice-add 'lisp-string-after-doc-keyword-p
                :around #'Struct:Support:-syntax-doc-advice))
   (t
    (font-lock-remove-keywords
     nil 
     Struct:Support:syntax-keywords)
    (advice-remove 'lisp-string-after-doc-keyword-p
                   #'Struct:Support:-syntax-doc-advice)))
  (font-lock-flush))

(defun Struct:Support:-syntax-doc-advice (fn list-begin start-position)
  "Advice `lisp-string-after-doc-keyword-p' to recognize property documentation."
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

(defun Struct:Support:completion-at-point-function ()
  "Completes struct property-keywords and trait-names."
  (or (Struct:Support:complete-struct-keywords)
      (Struct:Support:-complete-trait-names)))

(defun Struct:Support:complete-struct-keywords ()
  (-when-let* (((type constructor-type)
                (Struct:Support:-struct-at-point))
               ((start . end)
                (Struct:Support:-keyword-region
                 nil (eq constructor-type 'macro)))
               (completions
                (--map (symbol-name (car it))
                       (Struct:unsafe-get type :properties))))
    (list start end completions)))

(defun Struct:Support:-complete-trait-names (&optional point)
  (-when-let* (((start . end)
                (Struct:Support:-trait-completion-region point))
               (completions
                (Struct:Support:defined-traits)))
    (list start end completions)))

(defun Struct:Support:-trait-completion-region (&optional point)
  (unless point (setq point (point)))
  (when (save-excursion
          (goto-char point)
          (or (ignore-error 'scan-error
                (save-excursion
                  (backward-sexp (if (thing-at-point 'symbol) 2 1))
                  (and (eq ?\( (char-before))
                       (looking-at-p "Trait:implement\\_>"))))
              (ignore-error 'scan-error
                (save-excursion
                  (backward-up-list)
                  (backward-sexp 2)
                  (and (eq ?\( (char-before))
                       (looking-at-p "Trait:define\\_>"))))))
    (or (bounds-of-thing-at-point 'symbol)
        (cons point point))))

(defun Struct:Support:defined-traits ()
  "Returns a list of defined `Trait' names."
  (let ((traits nil))
    (mapatoms (lambda (symbol)
                (when (Trait:get symbol)
                  (push symbol traits))))
    traits))

(defun Struct:Support:-keyword-region (&optional point allow-extended-syntax)
  
  (unless point (setq point (point)))
  (cl-flet ((next-sexp (n)
              (while (and (> n 0)
                          (ignore-error 'scan-error (forward-sexp) t)
                          (> (skip-syntax-forward " >") 0))
                (cl-decf n))
              (unless (= n 0)
                (throw 'result nil))))
    (save-excursion
      (goto-char point)
      (ignore-error 'scan-error
        (backward-up-list)
        (when (eq ?\( (char-after))
          (forward-char)
          (catch 'result
            (next-sexp 1)
            (while :true
              (cond
               ((eq ?: (char-after))
                (-let (((begin . end) (bounds-of-thing-at-point 'symbol)))
                  (when (and begin end
                             (<= begin point)
                             (>= end point))
                    (throw 'result (cons begin end)))
                  (next-sexp 2)))
               ((and (>= (point) point)
                     (string-match-p "\\`[\t ]*\\'" (buffer-substring point (point))))
                (throw 'result (cons point point)))
               (allow-extended-syntax
                (next-sexp 1))))))))))

(defun Struct:Support:-struct-at-point (&optional point)
  (save-excursion
    (goto-char (or point (point)))
    (ignore-error 'scan-error
      (backward-up-list)
      (when (eq ?\( (char-after))
        (forward-char)
        (when-let (symbol (thing-at-point 'symbol))
          (let* ((macro? (macrop (intern-soft symbol)))
                 (type (intern-soft (if (string-suffix-p "*" symbol)
                                        (substring symbol 0 (1- (length symbol)))
                                      symbol)))
                 (struct (Struct:Type:get type)))
            (when struct
              (list struct (if macro? 'macro 'defun)))))))))

(provide 'Struct/Support)
;;; Struct:Support.el ends here
