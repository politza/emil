;; -*- lexical-binding: t -*-

(require 'Struct)
(require 'Struct/Impl)
(require 'Trait)

(Struct:define Emil:Support:StructImpl
  (name :type symbol)
  (properties :type list)
  (functions :type (List (Struct:Function))))

(Struct:define Emil:Support:TraitDef
  (name :type symbol)
  (supertraits :type (List symbol))
  (properties :type list)
  (functions :type (List (Struct:Function))))

(Struct:define Emil:Support:TraitImpl
  (name :type symbol)
  (type :type symbol)
  (properties :type list)
  (functions :type (List (Struct:Function))))

(defconst Emil:Support:structure-start-regexp
  (format "([ \t]*%s\\_>"
          (regexp-opt
           '("Struct:implement" "Trait:define" "Trait:implement")
           t)))

(defconst Emil:Support:keyword-regexp ":\\(?:\\w\\|\\s_\\)+")

(defconst Emil:Support:fn-regexp "^ *( *fn \\(\\(?:\\w\\|\\s_\\)+\\)")

(defun Emil:Support:parse (&optional position)
  (let ((symbols-with-pos-enabled t))
    (save-excursion
      (when position (goto-char position))
      (ignore-error 'user-error
        (while (not (looking-at-p Emil:Support:structure-start-regexp))
          (up-list -1 t t)))
      (when (looking-at Emil:Support:structure-start-regexp)
        (let ((kind (read-positioning-symbols (match-string 1)))
              (end (Emil:Support:structure-end)))
          (save-restriction
            (narrow-to-region (point) end)
            (condition-case-unless-debug nil
                (cl-case kind
                  (Struct:implement (Emil:Support:parse-struct-impl))
                  (Trait:define (Emil:Support:parse-trait-def))
                  (Trait:implement (Emil:Support:parse-trait-impl)))
              (error nil))))))))

(defun Emil:Parse:current-function (&optional position)
  (save-excursion
    (when position (goto-char position))
    (skip-syntax-backward " ")
    (ignore-error 'user-error
      (while (not (looking-at-p Emil:Support:fn-regexp))
        (up-list -1 t t)
        (skip-syntax-backward " ")))
    (when (looking-at Emil:Support:fn-regexp)
      (read-positioning-symbols (match-string 1)))))

(defun Emil:Support:structure-end ()
  (save-excursion
    (or (ignore-errors (forward-sexp)
                       (point))
        (let ((column (current-column)))
          (while (and (not (eobp))
                      (>= (current-column) column))
            (forward-line 1))
          (point)))))

(defun Emil:Support:parse-struct-impl ()
  (let ((name nil)
        (properties nil)
        (functions nil))
    (down-list 1)
    (forward-sexp 2)
    (backward-sexp)
    (when (setq name (read-positioning-symbols (current-buffer)))
      (setq properties (Emil:Support:parse-properties))
      (setq functions (Emil:Support:map-functions
                       (lambda (fn-name arguments body)
                         (Struct:read-function
                          name
                          `(fn ,fn-name ,arguments ,@body)
                          (plist-get properties :disable-syntax)))))
      (Emil:Support:StructImpl* name functions properties))))

(defun Emil:Support:parse-trait-def ()
  (let ((name nil)
        (supertraits nil)
        (properties nil)
        (functions nil))
    (down-list 1)
    (forward-sexp 2)
    (backward-sexp)
    (when (setq name (read-positioning-symbols (current-buffer)))
      (setq supertraits (ignore-errors (read-positioning-symbols (current-buffer))))
      (unless (listp supertraits)
        (setq supertraits nil))
      (setq supertraits (-filter #'symbolp supertraits))
      (setq properties (Emil:Support:parse-properties))
      (setq functions (Emil:Support:map-functions
                       (lambda (fn-name arguments body)
                         (Trait:read-function
                          name
                          `(fn ,fn-name ,arguments ,@body)
                          (plist-get properties :disable-syntax)))))
      (Emil:Support:TraitDef* name supertraits properties functions))))

(defun Emil:Support:parse-trait-impl ()
  (let ((name nil)
        (type nil)
        (properties nil)
        (functions nil))
    (down-list 1)
    (forward-sexp 2)
    (backward-sexp)
    (when (and (setq name (read-positioning-symbols (current-buffer)))
               (setq type (read-positioning-symbols (current-buffer))))
      (setq properties (Emil:Support:parse-properties))
      (setq functions (Emil:Support:map-functions
                       (lambda (fn-name arguments body)
                         (Trait:read-impl
                          name
                          type
                          `(fn ,fn-name ,arguments ,@body)
                          (plist-get properties :disable-syntax)))))
      (Emil:Support:TraitImpl* name type properties functions))))

(defun Emil:Support:parse-properties ()
  (let ((properties nil))
    (skip-syntax-forward " >")
    (while (looking-at-p Emil:Support:keyword-regexp)
      (push (read-positioning-symbols (current-buffer)) properties)
      (push (read-positioning-symbols (current-buffer)) properties))
    (nreverse properties)))

(defun Emil:Support:map-functions (fn)
  (let ((functions nil))
    (while (re-search-forward Emil:Support:fn-regexp nil t)
      (ignore-errors
        (let* ((name (read-positioning-symbols (match-string 1)))
               (start (match-beginning 0))
               (end (save-excursion
                      (goto-char start)
                      (Emil:Support:structure-end)))
               (arguments (read-positioning-symbols (current-buffer)))
               (body (save-restriction
                       (narrow-to-region (point) end)
                       (Emil:Support:parse-body))))
          (push (funcall fn name arguments body)
                functions))))
    (nreverse functions)))

(defun Emil:Support:parse-body ()
  (let* ((ppss (syntax-ppss))
         (body (buffer-substring-no-properties (point-min) (point-max)))
         (closer ()))
    (dolist (p (nth 9 ppss))
      (push (cdr (syntax-after p)) closer))
    (read-positioning-symbols (concat body (apply #'string closer)))))

(provide 'Emil/Support)
