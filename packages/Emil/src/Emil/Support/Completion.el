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

(require 'Struct/Impl)
(require 'Trait)
(require 'Emil/Support)
(require 'Emil/Syntax)
(require 'Emil/Form)
(require 'Emil)

(defconst Emil:Support:annotation-property 'Emil:Support:annotation-property)

(defun Emil:Support:completion-at-point ()
  (-when-let* (((start . end) (unless (nth 8 (syntax-ppss))
                                (bounds-of-thing-at-point 'symbol)))
               (structure (Emil:Support:parse))
               (syntax-disabled?
                (not (plist-get (Struct:get structure :properties) :disable-syntax)))
               (function-name (Emil:Support:current-function))
               (symbols-with-pos-enabled t)
               (function (--find (eq function-name (Struct:get it :name))
                                 (Struct:get structure :functions)))
               (completion-table
                (completion-table-dynamic
                 (lambda (_)
                   (Emil:Support:completions structure function start)))))
    (list start end completion-table
          :company-kind #'Emil:Support:completion-kind)))

(defun Emil:Support:completions (structure function position)
  (Emil:Support:with-declarations structure
    (let* ((symbols-with-pos-enabled t)
           (syntax-env
            (Emil:Syntax
             :env
             (Emil:Env:Alist :variables (Emil:Syntax:Function:bindings function))))
           (form (Emil:transform `(progn ,@(Struct:get function :body))
                                 syntax-env :no-error)))
      (cl-labels
          ((complete-expression (value local-env &optional function-context?)
             (condition-case nil
                 (Emil:Support:complete-symbol
                  syntax-env value (Emil:Env:Alist :variables local-env)
                  function-context?)
               (Emil:type-error nil)))
           (visit (form &optional local-env)
             (pcase form
               ((Struct Emil:Form:Invalid form)
                (when-let ((completable (Emil:Support:completable-find form position)))
                  (complete-expression (car completable) local-env (cdr completable))))
               ((and (Struct Emil:Form:Function value)
                     (let (Struct Emil:Form:Atom :value symbol) value)
                     (guard (Emil:Support:form-at-position? symbol position)))
                (complete-expression symbol local-env t))
               ((and (Struct Emil:Form:Atom value)
                     (guard (Emil:Support:form-at-position? value position)))
                (complete-expression value local-env))
               (_
                (Emil:Form:each-child form #'visit local-env)))))
        (visit form)))))

(defun Emil:Support:form-at-position? (form position)
  (and (symbol-with-pos-p form)
       (eq position (symbol-with-pos-pos form))))

(defun Emil:Support:completable-find (form position)
  (pcase form
    ((guard (Emil:Support:form-at-position? form position))
     (cons form nil))
    ((and (or `(function ,value)
              `(,value . ,_))
          (guard (Emil:Support:form-at-position? value position)))
     (cons value t))
    ((guard (proper-list-p form))
     (--some (Emil:Support:completable-find it position)
             form))))

(defun Emil:Support:complete-symbol (syntax-env symbol
                                                &optional
                                                local-env function-context?)
  (let ((methods (when function-context?
                   (-when-let* (((suffix . type)
                                 (Emil:Syntax:resolve-expression syntax-env symbol local-env))
                                (prefix (substring (symbol-name symbol)
                                                   0 (- (length (symbol-name symbol))
                                                        (length (symbol-name suffix))))))
                     (--map (Emil:Support:annotation-put
                             (concat prefix (symbol-name (Struct:get it :name)))
                             it)
                            (Emil:Syntax:type-functions type)))))
        (properties
         (-when-let* (((suffix . type)
                       (Emil:Syntax:resolve-expression syntax-env symbol local-env))
                      (struct (and (Emil:Type:Basic? type)
                                   (Struct:Type:get (Struct:get type :name))))
                      (prefix (substring (symbol-name symbol)
                                         0 (- (length (symbol-name symbol))
                                              (length (symbol-name suffix))))))
           (--map (Emil:Support:annotation-put
                   (concat prefix (symbol-name (Struct:get (cdr it) :name)))
                   (cdr it))
                  (Struct:get struct :properties)))))
    (append methods properties)))

(defun Emil:Support:annotation-put (string annotation)
  (put-text-property 0 1 Emil:Support:annotation-property annotation string)
  string)

(defun Emil:Support:annotation-get (string)
  (and (stringp string)
       (get-text-property 0 Emil:Support:annotation-property string)))

(defun Emil:Support:completion-kind (candidate)
  (when-let (struct (Emil:Support:annotation-get candidate))
    (pcase struct
      ((Struct Struct:Function)
       'method)
      ((Struct Struct:Property)
       'field))))

(provide 'Emil/Support/Completion)
