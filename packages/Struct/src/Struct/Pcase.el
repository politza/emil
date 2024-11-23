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

(require 'pcase)
(require 'dash)
(require 'Commons)
(require 'Struct/Primitives)

(pcase-defmacro Struct (name &rest properties)
  "Match against struct values.

Binds named properties to their corresponding values."
  (let ((properties (Struct:Pcase:-normalize-properties properties)))
    (--each properties
      (unless (Struct:Type:get-property
               (Struct:Type:get name :ensure)
               (car it))
        (error "Property is not a member of struct %s: %s" name (car it))))
    (cons
     'and
     (cons
      (list '\` (cons name (list `\, '_)))
      (mapcar (-lambda ((property . symbol))
                `(app (pcase--flip Struct:unsafe-get ,property) ,symbol))
              properties)))))

(defun Struct:Pcase:-normalize-properties (properties)
  (let ((normalized nil))
    (while properties
      (let ((property (pop properties)))
        (cond
         ((and (symbolp property)
               (not (keywordp property)))
          (push (cons (Commons:symbol-to-keyword property) property)
                normalized))
         ((and (keywordp property)
               properties
               (symbolp (car properties))
               (not (keywordp (car properties))))
          (push (cons property (pop properties))
                normalized))
         ((keywordp property)
          (error "Property should be followed by a symbol to bind to: %s" property))
         (t
          (error "Property should be a symbol: %s" property)))))
    normalized))

(provide 'Struct/Pcase)
