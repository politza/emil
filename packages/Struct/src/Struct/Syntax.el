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

(require 'Commons)

(defun Struct:Syntax:expand-properties (properties)
  "Expands shorthand and spread-syntax in PROPERTIES."
  (let ((expanded-properties nil))
    (while properties
      (let ((argument (pop properties)))
        (cond
         ((and (keywordp argument) properties)
          ;; Keyword-value pair.
          (push `(list ,argument ,(pop properties))
                expanded-properties))
         ((and (eq '\,@ (car-safe argument))
               (= 2 (length argument)))
          ;; Spread syntax.
          (push `(Struct:properties ,(nth 1 argument))
                expanded-properties))
         ((and (symbolp argument) (not (keywordp argument)))
          ;; Shorthand syntax.
          (push `(list ,(Commons:symbol-to-keyword argument)
                       ,argument)
                expanded-properties))
         (t
          ;; Probably an error later on.
          (push `(list ,argument)
                expanded-properties)))))
    `(apply (function append)
            (list ,@(nreverse expanded-properties)))))

(provide 'Struct/Syntax)
