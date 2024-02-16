;; -*- lexical-binding: t -*-

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
