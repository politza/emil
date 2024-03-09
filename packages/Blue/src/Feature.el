;;; -*- lexical-binding: t; -*-

(require 'dash)

(defvar Feature:current-packages nil
  "A list of packages used by some feature.

This variable is `let'-bound around loading a feature.")

(defvar Feature:list nil
  "A list of registered features.")

(defmacro Feature:define (name documentation &rest body)
  "Defines a new feature.

\(FN NAME DOCUMENTATION [PROPERTIES] ACTIVATION-FORMS\)"
  (unless (symbolp name)
    (error "Name should be a symbol: %s" name))
  (unless (stringp documentation)
    (error "Documentation should be a string: %s" documentation))
  (when (and body (keywordp (car body)))
    (error "Properties currently not supported: %s" (car body)))
  `(Feature:register (list ',name
                     :activate (lambda nil ,@body)
                     :documentation ,documentation)))

(defun Feature:register (feature)
  "Registers a new FEATURE."
  (setq Feature:list
        (cons feature
              (--remove (eq (car it) (car feature))
                        Feature:list))))

(provide 'Feature)
;;; Feature.el ends here
