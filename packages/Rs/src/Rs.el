;;; Rs.el --- Reactive streams library.             -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Andreas Politz

;; Author: Andreas Politz <mail@andreas-politz.de>
;; Keywords: extensions
;; Version: 1.0.0beta1
;; Package-Requires: ((emacs "29.1") (dash "2.19.1") (Struct "1.0.0beta1"))

(require 'Struct)
(require 'Trait)
(require 'dash)

;; See https://github.com/reactive-streams/reactive-streams-jvm/blob/v1.0.4/README.md#specification 

(Trait:define Rs:Publisher ()
  (defmethod Rs:Publisher:subscribe (self (subscriber (Trait Rs:Subscriber)))))

(Trait:define Rs:Subscriber ()
  (defmethod Rs:Subscriber:on-subscribe (self (subscription (Trait Rs:Subscription))))
  (defmethod Rs:Subscriber:on-next (self value))
  (defmethod Rs:Subscriber:on-error (self (error Rs:error)))
  (defmethod Rs:Subscriber:on-complete (self)))

(Trait:define Rs:Subscription ()
  (defmethod Rs:Subscription:request (self (amount (integer 1 *))))
  (defmethod Rs:Subscription:cancel (self)))

(Trait:define Rs:Processor (Rs:Publisher Rs:Subscriber))

(cl-deftype Rs:error ()
  `(and cons
        (satisfies (lambda (value)
                     (and (symbolp (car value))
                          (get (car value) 'error-conditions))))))

(provide 'Rs)
;;; Rs.el ends here
