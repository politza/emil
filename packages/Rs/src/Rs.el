;;; Rs.el --- Reactive streams library.             -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Andreas Politz

;; Author: Andreas Politz <mail@andreas-politz.de>
;; Keywords: extensions
;; Version: 1.0.0beta1
;; Package-Requires: ((emacs "29.1") (dash "2.19.1") (Struct "1.0.0beta1") (Commons "1.0.0beta1"))

(require 'Struct)
(require 'Trait)
(require 'dash)
(require 'cl-macs)

(cl-deftype error ()
  `(and cons
        (satisfies (lambda (value)
                     (and (symbolp (car value))
                          (get (car value) 'error-conditions))))))

;; See https://github.com/reactive-streams/reactive-streams-jvm/blob/v1.0.4/README.md#specification
(Trait:define Rs:Subscriber ()
  (defmethod Rs:Subscriber:on-subscribe (self (subscription (Trait Rs:Subscription))))
  (defmethod Rs:Subscriber:on-next (self value))
  (defmethod Rs:Subscriber:on-error (self (error error)))
  (defmethod Rs:Subscriber:on-complete (self)))

(Struct:define Rs:PartialSubscriber
  "A subscriber which is only interested in some events."
  (on-subscribe :type function)
  (on-next :type function)
  (on-error :type function)
  (on-complete :type function)
  (subscription :type (Trait Rs:Subscription))
  (cancelled :type boolean))

(Struct:defun Rs:PartialSubscriber:cancel ((self Rs:PartialSubscriber))
  (Struct:set self :cancelled t)
  (when-let (subscription (Struct:get self :subscription))
    (Rs:Subscription:cancel subscription)))

(Trait:implement Rs:Subscriber Rs:PartialSubscriber
  (defmethod Rs:Subscriber:on-subscribe (self (subscription (Trait Rs:Subscription)))
    (if (Struct:get self :cancelled)
        (Rs:Subscription:cancel subscription)
      (Struct:set self :subscription subscription)
      (if-let (on-subscribe (Struct:get self :on-subscribe))
          (funcall on-subscribe subscription)
        (Rs:Subscription:request subscription most-positive-fixnum))))

  (defmethod Rs:Subscriber:on-next (self value)
    (unless (Struct:get self :cancelled)
      (when-let (on-next (Struct:get self :on-next))
        (funcall on-next value))))

  (defmethod Rs:Subscriber:on-error (self (error error))
    (unless (Struct:get self :cancelled)
      (if-let (on-error (Struct:get self :on-error))
          (funcall on-error error)
        (signal (car error) (cdr error)))))

  (defmethod Rs:Subscriber:on-complete (self)
    (unless (Struct:get self :cancelled)
      (when-let (on-complete (Struct:get self :on-complete))
        (funcall on-complete)))))

(Trait:define Rs:Publisher ()
  (defmethod Rs:Publisher:subscribe (self (subscriber (Trait Rs:Subscriber))))

  (defmethod Rs:Publisher:subscribe* (self &struct (subscriber Rs:PartialSubscriber))
    (Rs:Publisher:subscribe self subscriber)
    subscriber))

(Trait:define Rs:Subscription ()
  (defmethod Rs:Subscription:request (self (amount (integer 1 *))))
  (defmethod Rs:Subscription:cancel (self)))

(Trait:define Rs:Processor (Rs:Publisher Rs:Subscriber))

(provide 'Rs)
;;; Rs.el ends here
