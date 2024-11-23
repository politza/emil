;;; Rs.el --- Reactive streams library.             -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Andreas Politz

;; Author: Andreas Politz <mail@andreas-politz.de>
;; Keywords: extensions
;; Version: 1.0.0beta1
;; Package-Requires: ((emacs "29.1") (dash "2.19.1") (Struct "1.0.0beta1") (Emil "1.0.0beta1"))

(require 'Struct)
(require 'Trait)
(require 'dash)
(require 'cl-macs)

(defconst Rs:default-buffer-size 256)

;; See https://github.com/reactive-streams/reactive-streams-jvm/blob/v1.0.4/README.md#specification
(Trait:define Rs:Subscriber ()
  (fn on-subscribe (self (subscription (Trait Rs:Subscription))))
  (fn on-next (self value))
  (fn on-error (self error))
  (fn on-complete (self)))

(Trait:define Rs:Subscription ()
  (fn request (self (count (integer 0 *))))
  (fn cancel (self)))

(Struct:define Rs:PartialSubscriber
  "A subscriber which is only interested in some events."
  (on-subscribe :type (or null function))
  (on-next :type (or null function))
  (on-error :type (or null function))
  (on-complete :type (or null function))
  (subscription :type (or null (Trait Rs:Subscription)) :mutable t)
  (cancelled :type boolean :mutable t))

(Struct:implement Rs:PartialSubscriber
  (fn cancel (self)
    (Struct:set self :cancelled t)
    (when self.subscription
      (self.subscription.cancel))))

(Trait:implement Rs:Subscriber Rs:PartialSubscriber
  (fn on-subscribe (self (subscription (Trait Rs:Subscription)))
    (if self.cancelled
        (subscription.cancel)
      (Struct:set self :subscription subscription)
      (if self.on-subscribe
          (funcall self.on-subscribe subscription)
        (subscription.request most-positive-fixnum))))

  (fn on-next (self value)
    (when (and (not self.cancelled)
               self.on-next)
      (funcall self.on-next value)))

  (fn on-error (self error)
    (unless self.cancelled
      (if self.on-error
          (funcall self.on-error error)
        (signal (car error) (cdr error)))))

  (fn on-complete (self)
    (unless self.cancelled
      (when self.on-complete
        (funcall self.on-complete)))))

(Trait:define Rs:Publisher ()
  (fn subscribe (self (subscriber (Trait Rs:Subscriber))))

  (fn subscribe* (self &rest partial-subscriber-properties)
    (let ((subscriber (Rs:PartialSubscriber*
                       ,@partial-subscriber-properties)))
      (self.subscribe subscriber)
      subscriber)))

(Trait:define Rs:Processor (Rs:Publisher Rs:Subscriber))

(provide 'Rs)
;;; Rs.el ends here
