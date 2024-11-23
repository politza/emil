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

(defconst Rs:default-buffer-size 256)

;; See https://github.com/reactive-streams/reactive-streams-jvm/blob/v1.0.4/README.md#specification
(Trait:define Rs:Subscriber ()
  :disable-syntax t
  (fn Rs:Subscriber:on-subscribe (self (subscription (Trait Rs:Subscription))))
  (fn Rs:Subscriber:on-next (self value))
  (fn Rs:Subscriber:on-error (self (error error)))
  (fn Rs:Subscriber:on-complete (self)))

(Struct:define Rs:PartialSubscriber
  "A subscriber which is only interested in some events."
  (on-subscribe :type (or null function))
  (on-next :type (or null function))
  (on-error :type (or null function))
  (on-complete :type (or null function))
  (subscription :type (or null (Trait Rs:Subscription)) :mutable t)
  (cancelled :type boolean :mutable t))

(defun Rs:PartialSubscriber:cancel (self)
  (Struct:set self :cancelled t)
  (when-let (subscription (Struct:get self :subscription))
    (Rs:Subscription:cancel subscription)))

(Trait:implement Rs:Subscriber Rs:PartialSubscriber
  (fn Rs:Subscriber:on-subscribe (self (subscription (Trait Rs:Subscription)))
    (if (Struct:get self :cancelled)
        (Rs:Subscription:cancel subscription)
      (Struct:set self :subscription subscription)
      (if-let (on-subscribe (Struct:get self :on-subscribe))
          (funcall on-subscribe subscription)
        (Rs:Subscription:request subscription most-positive-fixnum))))

  (fn Rs:Subscriber:on-next (self value)
    (unless (Struct:get self :cancelled)
      (when-let (on-next (Struct:get self :on-next))
        (funcall on-next value))))

  (fn Rs:Subscriber:on-error (self (error error))
    (unless (Struct:get self :cancelled)
      (if-let (on-error (Struct:get self :on-error))
          (funcall on-error error)
        (signal (car error) (cdr error)))))

  (fn Rs:Subscriber:on-complete (self)
    (unless (Struct:get self :cancelled)
      (when-let (on-complete (Struct:get self :on-complete))
        (funcall on-complete)))))

(Trait:define Rs:Publisher ()
  :disable-syntax t
  (fn Rs:Publisher:subscribe (self (subscriber (Trait Rs:Subscriber))))

  (fn Rs:Publisher:subscribe* (self &struct (subscriber Rs:PartialSubscriber))
    (Rs:Publisher:subscribe self subscriber)
    subscriber))

(Trait:define Rs:Subscription ()
  :disable-syntax t
  (fn Rs:Subscription:request (self (count (integer 0 *))))
  (fn Rs:Subscription:cancel (self)))

(Trait:define Rs:Processor (Rs:Publisher Rs:Subscriber)
  :disable-syntax t)

(provide 'Rs)
;;; Rs.el ends here
