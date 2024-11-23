;;; Rs.el --- Reactive streams library.             -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Andreas Politz

;; Author: Andreas Politz
;; Keywords: extensions
;; Version: 1.0.0beta1
;; Package-Requires: ((emacs "29.1") (dash "2.19.1") (Struct "1.0.0beta1") (Emil "1.0.0beta1"))

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

(require 'Struct)
(require 'Trait)
(require 'dash)
(require 'cl-macs)

(defconst Rs:default-buffer-size 256)

;; See https://github.com/reactive-streams/reactive-streams-jvm/blob/v1.0.4/README.md#specification
(Trait:define Rs:Subscriber ()
  (fn on-subscribe (self (subscription (Trait Rs:Subscription)) -> Void))
  (fn on-next (self value -> Void))
  (fn on-error (self error -> Void))
  (fn on-complete (self -> Void)))

(Trait:define Rs:Subscription ()
  (fn request (self (count (integer 0 *)) -> Void))
  (fn cancel (self -> Void)))

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
    (setf self.cancelled t)
    (when self.subscription
      (self.subscription.cancel))))

(Trait:implement Rs:Subscriber Rs:PartialSubscriber
  (fn on-subscribe (self (subscription (Trait Rs:Subscription)))
    (if self.cancelled
        (subscription.cancel)
      (setf self.subscription subscription)
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
  (fn subscribe (self (subscriber (Trait Rs:Subscriber)) -> Void))

  (fn subscribe* (self &rest partial-subscriber-properties -> Rs:PartialSubscriber)
    (let ((subscriber (apply #'Rs:PartialSubscriber partial-subscriber-properties)))
      (self.subscribe subscriber)
      subscriber)))

(Trait:define Rs:Processor (Rs:Publisher Rs:Subscriber))

(provide 'Rs)
;;; Rs.el ends here
