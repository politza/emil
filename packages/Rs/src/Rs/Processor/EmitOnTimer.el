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

(require 'Struct)
(require 'Rs)
(require 'dash)

(Struct:define Rs:Processor:EmitOnTimerConfig
  (batch-size :type (number 1 *))
  (delay :type number :default 1))

(Struct:define Rs:Processor:EmitOnTimer
  (config :type Rs:Processor:EmitOnTimerConfig)
  (publisher :type (Trait Rs:Publisher))
  (subscriber :type (or null (Trait Rs:Subscriber)) :mutable t)
  (subscription :type (or null (Trait Rs:Subscription)) :mutable t)
  (timer :type nil :mutable t)
  (values :type list :mutable t)
  (complete? :type boolean :mutable t))

(Trait:implement Rs:Subscription Rs:Processor:EmitOnTimer
  (fn request (self (count (integer 0 *)))
    (self.subscription.request count))
  
  (fn cancel (self)
    (self.subscription.cancel)))

(Struct:implement Rs:Processor:EmitOnTimer
  (fn -cancel-timer (self)
    (when self.timer
      (cancel-timer self.timer)
      (setf self.timer nil)))

  (fn -ensure-timer (self)
    (unless (timerp self.timer)
      (setf self.timer
            (run-with-timer
             self.config.delay
             self.config.delay
             #'self.-on-timer))))

  (fn -on-timer (self)
    (condition-case nil
        (-let* (((rest batch)
                 (-split-at (max 0 (- (length self.values) self.config.batch-size)) self.values)))
          (when batch
            (setf self.values rest)
            (--each (nreverse batch)
              (self.subscriber.on-next it)))
          (when (and self.complete? (null rest))
            (self.-cancel-timer)
            (self.subscriber.on-complete)))
      (error
       (self.-cancel-timer)))))

(Trait:implement Rs:Subscriber Rs:Processor:EmitOnTimer
  (fn on-subscribe (self (subscription (Trait Rs:Subscription)))
    (self.subscriber.on-subscribe subscription))
  
  (fn on-next (self value)
    (setf self.values (cons value self.values))
    (self.-ensure-timer))

  (fn on-error (self error)
    (self.-cancel-timer)
    (self.subscriber.on-error error))

  (fn on-complete (self)
    (setf self.complete? t)
    (self.-ensure-timer)))

(Trait:implement Rs:Publisher Rs:Processor:EmitOnTimer
  (fn subscribe (self (subscriber (Trait Rs:Subscriber)))
    (when self.subscriber
      (error "Multiple subscriber not supported"))
    (setf self.subscriber subscriber)
    (self.publisher.subscribe self)))

(provide 'Rs/Processor/EmitOnTimer)
