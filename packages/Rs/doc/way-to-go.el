;; -*- lexical-binding: t -*-

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
  (items :type list :mutable t)
  (complete? :type boolean :mutable t))

(Trait:implement Rs:Publisher Rs:Processor:EmitOnTimer
  (fn subscribe (self (subscriber (Trait Rs:Subscriber)))
    (when self.subscriber
      (error "Multiple subscriber not supported"))
    (setf self.subscriber subscriber)
    (setf self.subscription
          (self.publisher.subscribe self))))

(Trait:implement Rs:Subscription Rs:Processor:EmitOnTimer
  (fn request (self (count (integer 0 *)))
    (request self.subscription count))
  
  (fn cancel (self)
    (cancel self.subscription)))

(Trait:implement Rs:Subscriber Rs:Processor:EmitOnTimer
  (fn on-subscribe (self (subscription (Trait Rs:Subscription)))
    (on-subscribe self.subscriber subscription))
  
  (fn on-next (self value)
    (setf self.items (cons value self.items))
    (-ensure-timer self))

  (fn on-error (self (error error))
    (self.cancel-timer)
    (on-error self.subscriber error))

  (fn on-complete (self)
    (setf self.complete? t)
    (-ensure-timer self)))

(Struct:implement Rs:Processor:EmitOnTimer
  (fn -cancel-timer (self)
    (when-let ((timer self.timer))
      (cancel-timer timer)
      (setf self.timer nil)))

  (fn -ensure-timer (self)
    (unless (timerp self.timer)
      (setf self.timer
            (run-with-timer
             self.config.delay
             self.config.delay
             #'self.-on-timer ;; => (-partial #'Rs:Processor:EmitOnTimer self)
             ))))

  (fn -on-timer (self)
    (condition-case error
        (-let* (((&plist :items :config :subscriber :complete?)
                 (Struct:properties self))
                (batch-size config.batch-size)
                ((rest batch)
                 (-split-at (max 0 (- (length items) batch-size)) items)))
          (when batch
            (setf self.items rest)
            (--each (nreverse batch)
              (subscriber.on-next it)))
          (when (and complete? (null rest))
            (-cancel-timer self)
            (subscriber.on-complete)))
      (error
       (self.-cancel-timer))
       (signal (car error) (cdr error)))))

(provide 'Rs/Processor/EmitOnTimer)
