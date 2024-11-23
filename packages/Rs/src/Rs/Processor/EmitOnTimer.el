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
  (fn Rs:Publisher:subscribe (self (subscriber (Trait Rs:Subscriber)))
    (when (Struct:get self :subscriber)
      (error "Multiple subscriber not supported"))
    (Struct:set self :subscriber subscriber)
    (Struct:set self :subscription
                (Rs:Publisher:subscribe (Struct:get self :publisher) self))))

(Trait:implement Rs:Subscription Rs:Processor:EmitOnTimer
  (fn Rs:Subscription:request (self (count (integer 0 *)))
    (Rs:Subscription:request (Struct:get self :subscription) count))
  
  (fn Rs:Subscription:cancel (self)
    (Rs:Subscription:cancel (Struct:get self :subscription))))

(Trait:implement Rs:Subscriber Rs:Processor:EmitOnTimer
  (fn Rs:Subscriber:on-subscribe (self (subscription (Trait Rs:Subscription)))
    (Rs:Subscriber:on-subscribe (Struct:get self :subscriber) subscription))
  
  (fn Rs:Subscriber:on-next (self value)
    (Struct:update self :items (-partial #'cons value))
    (Rs:Processor:EmitOnTimer:-ensure-timer self))

  (fn Rs:Subscriber:on-error (self (error error))
    (Rs:Processor:EmitOnTimer:-cancel-timer self)
    (Rs:Subscriber:on-error (Struct:get self :subscriber) error))

  (fn Rs:Subscriber:on-complete (self)
    (Struct:set self :complete? t)
    (Rs:Processor:EmitOnTimer:-ensure-timer self)))

(Struct:implement Rs:Processor:EmitOnTimer
  (fn Rs:Processor:EmitOnTimer:-cancel-timer (self)
    (when-let ((timer (Struct:get self :timer)))
      (cancel-timer timer)
      (Struct:set self :timer nil)))

  (fn Rs:Processor:EmitOnTimer:-ensure-timer (self)
    (unless (timerp (Struct:get self :timer))
      (Struct:set self :timer
        (run-with-timer
         (Struct:get (Struct:get self :config) :delay)
         (Struct:get (Struct:get self :config) :delay)
         #'Rs:Processor:EmitOnTimer:-on-timer
         self))))

  (fn Rs:Processor:EmitOnTimer:-on-timer (self)
    (condition-case error
        (-let* (((&plist :items :config :subscriber :complete?)
                 (Struct:properties self))
                (batch-size (Struct:get config :batch-size))
                ((rest batch)
                 (-split-at (max 0 (- (length items) batch-size)) items)))
          (when batch
            (Struct:set self :items rest)
            (--each (nreverse batch)
              (Rs:Subscriber:on-next subscriber it)))
          (when (and complete? (null rest))
            (Rs:Processor:EmitOnTimer:-cancel-timer self)
            (Rs:Subscriber:on-complete subscriber)))
      (error
       (Rs:Processor:EmitOnTimer:-cancel-timer self))
       (signal (car error) (cdr error)))))

(provide 'Rs/Processor/EmitOnTimer)
