;; -*- lexical-binding: t -*-

(require 'Rs)
(require 'Struct)

(Struct:define Rs:SubmissionPublisher:Subscription
  "Subscription handling a single subscriber."
  (subscriber :type (Trait Rs:Subscriber) :required t :read-only t)
  (requested :default-value 0 :type (integer 0 *) :required t))

(Trait:implement Rs:Subscription Rs:SubmissionPublisher:Subscription
  (defmethod Rs:Subscription:request (self (amount (integer 1 *))))
  (defmethod Rs:Subscription:cancel (self)))

(Struct:define Rs:SubmissionPublisher
  "A publisher where values can be submitted to."
  (subscriptions :type list))

(Trait:implement Rs:Publisher Rs:SubmissionPublisher
  (defmethod Rs:Publisher:subscribe (self (subscriber (Trait Rs:Subscriber)))))

(provide 'Rs/SubmissionPublisher)
