;; -*- lexical-binding: t -*-

(require 'Rs)
(require 'Struct)

(Struct:define Rs:SubmissionPublisher
  "A publisher where values can be submitted to."
  (subscriber :type list))

(Trait:implement Rs:Publisher Rs:SubmissionPublisher
  (defmethod Rs:Publisher:subscribe (self (subscriber (Trait Rs:Subscriber)))))

(provide 'Rs/publisher/SubmissionPublisher)
