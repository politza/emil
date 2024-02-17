;; -*- lexical-binding: t -*-

(require 'Rs)
(require 'Struct)
(require 'ring)

(Struct:define Rs:Sp:Subscription
  "Subscription handling a single subscriber."
  (subscriber :type (Trait Rs:Subscriber))
  (publisher :type Rs:SubmissionPublisher)
  (buffer-size :default Rs:default-buffer-size
               :type number)
  (buffer :default (make-ring buffer-size)
          :type ring)
  (request-count :default 0
                 :type (integer 0 *) :mutable t)
  (closed? :type boolean :mutable t)
  (emitting? :type boolean :mutable t))

(Struct:define Rs:SubmissionPublisher
  "A publisher where values can be submitted to."
  (subscriptions :type list :mutable t)
  (closed? :type boolean :mutable t))

(Struct:defun Rs:Sp:Subscription:new ((publisher Rs:SubmissionPublisher)
                                      (subscriber (Trait Rs:Subscriber))
                                      &optional
                                      (buffer-size (integer 0 *)))
  (Rs:Sp:Subscription* publisher subscriber buffer-size))

(Struct:defun Rs:Sp:Subscription:emit-some ((self Rs:Sp:Subscription))

  (unless (Struct:get self :emitting?)
    (Struct:set self :emitting? t)
    (unwind-protect
        (while (and (not (Struct:get self :closed?))
                    (not (ring-empty-p (Struct:get self :buffer)))
                    (> (Struct:get self :request-count) 0))
          (let ((item (ring-remove (Struct:get self :buffer))))
            (Struct:update self :request-count #'1-)
            (Rs:Subscriber:on-next (Struct:get self :subscriber) item)))
      (Struct:set self :emitting? nil))))

(Struct:defun Rs:Sp:Subscription:buffer-full? ((self Rs:Sp:Subscription))
  (= (ring-size (Struct:get self :buffer))
     (ring-length (Struct:get self :buffer))))

(Struct:defun Rs:Sp:Subscription:close ((self Rs:Sp:Subscription))
  (unless (Struct:get self :closed?)
    (Struct:set self :closed? t)
    (Rs:SubmissionPublisher:remove
     (Struct:get self :publisher) self)))

(Struct:defun Rs:Sp:Subscription:next ((self Rs:Sp:Subscription) item)
  (unless (Struct:get self :closed?)
    (when (Rs:Sp:Subscription:buffer-full? self)
      (Rs:Sp:Subscription:emit-some self))

    (cond
     ((Rs:Sp:Subscription:buffer-full? self)
      (Rs:Sp:Subscription:close self)
      (Rs:Subscriber:on-error
       (Struct:get self :subscriber)
       (cons 'error :buffer-overflow)))
     (t
      (ring-insert (Struct:get self :buffer) item)
      (Rs:Sp:Subscription:emit-some self)))))

(Struct:defun Rs:Sp:Subscription:error ((self Rs:Sp:Subscription) error)
  (unless (Struct:get self :closed?)
    (Rs:Sp:Subscription:close self)
    (Rs:Subscriber:on-error (Struct:get self :subscriber) error)))

(Struct:defun Rs:Sp:Subscription:complete ((self Rs:Sp:Subscription))
  (unless (Struct:get self :closed?)
    (Rs:Sp:Subscription:close self)
    (Rs:Subscriber:on-complete (Struct:get self :subscriber))))

(Trait:implement Rs:Subscription Rs:Sp:Subscription
  (defmethod Rs:Subscription:request (self (count (integer 0 *)))
    (unless (Struct:get self :closed?)
      (Struct:update self :request-count (-partial #'+ count))
      (Rs:Sp:Subscription:emit-some self)))

  (defmethod Rs:Subscription:cancel (self)
    (Rs:Sp:Subscription:close self)))

(Struct:defun Rs:SubmissionPublisher:remove
  ((self Rs:SubmissionPublisher) (subscription Rs:Sp:Subscription))
  (Struct:update self :subscriptions (-partial #'remq subscription)))

(Struct:defun Rs:SubmissionPublisher:next ((self Rs:SubmissionPublisher) item)
  (--each (Struct:get self :subscriptions)
    (Rs:Sp:Subscription:next it item)))

(Struct:defun Rs:SubmissionPublisher:error ((self Rs:SubmissionPublisher)
                                            (error error))
  (let ((subscriptions (Struct:get self :subscriptions)))
    (Struct:set self :subscriptions nil)
    (Struct:set self :closed? t)
    (--each subscriptions
      (Rs:Sp:Subscription:error it error))))

(Struct:defun Rs:SubmissionPublisher:complete ((self Rs:SubmissionPublisher))
  (let ((subscriptions (Struct:get self :subscriptions)))
    (Struct:set self :subscriptions nil)
    (Struct:set self :closed? t)
    (--each subscriptions
      (Rs:Sp:Subscription:complete it))))

(Trait:implement Rs:Publisher Rs:SubmissionPublisher
  (defmethod Rs:Publisher:subscribe (self (subscriber (Trait Rs:Subscriber)))
    (let ((subscription (Rs:Sp:Subscription:new self subscriber)))
      (Struct:update self :subscriptions (-partial #'cons subscription))
      (Rs:Subscriber:on-subscribe subscriber subscription))))

(provide 'Rs/SubmissionPublisher)
