;; -*- lexical-binding: t -*-

(require 'Rs)
(require 'Struct)
(require 'ring)

(Struct:define Rs:Publisher:Submission:Subscription
  "Subscription handling a single subscriber."
  (subscriber :type (Trait Rs:Subscriber))
  (publisher :type Rs:Publisher:Submission)
  (buffer-size :default Rs:default-buffer-size
               :type number)
  (buffer :default (make-ring buffer-size)
          :type ring)
  (request-count :default 0
                 :type (integer 0 *) :mutable t)
  (closed? :type boolean :mutable t)
  (emitting? :type boolean :mutable t))

(Struct:define Rs:Publisher:Submission
  "A publisher where values can be submitted to."
  (subscriptions :type list :mutable t)
  (closed? :type boolean :mutable t))

(Struct:implement Rs:Publisher:Submission:Subscription
  (fn Rs:Publisher:Submission:Subscription:new ((publisher Rs:Publisher:Submission)
                              (subscriber (Trait Rs:Subscriber))
                              &optional
                              (buffer-size (integer 0 *)))
    (Rs:Publisher:Submission:Subscription* publisher subscriber buffer-size))

  (fn Rs:Publisher:Submission:Subscription:emit-some ((self Rs:Publisher:Submission:Subscription))

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

  (fn Rs:Publisher:Submission:Subscription:buffer-full? ((self Rs:Publisher:Submission:Subscription))
    (= (ring-size (Struct:get self :buffer))
       (ring-length (Struct:get self :buffer))))

  (fn Rs:Publisher:Submission:Subscription:close ((self Rs:Publisher:Submission:Subscription))
    (unless (Struct:get self :closed?)
      (Struct:set self :closed? t)
      (Rs:Publisher:Submission:remove
       (Struct:get self :publisher) self)))

  (fn Rs:Publisher:Submission:Subscription:next ((self Rs:Publisher:Submission:Subscription) item)
    (unless (Struct:get self :closed?)
      (when (Rs:Publisher:Submission:Subscription:buffer-full? self)
        (Rs:Publisher:Submission:Subscription:emit-some self))

      (cond
       ((Rs:Publisher:Submission:Subscription:buffer-full? self)
        (Rs:Publisher:Submission:Subscription:close self)
        (Rs:Subscriber:on-error
         (Struct:get self :subscriber)
         (cons 'error :buffer-overflow)))
       (t
        (ring-insert (Struct:get self :buffer) item)
        (Rs:Publisher:Submission:Subscription:emit-some self)))))

  (fn Rs:Publisher:Submission:Subscription:error ((self Rs:Publisher:Submission:Subscription) error)
    (unless (Struct:get self :closed?)
      (Rs:Publisher:Submission:Subscription:close self)
      (Rs:Subscriber:on-error (Struct:get self :subscriber) error)))

  (fn Rs:Publisher:Submission:Subscription:complete ((self Rs:Publisher:Submission:Subscription))
    (unless (Struct:get self :closed?)
      (Rs:Publisher:Submission:Subscription:close self)
      (Rs:Subscriber:on-complete (Struct:get self :subscriber)))))

(Trait:implement Rs:Subscription Rs:Publisher:Submission:Subscription
  (fn Rs:Subscription:request (self (count (integer 0 *)))
    (unless (Struct:get self :closed?)
      (Struct:update self :request-count (-partial #'+ count))
      (Rs:Publisher:Submission:Subscription:emit-some self)))

  (fn Rs:Subscription:cancel (self)
    (Rs:Publisher:Submission:Subscription:close self)))

(Struct:implement Rs:Publisher:Submission
  (fn Rs:Publisher:Submission:remove
    ((self Rs:Publisher:Submission) (subscription Rs:Publisher:Submission:Subscription))
    (Struct:update self :subscriptions (-partial #'remq subscription)))

  (fn Rs:Publisher:Submission:next ((self Rs:Publisher:Submission) item)
    (--each (Struct:get self :subscriptions)
      (Rs:Publisher:Submission:Subscription:next it item)))

  (fn Rs:Publisher:Submission:error ((self Rs:Publisher:Submission)
                                    (error error))
    (let ((subscriptions (Struct:get self :subscriptions)))
      (Struct:set self :subscriptions nil)
      (Struct:set self :closed? t)
      (--each subscriptions
        (Rs:Publisher:Submission:Subscription:error it error))))

  (fn Rs:Publisher:Submission:complete ((self Rs:Publisher:Submission))
    (let ((subscriptions (Struct:get self :subscriptions)))
      (Struct:set self :subscriptions nil)
      (Struct:set self :closed? t)
      (--each subscriptions
        (Rs:Publisher:Submission:Subscription:complete it)))))

(Trait:implement Rs:Publisher Rs:Publisher:Submission
  (fn Rs:Publisher:subscribe (self (subscriber (Trait Rs:Subscriber)))
    (let ((subscription (Rs:Publisher:Submission:Subscription:new self subscriber)))
      (Struct:update self :subscriptions (-partial #'cons subscription))
      (Rs:Subscriber:on-subscribe subscriber subscription))))

(provide 'Rs/Publisher/Submission)
