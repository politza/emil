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
  (fn new ((publisher Rs:Publisher:Submission)
           (subscriber (Trait Rs:Subscriber))
           &optional
           (buffer-size (integer 0 *)))
    (Rs:Publisher:Submission:Subscription* publisher subscriber buffer-size))

  (fn emit-some (self)
    (unless self.emitting?
      (setf self.emitting? t)
      (unwind-protect
          (while (and (not self.closed?)
                      (not (ring-empty-p self.buffer))
                      (> self.request-count 0))
            (let ((item (ring-remove self.buffer)))
              (setf self.request-count (1- self.request-count))
              (self.subscriber.on-next item)))
        (setf self.emitting? nil))))

  (fn buffer-full? (self)
    (= (ring-size self.buffer)
       (ring-length self.buffer)))

  (fn close (self)
    (unless self.closed?
      (setf self.closed? t)
      (Rs:Publisher:Submission:remove self.publisher self)))

  (fn next (self item)
    (unless self.closed?
      (when (self.buffer-full?)
        (self.emit-some))
      (cond
       ((self.buffer-full?)
        (self.close)
        (self.subscriber.on-error (cons 'error :buffer-overflow)))
       (t
        (ring-insert self.buffer item)
        (self.emit-some)))))

  (fn error (self error)
    (unless self.closed?
      (self.close)
      (self.subscriber.on-error error)))

  (fn complete (self)
    (unless self.closed?
      (self.close)
      (self.subscriber.on-complete))))

(Trait:implement Rs:Subscription Rs:Publisher:Submission:Subscription
  (fn request (self (count (integer 0 *)))
    (unless self.closed?
      (setf self.request-count (+ self.request-count count))
      (self.emit-some)))

  (fn cancel (self)
    (self.close)))

(Struct:implement Rs:Publisher:Submission
  (fn remove ((self Rs:Publisher:Submission)
              (subscription Rs:Publisher:Submission:Subscription))
    (setf self.subscriptions (remq subscription self.subscriptions)))

  (fn next ((self Rs:Publisher:Submission) item)
    (--each self.subscriptions
      (Rs:Publisher:Submission:Subscription:next it item)))

  (fn error ((self Rs:Publisher:Submission) error)
    (let ((subscriptions self.subscriptions))
      (setf self.subscriptions nil)
      (setf self.closed? t)
      (--each subscriptions
        (Rs:Publisher:Submission:Subscription:error it error))))

  (fn complete ((self Rs:Publisher:Submission))
    (let ((subscriptions self.subscriptions))
      (setf self.subscriptions nil)
      (setf self.closed? t)
      (--each subscriptions
        (Rs:Publisher:Submission:Subscription:complete it)))))

(Trait:implement Rs:Publisher Rs:Publisher:Submission
  (fn subscribe (self (subscriber (Trait Rs:Subscriber)))
    (let ((subscription (Rs:Publisher:Submission:Subscription:new self subscriber)))
      (setf self.subscriptions (cons subscription self.subscriptions))
      (subscriber.on-subscribe subscription))))

(provide 'Rs/Publisher/Submission)
