;; -*- lexical-binding: t -*-

(require 'Rs)
(require 'Struct)

(defmacro Struct:implement (type &rest body)
  (declare (indent 1)))

(defmacro fn (type &rest body)
  (declare (indent defun)))

(font-lock-add-keywords
 nil
 '(("(\\(fn\\)[ \t]+\\(\\(?:\\sw\\|\\s_\\)+\\)?"
    (1 font-lock-keyword-face)
    (2 font-lock-function-name-face nil t))))

(font-lock-add-keywords
 nil
 '(("\\_<\\(Self\\\)\\_>" (1 font-lock-type-face))))

(font-lock-add-keywords
 nil
 '(("\\_<\\(from\\|use\\\)\\_>" (1 font-lock-keyword-face))))

(Struct:define Rs:Sp:Subscription
  "Subscription handling a single subscriber."
  (subscriber :type (Trait Rs:Subscriber) :required t :read-only t)
  (publisher :type Rs:SubmissionPublisher :required t :read-only t)
  (buffer-size :default-value Rs:default-buffer-size
               :type number :required t :read-only t)
  (buffer :default-value (make-ring buffer-size)
          :type ring :read-only t :required t)
  (request-count :default-value 0
                 :type (integer 0 *) :required t)
  (closed? :type boolean)
  (emitting? :type boolean))

(Struct:implement Rs:Sp:Subscription
  (use ((on-next on-error on-complete) from Rs:Subscription)
       ((remove) from Rs:SubmissionPublisher))
  
  (fn new ((publisher Rs:SubmissionPublisher)
           (subscriber (Trait Rs:Subscriber))
           &optional
           (buffer-size (integer 0 *)))
    (Rs:Sp:Subscription* publisher subscriber buffer-size))

  (fn emit-some ((self Self))
    (unless (get self :emitting?)
      (set self :emitting? t)
      (unwind-protect 
          (while (and (not (get self :closed?))
                      (not (ring-empty-p (get self :buffer)))
                      (> (get self :request-count) 0))
            (let ((item (ring-remove (get self :buffer))))
              (update self :request-count #'1-)
              (on-next (get self :subscriber) item)))
        (set self :emitting? nil))))

  (fn buffer-full? ((self Self))
    (= (ring-size (get self :buffer))
       (ring-length (get self :buffer))))

  (fn close ((self Self))
    (unless (get self :closed?)
      (set self :closed? t)
      (remove (get self :publisher) self)))

  (fn next ((self Self) item)
    (unless (get self :closed?)
      (when (buffer-full? self)
        (emit-some self))

      (cond
       ((buffer-full? self)
        (close self)
        (on-error
         (get self :subscriber)
         (cons 'error :buffer-overflow)))
       (t
        (ring-insert (get self :buffer) item)
        (emit-some self)))))

  (fn error ((self Self) error)
    (unless (get self :closed?)
      (close self)
      (on-error (get self :subscriber) error)))

  (fn complete ((self Self) error)
    (unless (get self :closed?)
      (close self)
      (on-complete (get self :subscriber)))))
