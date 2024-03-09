;; -*- lexical-binding: t -*-

(require 'Rs)
(require 'Rs/Publisher/Submission)
(require 'Rs/Processor/EmitOnTimer)

(defun Rs:Demo:simple ()
  (interactive)
  (let* ((publisher (Rs:Publisher:Submission))
         (subscription
          (Rs:Publisher:subscribe*
           publisher
           :on-next (lambda (message)
                      (message "On Next: %s" message))
           :on-complete (lambda ()
                          (message "On Complete"))
           :on-error (lambda (error)
                       (message "On Error: %s"
                                (error-message-string error))))))
    (ignore subscription)
    (run-with-timer
     1 nil (lambda nil
             (Rs:Publisher:Submission:next publisher 0)))
    (run-with-timer
     2 nil (lambda nil
             (Rs:Publisher:Submission:next publisher 1)))
    (run-with-timer
     3 nil (lambda nil
             (Rs:Publisher:Submission:complete publisher)))))

(defun Rs:Demo:EmitOnTimer ()
  (interactive)
  (let* ((publisher (Rs:Publisher:Submission))
         (config (Rs:Processor:EmitOnTimerConfig :batch-size 1 :delay 0.25))
         (subscription
          (Rs:Publisher:subscribe*
           (Rs:Processor:EmitOnTimer* publisher config)
           :on-next (lambda (message)
                      (message "%s" message))
           :on-complete (lambda ()
                          (message "Done.")))))
    (ignore subscription)
    (dotimes (n 10)
      (Rs:Publisher:Submission:next publisher (format "%s%%" (* 10 (1+ n)))))
    (Rs:Publisher:Submission:complete publisher)))

(Rs:Demo:EmitOnTimer)
