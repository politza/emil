;; -*- lexical-binding: t -*-

(require 'Transformer)
(require 'Trait)
(require 'trace)

(defun Emil:Trace:functions ()
  "Returns a list of `Transformer' methods."
  (append (-map #'car (Struct:get (Trait:get 'Transformer) :methods))
          '(Emil:infer Emil:check Emil:subtype Emil:infer-do
                       Emil:infer-application Emil:Context:hole
                       Emil:Context:double-hole
                       Emil:Context:lookup-binding
                       Emil:Context:lookup-solved
                       Emil:Context:drop-until-after
                       Emil:Context:well-formed? Emil:Context:resolve
                       Emil:Context:concat Emil:Context:member?
                       Emil:Environment:lookup)))

(define-minor-mode Emil:Trace:mode
  "Trace methods calls."
  :global t
  (cond
   (Emil:Trace:mode
    (--each (Emil:Trace:functions)
      (trace-function-background it))
    (advice-add #'trace-entry-message
                :filter-args #'Emil:Trace:trace-entry-message-filter))
   (t
    (advice-remove #'trace-entry-message
                   #'Emil:Trace:trace-entry-message-filter)
    (--each (Emil:Trace:functions)
      (untrace-function it)))))

(defun Emil:Trace:trace-entry-message-filter (arguments)
  "Filter some arguments before printing the trace message."
  (list (nth 0 arguments) (nth 1 arguments)
        (--filter (not (eq 'Emil (car-safe it))) (nth 2 arguments))
        (nth 3 arguments)))
