;; -*- lexical-binding: t -*-

(require 'Struct)
(require 'Struct/Impl)
(require 'pcase)
(require 'bytecomp)

(Struct:define Emil:Message
  (type
   "The type of this message. Either one of `:info', `:warning' or `:error'"
   :type (member :debug :warning :error))
  (content
   "The content of the message."
   :type string)
  (error-condition
   "The error-condition, only set if type is `error'."
   :type symbol :default (when (eq type :error)
                           (error "error-condition not provided")))
  (form
   "The form related to this message.")
  (position
   "The buffer-position related to this message."
   :type (or null integer)))

(Struct:implement Emil:Message
  :disable-syntax t
  (fn Emil:Message:format(self)
    (format "%s: %s"
            (pcase-exhaustive (Struct:get self :type)
              (:info "Info")
              (:warning "Warning")
              (:error "Error"))
            (Struct:get self :content)))

  (fn Emil:Message:byte-compile-log (self)
    (setq byte-compiler-error-flag
          (or byte-compiler-error-flag
              (eq :error (Struct:get self :type))))
    (let* ((form (Struct:get self :form))
           ;; Trick the byte-compile into outputting proper error positions.
           (byte-compile-form-stack
            (if (or form (symbol-with-pos-p form))
                (list form)
              byte-compile-form-stack)))
      (funcall byte-compile-log-warning-function
               (Struct:get self :content)
               nil
               nil
               (Struct:get self :type)))))

(provide 'Emil/Message)
