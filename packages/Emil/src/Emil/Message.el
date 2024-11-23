;; -*- lexical-binding: t -*-

(require 'Struct)
(require 'pcase)

(Struct:define Emil:Message
  (type
   "The type of this message. Either one of `:info', `:warning' or `:error'"
   :type (member :info :warning :error))
  (content
   "The content of the message."
   :type string)
  (form
   "An optional form associated with this message."))

(Struct:implement Emil:Message
  (fn Emil:Message:format(self)
    (format "%s: %s%s"
            (pcase-exhaustive (Struct:get self :type)
              (:info "Info")
              (:warning "Warning")
              (:error "Error"))
            (Struct:get self :content)
            (if-let (form (Struct:get self :form))
                (let ((print-circle t)
                      (print-escape-newlines t)
                      (print-length 8)
                      (print-level 3))
                  (format " in %S" form))
              ""))))

(provide 'Emil/Message)
