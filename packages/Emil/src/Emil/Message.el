;; -*- lexical-binding: t -*-

(Struct:define Emil:Message
  (type
   "The type of this message. Either one of `:info', `:warning' or `:error'"
   :type (member :info :warning :error))
  (content
   "The content of the message."
   :type string)
  (form
   "An optional form associated with this message."))

(provide 'Emil/Message)
