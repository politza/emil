;;; Struct.el --- A struct data-type.             -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Andreas Politz

;; Author: Andreas Politz <mail@andreas-politz.de>
;; Keywords: extensions
;; Version: 1.0.0beta1
;; Package-Requires: ((emacs "29.1") (dash "2.19.1") (Commons "1.0.0beta1"))

(require 'Commons)

(eval-and-compile
  (Commons:load-main 'Struct))

(provide 'Struct)
;;; Struct.el ends here
