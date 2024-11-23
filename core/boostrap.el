;; -*- lexical-binding: t; no-byte-compile: t -*-

;; Initialize the straight package manager.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Configure straight and make it work with use-package.
(setq straight-recipe-repositories
      '(melpa gnu-elpa-mirror))
(setq straight-profiles
      `((nil . ,(concat user-emacs-directory "config/package-versions"))))
(setq straight-use-package-by-default t)
(setq use-package-always-defer t)

;; Initialize the core module.
(load-file (concat user-emacs-directory "core/init.el"))
