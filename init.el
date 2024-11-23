;; -*- lexical-binding: t; no-byte-compile: t -*-

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (message "Straight bootstrap: %s" bootstrap-file)
  (load bootstrap-file nil 'nomessage))

(setq straight-recipe-repositories
      '(melpa gnu-elpa-mirror))
(setq straight-profiles
      `((nil . ,(concat user-emacs-directory "package-versions.el"))))
(setq straight-use-package-by-default t)
(setq use-package-always-defer t)

(straight-use-package
       `(Core
         :local-repo
         ,(expand-file-name "packages/Core" user-emacs-directory)
         :files ("src/*.el")))

(Core:init)

(when (file-exists-p (expand-file-name "init.el" Core:user-directory))
  (load-file (expand-file-name "init.el" Core:user-directory)))
