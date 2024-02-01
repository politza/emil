;; -*- lexical-binding: t -*-

(setq package-enable-at-startup nil)

(defconst Blue:user-directory (concat user-emacs-directory "user")
  "The user directory of Blue.

This directory may contain custom init-files.")

(when (file-exists-p (expand-file-name "early-init.el" Blue:user-directory))
  (load-file (expand-file-name "early-init.el" Blue:user-directory)))
