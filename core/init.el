;; -*- lexical-binding: t; no-byte-compile: t -*-

(defconst Core:core-directory (concat user-emacs-directory "core")
  "The core directory of Blue.

It provides the basic framework for the editor.")

(defconst Core:builtin-packages-directory (concat user-emacs-directory "packages")
  "The builtin packages directory of Blue.

This directory provides a repository for builtin packages."

(defconst Core:features-directory (concat user-emacs-directory "features")
  "The features directory of Blue.

This directory contains the features of the editor.")

(defconst Core::core-packages '(dash f s)
  "A list of packages that should always be available.")

(defconst Core::builtin-packages-files '("src/*.el")
  "Declaration of files for builtin packages.

This is used as the value of the `:files' directive in `straight-register-package'."
  
(defun Core::require-core-packages ()
  "Acquire and require the packages in `Core::core-packages'."
  (--each Core::core-packages
    (straight-use-package it)
    (require it)))

(defun Core::register-builtin-packages ()
  "Register packages in `Core::packages-directory' with straight."
  (--each (f-directories Core::builtin-packages-directory)
    (straight-register-package
     `(,(intern (f-filename it))
       :local-repo it
       :file ,Core::builtin-packages-files))))

(Core::require-core-packages)
(Core::register-packages)
