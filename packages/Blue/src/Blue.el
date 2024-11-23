;;; Blue.el --- Blue's core package.             -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Andreas Politz

;; Author: Andreas Politz <mail@andreas-politz.de>
;; Keywords: extensions
;; Version: 1.0.0beta1
;; Package-Requires: ((emacs "29.1") (dash "2.19.1"))

(declare-function straight-register-package "ext:straight")

(defconst Blue:packages-directory (concat user-emacs-directory "packages")
  "The packages directory of Blue.

This directory acts as a repository for packages.")

(defconst Blue:features-directory (concat user-emacs-directory "features")
  "The features directory of Blue.

This directory contains the features of the editor.")
  
(defun Blue:register-packages ()
  "Register packages in `Blue:packages-directory' with straight."
  (dolist (it (directory-files
               Blue:packages-directory
               :full
               directory-files-no-dot-files-regexp))
    (when (file-exists-p (expand-file-name "Eldev" it))
      (straight-register-package
           `(,(intern (file-name-base it))
             :local-repo ,it :files ("src/**/*.el"))))))

;;;###autoload
(defun Blue:init ()
  (Blue:register-packages))

(provide 'Blue)
;;; Blue.el ends here
