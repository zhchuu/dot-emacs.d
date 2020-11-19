;;; init-elpa.el --- Settings and helpers for package.el (ELPA) -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Download package.el and put to lisp/ if Emacs version < 24
(require 'package)

;; Add packages to list
;; b/t indicates that add the element to the beginning/terminal
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Define require-package function
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
	  t
	(if (or (assoc package package-archive-contents) no-refresh)
		(package-install package)
	(progn
	  (package-refresh-contents)
	  (require-package package min-version t)))))

;; Force to initialize ELPA
(package-initialize)
;; Run M-x package-refresh-contents comand when facing package not found error
;; (package-refresh-contents)

(provide 'init-elpa)
;;; init-elpa.el ends here
