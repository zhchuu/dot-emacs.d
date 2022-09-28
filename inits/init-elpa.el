;;; init-elpa.el --- Settings and helpers for package.el (ELPA)
;;; Commentary:

;; Initialize the Emacs packages manager MELPA.
;; Download package by user-package function.

;; Code:

;; Download package.el and put to lisp/ if Emacs version < 24
(require 'package)

;; Tuna
(setq package-archives '(("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
                         ("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/stable-melpa/")))

;; Force to initialize ELPA
;; Run M-x package-refresh-contents command when facing package not found error
(package-initialize)

;; Update packages list if we are on a new install
(unless package-archive-contents
  (package-refresh-contents))

;; A list of pkgs to programmatically install
(setq my-package-list '(use-package))

;; Programmatically install/ensure installed
(dolist (package my-package-list)
  (unless (package-installed-p package)
    (package-install package)))

(provide 'init-elpa)
;;; init-elpa.el ends here
