;;; init-elpa.el --- Settings and helpers for package.el (ELPA)
;;; Commentary:

;; Initialize the Emacs packages manager MELPA.
;; Download package by user-package function.

;; Code:

;; Doenload package.el and put to lisp/ if Emacs version < 24
(require 'package)

(toggle-debug-on-error)

;; Add packages to list
;; Official
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
;; (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Tuna
(add-to-list 'package-archives '("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/") t)
(add-to-list 'package-archives '("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/") t)
(add-to-list 'package-archives '("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/stable-melpa/") t)

;; Force to initialize ELPA
;; Run M-x package-refresh-contents when facing package not found error
(package-initialize)

;; Keep Emacs from automatically making packages available at startup
(setq package-enable-at-startup nil)

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
