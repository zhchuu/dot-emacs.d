;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; Load all configuration
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

;;; Code:

;; Add lisp/ directory to load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/langs" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/themes" user-emacs-directory))

;; Requires
(require 'init-elpa)
(require 'init-global-settings)
(require 'init-theme)
(require 'init-flycheck)
(require 'init-autopair)
(require 'init-neotree)
;; (require 'init-auto-complete)
;; (require 'init-ac-source)
;; (require 'init-autopep8)
;; (require 'init-yasnippet)
;; (require 'init-markdown-mode)

(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" default)))
 '(markdown-command "/usr/bin/pandoc")
 '(package-selected-packages (quote (elpy flycheck))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el ends here
