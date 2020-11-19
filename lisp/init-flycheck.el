;;; init-flycheck.el --- Setting of flycheck -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'flycheck)

(add-hook 'after-init-hook 'global-flycheck-mode)

;; Flycheck auto initialize
 (custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
  '(package-selected-packages (quote (flycheck))))
 (custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
  )

(provide 'init-flycheck)
;;; init-flycheck.el ends here
