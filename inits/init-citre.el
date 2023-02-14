;;; init-citre.el -- ctags frontend for Emacs
;;; Commentary:

;; Code:

(use-package citre
  :defer t
  :init
  ;; This is needed in `:init' block for lazy load to work.
  (require 'citre)
  (require 'citre-config)
  (global-set-key (kbd "M-.") 'citre-jump)
  (global-set-key (kbd "M-,") 'citre-jump-back)
  (global-set-key (kbd "M-P") 'citre-peek)
  ;; :config
  (setq
   ;; Set these if readtags/ctags is not in your PATH.
   citre-readtags-program "/opt/homebrew/bin/readtags"
   citre-ctags-program "/opt/homebrew/bin/ctags"
   ;; Set this if you use project management plugin like projectile.  It's
   ;; used for things like displaying paths relatively, see its docstring.
   ;; citre-project-root-function #'projectile-project-root
   citre-use-project-root-when-creating-tags t
   citre-prompt-language-for-ctags-command t
   citre-auto-enable-citre-mode-modes '(prog-mode))
  )


(provide 'init-citre)
;;; init-citre.el ends here
