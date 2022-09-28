;;; init-projectile.el --- Settings of projectile
;;; Commentary:

;;; Code:

(use-package projectile
  :ensure t
  :init
  (setq projectile-enable-caching t)
  (setq projectile-file-exists-remote-cache-expire nil)
  (setq projectile-track-known-projects-automatically nil)  ;; Disable automatic project detection
  (setq projectile-mode-line
        '(:eval (if (projectile-project-p)
                    (format " Projectile[%s]" (projectile-project-name))
                  "")))
  ;; See: https://github.com/bbatsov/projectile/issues/234
  (defadvice projectile-on (around exlude-tramp activate)
    "This should disable projectile when visiting a remote file"
    (unless  (--any? (and it (file-remote-p it))
                     (list
                      (buffer-file-name)
                      list-buffers-directory
                      default-directory
                      dired-directory))
      ad-do-it))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-global-mode)
  )


(provide 'init-projectile)
;; init-projectile.el ends here
