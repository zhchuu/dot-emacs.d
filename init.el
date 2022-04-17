;;; init.el --- Load the full configuration (Emacs version: 26.3)
;;; Commentary:

;; Load all configuration.


;; Code:

;; Add lisp/ directory to load-path
(add-to-list 'load-path (expand-file-name "inits" user-emacs-directory))

;; Set default font size
(set-default-font "Monaco 15")

(require 'init-elpa)
(require 'init-global-settings)
(require 'init-helm)
(require 'init-projectile)
(require 'init-dashboard)
(require 'init-tramp)

(add-hook 'after-init-hook
          #'(lambda ()
              (message "Loading time: %s."
                       (float-time
                        (time-subtract after-init-time before-init-time)))))


(provide 'init)
;;; init.el ends here
