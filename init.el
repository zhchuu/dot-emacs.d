;;; init.el --- Load the full configuration (Emacs version: 27.2+)
;;; Commentary:

;; Code:

(toggle-debug-on-error)
(setq package-enable-at-startup nil)

;; Add lisp/ directory to load-path
(add-to-list 'load-path (expand-file-name "inits" user-emacs-directory))

;; Mac specific key bindings
;; You may delete these if you don't want it.
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'nil)
  )

;; Set default font size
;; You may delete these if you don't want it.
(add-to-list 'default-frame-alist '(font . "Monaco 10"))
(set-face-attribute 'default t :font "Monaco 10")

;; Init
(require 'init-elpa)
(require 'init-global-settings)
(require 'init-performance-tuning)

;; Plugins
(require 'init-helm)
(require 'init-projectile)
(require 'init-tramp)

;; TODO: Langs

(add-hook 'after-init-hook
          #'(lambda ()
              (message "Loading time: %s."
                       (float-time
                        (time-subtract after-init-time before-init-time)))))


(provide 'init)
;;; init.el ends here
