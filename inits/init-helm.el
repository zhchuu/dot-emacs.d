;;; init-helm.el --- Settings of the helm-mode
;;; Commentary:

;;; Code:

(use-package helm-swoop)
;; (use-package helm-gtags)
(use-package helm
  :diminish helm-mode
  :init
  (progn
    (setq helm-candiate-number-limit 100)
    ;; https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          helm-input-idle-delay 0.01  ; this actually updates things
                                        ; reeeelatively quickly.
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t)
    (helm-mode))
  :config
  (progn
    )
  :bind
  (("C-c s" . helm-swoop)
   ("C-x C-f" . helm-find-files)
   ("C-x b" . helm-buffers-list)
   ("M-y" . helm-show-kill-ring)
   ("M-x" . helm-M-x))
  )


(provide 'init-helm)
;;; init-helm.el ends here
