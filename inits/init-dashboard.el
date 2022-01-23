;;; init-dashboard.el --- Settings of the dashboard
;;; Commentary:

;;; Code:

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  ;; Set the banner
  (setq dashboard-startup-banner 3)
  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)
  ;; Icons
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  ;; To show info about the packages loaded and the init time
  (setq dashboard-set-init-info t)
  ;; To customize which widgets are displayed
  (setq dashboard-items '((recents . 30)
                          (projects . 15)
                          (agenda . 5)))
  ;; Footer message
  (setq dashboard-footer-messages '("chhzhong's emacs"))
;;   (setq dashboard-footer-icon (all-the-icons-octicon "dashboard"
;;                                                      :height 1.1
;;                                                      :v-adjust -0.05
;;                                                      :face 'font-lock-keyword-face))
  )


(provide 'init-dashboard)
;;; init-dashboard.el ends here
