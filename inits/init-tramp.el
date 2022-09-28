;;; init-tramp.el -- Some config of tramp mode
;;; Commentary:

;;; Code:

(use-package tramp
  :config
  (setq  tramp-ssh-controlmaster-options nil)
  (setq remote-file-name-inhibit-cache 600)
  ;; Avoid error: “gzip: (stdin): unexpected end of file”
  (setq tramp-inline-compress-start-size (* 1024 8))
  (setq tramp-copy-size-limit (* 1024 1024 2))
  (setq tramp-allow-unsafe-temporary-files t)
  (setq tramp-auto-save-directory temporary-file-directory)
  (setq tramp-persistency-file-name (expand-file-name "tramp-connection-history" user-emacs-directory))
  ;; Backup (file~) disabled and auto-save (#file#) locally to prevent delays in editing remote files
  ;; https://stackoverflow.com/a/22077775
  (add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil)))


(provide 'init-tramp)
;;; init-tramp.el ends here
