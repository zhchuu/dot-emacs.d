;;; init-ecb.el --- Setting of Emacs Code Browser
;;; Commentary:
;;; Code:

(require-package 'ecb)

(defun zhchuu/ecb ()
  (setq ecb-fix-window-size 'width)
  (setq ecb-windows-width 0.18)
  )

(provide 'init-ecb)
;;; init-ecb.el ends here
