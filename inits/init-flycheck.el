;;; init-flycheck.el --- Setting of flycheck
;;; Commentary:
;;; Code:
(require-package 'flycheck)

(declare-function flycheck-display-error-messages-unless-error-list
		  "Show messages of ERRORS unless the error list is visible.")

;; Display the windows below.
(defun zhchuu/flycheck ()
  "Configurate flycheck."
  (add-to-list 'display-buffer-alist
	       `(,(rx bos "*Flycheck errors*" eos)
		 (display-buffer-reuse-window
		  display-buffer-in-side-window)
		 (side            . bottom)
		 (reusable-frames . visible)
		 (window-height   . 0.23)))
  (setq flycheck-display-errors-function
	#'flycheck-display-error-messages-unless-error-list))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
