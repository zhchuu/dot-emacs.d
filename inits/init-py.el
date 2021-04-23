;;; init-py.el --- Load Python settings
;;; Commentary:
;;; Code:

(defun zhchuu/python()
  "Elpy will automatically provide code completion, syntax error highlighting,
 and code hinting (in the modeline) for python files."
  (require-package 'elpy)
  (elpy-enable)
  (setq elpy-rpc-backend "jedi")

  );; function ends

;; Install flake8 to enable syntax checking.
;; Install jedi to enable find definitions.
(add-hook 'python-mode-hook 'zhchuu/python)
(add-hook 'python-mode-hook 'global-flycheck-mode)

(provide 'init-py)
;;; init-py.el ends here
