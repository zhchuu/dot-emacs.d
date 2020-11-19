;;; my-python-mode.el --- Python mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun zhchuu/python()
  "Elpy will automatically provide code completion, syntax error highlighting and code hinting (in the modeline) for python files."
  (require-package 'elpy)
  (elpy-enable)

  );; function ends

(provide 'my-python-mode)
;;; my-python-mode.el ends here
