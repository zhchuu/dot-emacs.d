;;; init-autopair.el --- Settings of autopair -*- lexical-binding: t -*-
;;; Commentary:

;; Autopair is an extension to the Emacs text editor that automatically pairs braces and quotes

;;; Code:

(require-package 'autopair)

(require 'autopair)
(autopair-global-mode)

(provide 'init-autopair)
;;; init-autopair.el ends here
