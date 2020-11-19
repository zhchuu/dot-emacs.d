;;; init-neotree.el --- Settings of autopair -*- lexical-binding: t -*-
;;; Commentary:

;; Autopair is an extension to the Emacs text editor that automatically pairs braces and quotes

;;; Code:

(require-package 'neotree)

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

(provide 'init-neotree)
;;; init-neotree.el ends here
