;;; init-theme.el --- Settings of theme -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; (add-to-list 'load-path "~/.emacs.d/lisp/themes/")
;; (add-to-list 'load-path "~/.emacs.d/lisp/")

(let ((basedir "~/.emacs.d/lisp/themes/"))
  (dolist (f (directory-files basedir))
    (if (and (not (or (equal f ".") (equal f "..")))
             (file-directory-p (concat basedir f)))
        (add-to-list 'load-path (concat basedir f))
	    (add-to-list 'custom-theme-load-path (concat basedir f)))))

(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
	 (color-theme-initialize)))


;; (load-theme 'misterioso)
;; (load-theme 'cyberpunk t)
(load-theme 'monokai t)
;; (load-theme 'wheatgrass)
;; (load-theme 'fogus)

;; (add-to-list 'default-frame-alist '(background-color . "#323232"))


(provide 'init-theme)
;;; init-theme.el ends here
