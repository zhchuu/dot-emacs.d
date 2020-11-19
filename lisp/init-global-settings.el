;;; init-global-setting.el --- Load global setting -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Tab width
;; Some variables in Emacs are "buffer-local", meaning that
;; each buffer is allowed to have a separate value for that
;; variable that overrides the global default.
;; setq sets its local value in the current buffer and setq-default sets the global default value
(setq c-basic-offset 4)
(setq default-tab-width 4)

;; Always show line number
(global-linum-mode 1)
(setq linum-format "%3d\u2503")

;; Not to create backup file
(setq make-backup-files nil)

;; C-k to kill whole line
(global-set-key (kbd "C-K") 'kill-whole-line)

;; Set cursor
(setq-default cursor-type 'bar)
(global-hl-line-mode 1)
(set-face-foreground 'highlight nil) ;; Keep the sytax highlighting
;; (set-face-background 'hl-line "3e4446")

;; Close toolbar and scroll bar
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

;; Display time
(display-time)

;; Parenth completion mode
(show-paren-mode t)

;; Set title
(setq frame-title-format "emacs@%b")

;; Grammar highlighting
(setq global-front-lock-mode t)

;; Enable eamcs share copy-paste with other applications
(setq x-select-enable-clipboard t)

;; Auto append newline
(setq require-final-newline t)

;; Toggle window split
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

;; (define-key ctl-x-4-map "t" 'toggle-window-split)
(global-set-key (kbd "C-x |") 'toggle-window-split)

;; (setq fci-rule-width 1)
;; (setq fci-rule-color "darkblue")

;; python mode
(autoload 'zhchuu/python "my-python-mode")
(add-hook 'python-mode-hook 'zhchuu/python)

;; latex mode
(add-hook 'latex-mode-hook '(lambda() (set-fill-column 80)))
(add-hook 'latex-mode-hook 'turn-on-auto-fill)

;; Code fold or unfold
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'ess-mode-hook        'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
(add-hook 'python-mode-hook     'hs-minor-mode)

;; F4 to fold and unfold code
(global-set-key [f4] 'hs-toggle-hiding)

;; column indicator
;; fci-mode causes the bug of showing two popup windows when complementing code
;; Will be replaced by display-fill-column-indicator-mode in Emacs 27
(require 'fill-column-indicator)
(add-hook 'c-mode-common-hook   'fci-mode)
(add-hook 'python-mode-hook     'fci-mode)
(setq-default fci-rule-column 120)

(provide 'init-global-settings)
;;; init-global-settings.el ends here
