;;; init-global-settings.el --- Load global settings
;;; Commentary:

;; Some global settings for all file types.

;;; Code:

;; Dump the custom-set-variables / custom-set-faces to the file rather than init.el
(setq custom-file (concat user-emacs-directory "/custom.el"))

;; Prevents loading emacs lisp mode automatically
(setq initial-major-mode 'fundamental-mode)

;; Set the frame title to display file path and name
(setq frame-title-format '((:eval (if buffer-file-name)
				  (abbreviate-file-name
				   (buffer-filr-name))
				  "%b")))

;; Disable startup msg (GNU emacs buffer)
(setq inhibit-startup-screen t)

;; Remove the beep
(setq visible-bell t)

;; M-x package-install-selected-packages to install all packages
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(misterioso))                ;; Put the theme loading in the front to avoid overriding the setting below
 ;; '(custom-enabled-themes '(spacemacs-dark))
 '(ediff-split-windoe-function 'split-window-sensibly) ;; Split vertically or horizontally depending on window dimensions
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(package-selected-packages
   '(lsp-mode yasnippet lsp-treemacs helm-lsp projectile flycheck company helm-xref helm-swoop helm-gtags dap-mode which-key elpy lsp-ui nyan-mode highlight-indent-guides highlight-indentation sql-indent dashboard all-the-icons smart-tab undo-tree rainbow-mode rainbow-delimiters neotree ag rg ace-window beacon minimap spacemacs-theme goto-chg gcmh))
 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "red" :height 2.0))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "orange" :height 1.8))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow" :height 1.6))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "green" :height 1.4))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "blue" :height 1.2))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "violet" :height 1.1))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "purple" :height 1.0))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "black" :height 0.9))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "cyan" :height 0.8))))
 '(aw-leading-char-face
   ((t (:foreground "red" :weight normal :height 2.9))))
 )


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Feature mode
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show line number
(if (> emacs-major-version 25)
    (global-display-line-numbers-mode t)
  (global-lineum-mode t))

;; Close the toolbar and scroll bar
(tool-bar-mode 0)
;; (scroll-bar-mode 0)

;; Display time
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq display-time-format "%y-%m-%d %a %H:%M")
(display-time)

;; Show matching parenth
(show-paren-mode t)

;; Show column number and file size
(column-number-mode t)
(size-indication-mode t)

;; Automatically insert the right matching bracket
(electric-pair-mode t)

;; Highlight the line
(global-hl-line-mode t)
(set-face-background 'hl-line "#3e4446")
(set-face-foreground 'highlight nil)  ;; keep the syntax highlighting

;; N spaces instead of a tab
(setq-default indent-tabs-mode nil)

;; Keeping buffers automatically up-to-date
(global-auto-revert-mode t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; C-k to kill the whole line
(global-set-key (kbd "C-k") 'kill-whole-line)

;; Not to create backup file
(setq make-backup-files nil)

;; Enable emacs share copy-paste with other application
(setq x-select-enable-clipboard t)

;; Auto append new line
(setq require-final-newline t)

;; Enable narrow-to-region command
(put 'narrow-to-region 'disable nil)

;; Set cursor
(set-cursor-color "#ffffff")

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

;; Fast move to word
(global-set-key (kbd "M-g w") 'avy-goto-word-0)

;; Fast move to last change
(global-set-key (kbd "M-g c") 'goto-last-change)

;; Scroll smoothly
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq scroll-margin 10)

;; The default setting is too low for lsp-mode's needs
;; due to the fact that client/server communication
;; generates a lot of memory/garbage.
(setq gc-cons-threshold 100000000)  ;; 100mb

;; Increase the amount of data which Emacs reads from
;; the process. Again the emacs default is too low 4k
;; considering that the some of the language server
;; responses are in 800k - 3M range.
(setq read-process-output-max (* 1024 1024))  ;; 1mb

;; Improve IO performance
(setq process-adaptive-read-buffering nil)

;; Improve the performace of processing long line
(setq bidi-inhibit-bpa t)
(setq-default bidi-display-reordering 'left-to-right)
(setq-default bidi-paragraph-direction 'left-to-right)

;; Auto fontify time
(setq jit-lock-defer-time nil)
(setq jit-lock-context-time 0.1)
(setq fast-but-imprecise-scrolling nil)
(setq redisplay-skip-fontification-on-input nil)

;; Screen update time
(setq idle-update-delay 0.1)

;; Cache of font
(setq inhibit-compacting-font-caches t)

;; Garbage Collector Magic Hack
(use-package gcmh
  :demand
  :init
  (setq gcmh-idle-delay 5)
  (setq gcmh-high-cons-threashold (* 64 1024 1024))
  (gcmh-mode 1)
  (gcmh-set-high-threshold))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some third-party mods are turned on by default
;; so I put them in the global settings
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show hint of shortcut key
(which-key-mode)

;; Smart tab
(use-package smart-tab
  :config
  (progn
    (defun @-enable-smart-tab ()
      (smart-tab-mode))
    (add-hook 'prog-mode-hook '@-enable-smart-tab)))

;; Undotree
;; C-x u -> undo-tree-visualize
(use-package undo-tree
  :ensure t
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

;; Rainbow mode
(use-package rainbow-mode
  :config
  (progn
    (defun @-enable-rainbow ()
      (rainbow-mode t))
    (add-hook 'prog-mode-hook '@-enable-rainbow)))

;; Neotree
(use-package neotree
  :custom
  (neo-theme 'nerd2)
  :config
  (progn
    (setq neo-smart-open t)
    (setq neo-theme (if (display-graphic-p) 'icons 'nerd))
    (global-set-key [f8] 'neotree-toggle)))

;; Start nyan-mode
(nyan-mode t)
(nyan-start-animation) ;; Start animation (cpu costly)

;; M-x highlight-indent-guides-mode
;; Nice interface but quite slow when the file is large
(require 'highlight-indent-guides)
;; (set 'highlight-indent-guides-method 'bitmap)
(set 'highlight-indent-guides-responsive 'top)

;; M-x highlight-indentation-mode
;; Not so pretty but it is able to handle large file
(require 'highlight-indentation)
(set-face-font 'highlight-indentation-face "Arial")
(set-face-background 'highlight-indentation-face "#808080")
(set-face-background 'highlight-indentation-current-column-face "#c3b3b3")

;; Switch between windows faster
(global-set-key (kbd "M-o") 'ace-window)

;; SQL indent
(add-hook 'sql-mode-hook 'sqlind-minor-mode)

(beacon-mode 1)

(set-face-attribute 'mode-line nil
                    :box nil
                    :overline "dark grey"
                    :underline "dark grey")


(provide 'init-global-settings)
;;; init-global-settings.el ends here
