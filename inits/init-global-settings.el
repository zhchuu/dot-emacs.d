;;; init-global-setting.el --- Load global setting
;;; Commentary:

;; Code:

(setq package-selected-packages
      '(lsp-mode yasnippet lsp-treemacs helm-lsp projectile flycheck company helm-xref
                 helm-swoop helm-gtags dap-mode which-key elpy lsp-ui highlight-indent-guides
                 highlight-indentation sql-indent all-the-icons smart-tab undo-tree rainbow-mode
                 rainbow-delimiters neotree ag rg ace-window goto-chg gcmh beacon spacemacs-theme
                 auto-complete protobuf-mode json-mode minimap all-the-icons origami xcscope
                 go-mode expand-region symbol-overlay helm-ag swiper-helm citre))

(load-theme 'spacemacs-dark t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 ;; '(custom-enabled-themes '(misterioso))                 ;; Put the theme loading in the front to avoid overriding the setting below
 '(custom-enabled-themes '(spacemacs-dark))
 '(ediff-split-window-function 'split-window-sensibly)  ;; Split vertically or horizontally depending on window dimensions
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
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
 '(rainbow-delimiters-unmatched-face ((t (:background "cyan" :height 0.8))))
 '(aw-leading-char-face
     ((t (:foreground "red" :weight normal :height 2.9))))
 )


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Built-in feature mode / Config / Appearance
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Dump the custom-set-variables / custom-set-faces to the file rather than init.el
(setq custom-file (concat user-emacs-directory "/custom.el"))

;; Prevents loading emacs lisp mode automatically
(setq initial-major-mode 'fundamental-mode)

;; Set the frame title to display file path and name
(setq frame-title-format '((:eval (if (buffer-file-name)
          (abbreviate-file-name
           (buffer-file-name))
        "%b"))))

;; Disable startup msg (GNU emacs buffer)
(setq inhibit-startup-screen t)

;; Remove the beep
(setq visible-bell t)

;; Display time
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq display-time-format "%y-%m-%d %a %H:%M")
(display-time)  ;; display-time refresh every min

;; N spaces intead of a tab
(setq-default indent-tabs-mode nil)

;; Not to create backup file
(setq make-backup-files nil)

;; Enable eamcs share copy-paste with other applications
(setq x-select-enable-clipboard t)

;; Auto append newline
(setq require-final-newline t)

;; Buffer menu width
(setq Buffer-menu-name-width 30)

(setq-default display-fill-column-indicator-column 119)

;; Scroll smoothly
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq scroll-margin 6)
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; Always show line number
(global-display-line-numbers-mode t)

;; Show matching parenth
(show-paren-mode t)

;; Close toolbar and scroll bar
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; Start column-number-mode and size-indication-mode
(column-number-mode t)
(size-indication-mode t)

;; Automatically insert the right matching bracket
(electric-pair-mode 1)

;; Highlight the line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#3e4446")
(set-face-foreground 'highlight nil)  ;; keep the syntax highlighting

;; Keeping buffers automatically up-to-date
(global-auto-revert-mode t)

;; Show the current function name
(which-function-mode)
(eval-after-load "which-func"
  '(setq which-func-modes '(c++-mode c-mode go-mode)))

;; Let TAGS be case-sensitive
(setq tags-case-fold-search nil)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keys binding and functions
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; C-k to kill whole line
(global-set-key (kbd "C-K") 'kill-whole-line)

;; Enable narrow-to-region command
(put 'narrow-to-region 'disabled nil)

;; Toggle window split
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
      (next-win-buffer (window-buffer (next-window)))
      (this-win-edges (window-edges (selected-window)))
      (next-win-edges (window-edges (next-window)))
      (next-win-point (window-point (next-window)))
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
   (if this-win-2nd (other-window 1))
   (set-window-point (next-window) next-win-point)))))

;; Toggle window swap
(defun toggle-window-swap ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (next-win-point (window-point (next-window))))
        (set-window-buffer (selected-window) next-win-buffer)
        (set-window-buffer (next-window) this-win-buffer)
        (other-window 1)
        (set-window-point (next-window) next-win-point))))

(global-set-key (kbd "C-x |") 'toggle-window-split)

(global-set-key (kbd "C-x \\") 'toggle-window-swap)

;; Fast move to word
(global-set-key (kbd "M-g w") 'avy-goto-word-0)

;; Fast move to last change
(global-set-key (kbd "M-g c") 'goto-last-change)

;; Rename current file
(defun rename-this-file-and-buffer ()
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive)
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (new-name (read-string "New name: " name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

;; Delete current file
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(use-package origami
  :config
  (origami-mode)
  :bind
  ("C-c f" . origami-recursively-toggle-node)
  ("C-c F" . origami-toggle-all-nodes))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some third-party modes
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Keys hint
(use-package which-key
  :config
  (which-key-mode))

;; Undotree
;; C-x u -> undo-tree-visualize
;; (use-package undo-tree
;;   :init
;;   (setq undo-tree-visualizer-timestamps t)
;;   (setq undo-tree-visualizer-diff t)
;;   (setq undo-tree-auto-save-history nil)
;;   :config
;;   (progn
;;     (global-undo-tree-mode)
;;     )
;;   )

;; Rainbow mode
(use-package rainbow-mode
  :config
  (progn
    (defun @-enable-rainbow ()
      (rainbow-mode t))
    (add-hook 'prog-mode-hook '@-enable-rainbow)
    ))

;; Neotree
(use-package neotree
  :custom
  (neo-theme 'nerd2)
  :config
  (progn
    (setq neo-smart-open t)
    (setq neo-theme (if (display-graphic-p) 'icons 'nerd))
    (setq neo-window-fixed-size nil)
    (setq neo-window-width 50)
    ;; (setq-default neo-show-hidden-files nil)
    (global-set-key [f8] 'neotree-toggle)
    ))


;; M-x highlight-indent-guides-mode
;; Nice interface but quite slow when the file is large
(use-package highlight-indent-guides
  :init
  (set 'highlight-indent-guides-method 'bitmap)
  (set 'highlight-indent-guides-responsive 'top)
  (set 'highlight-indent-guides-suppress-auto-error t)
  (set 'highlight-indent-guides-delay 0.1))

;; M-x highlight-indent-mode
;; Not so pretty but it is able to handle large file
;; (use-package highlight-indentation
;;   :init
;;   (set-face-font 'highlight-indentation-face "Arial")
;;   (set-face-background 'highlight-indentation-face "#808080")
;;   (set-face-background 'highlight-indentation-current-column-face "#c3b3b3"))

;; Switch between windows faster
(use-package ace-window
  :bind ("M-o" . ace-window))

;; Hightlight cursor
(use-package beacon
  :config
  (beacon-mode 1))

(use-package minimap
  :init
  (setq minimap-window-location 'right)
  (setq minimap-minimum-width 20)
  (setq minimap-width-fraction 0.01)
  (setq minimap-automatically-delete-window 'visible)
  (setq minimap-hide-fringes 1))

(use-package auto-complete
  :config
  (ac-config-default))

(use-package xcscope
  :init
  (setq cscope-index-recursively 1)
  :config
  (cscope-setup))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package symbol-overlay
  :bind (("M-i" . symbol-overlay-put)
         ("M-n" . symbol-overlay-switch-forward)
         ("M-p" . symbol-overlay-switch-backward)))


(provide 'init-global-settings)
;;; init-global-settings.el ends here
