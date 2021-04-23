;;; init-c-cpp.el --- Load the C/C++ settings
;;; Commentary:

;; The C/C++ development environment includes auto completion, syntax check,
;; disassemble, navigation, and code browsing.

;; About Auto completion:
;; The campany is the frontend and the irony is the backend.
;; Install clang and LLVM before M-x irony-install-server.
;; Use M-n and M-p to select candidate items.

;; About syntax check:
;; Again I use irony as the backend and the frontend becomes the flycheck.
;; Use C-c ! l command to display errors and warnings.

;; About disassemble:
;; M-x disaster

;; About navigation and refactoring:
;; Compile and install rtags by hand.
;; M-x cmake-ide-compile to compile the configured cmake project within Emacs.

;; About coding browsing:
;; Use ECB package (Emacs Code Browsing).
;; M-x ecb-activate command to enable ECB.
;; The rebuild-method-buffer in ECB works only if the semantic is enabled.

;;; Code:

;; Package installation of cmake-ide
(require-package 'cmake-ide)

(setq c-basic-offset 4)

;; Auto completion ---
(require-package 'irony)
(require-package 'company)
(require-package 'company-irony)
(require-package 'company-c-headers)
(require-package 'irony-eldoc)
(require-package 'company-irony-c-headers)


(require 'company)
(require 'irony)
(require 'company-irony)
(require 'company-c-headers)
(require 'irony-eldoc)
(require 'company-irony-c-headers)
(add-hook 'c++-mode-hook 'company-mode)
(add-hook 'c-mode-hook 'company-mode)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(add-hook 'irony-mode-hook 'irony-eldoc)
(add-hook 'eldoc-mode-hook #'(lambda ()
				 (setf eldoc-idle-delay 0.5)))

(setq company-idle-delay 0)
(setq company-minimum-prefix-length 2)
(setq company-dabbrev-downcase nil)

(setf company-backends '())  ;; removes all the predefined backends at first
(add-to-list 'company-backends 'company-dabbrev)
(add-to-list 'company-backends 'company-dabbrev-code)
(add-to-list 'company-backends 'company-files)
(add-to-list 'company-backends 'company-semantic)
(add-to-list 'company-backends 'company-keywords)
(add-to-list 'company-backends 'company-irony)
(add-to-list 'company-backends 'company-capf)
(add-to-list 'company-backends 'company-c-headers)
(add-to-list 'company-backends 'company-irony-c-headers)

;; (add-to-list 'company-backends 'company-gtags)

;; (add-to-list 'company-c-headers-path-system "/usr/include/c++/4.9.2/")
;; (global-set-key (kbd "C-;") 'company-complete-common)

;; Company color setting
;; (defun theme-dark ()
;;   (interactive)
;;    (set-face-foreground 'company-tooltip "#000")
;;     (set-face-background 'company-tooltip "#fff")
;;      (set-face-foreground 'company-scrollbar-bg "#fff")
;;       (set-face-background 'company-scrollbar-fg "#999")
;; )
;; (theme-dark)

;; Syntax check ---
(require-package 'flycheck)
(require-package 'flycheck-irony)
(autoload 'zhchuu/flycheck "init-flycheck")
(require 'flycheck)
(add-hook 'c++-mode-hook 'global-flycheck-mode)
(add-hook 'c-mode-hook 'global-flycheck-mode)
(add-hook 'flycheck-mode-hook 'flycheck-irony-setup)
(add-hook 'prog-mode-hook 'zhchuu/flycheck)

;; Disassemble ---
(require-package 'disaster)
(require 'disaster)

;; Code browsing ---
(require-package 'ecb)
(require 'ecb)
(autoload 'zhchuu/ecb "init-ecb")
(add-hook 'c++-mode-hook 'zhchuu/ecb)
(add-hook 'c-mode-hook 'zhchuu/ecb)
;; (setq ecb-auto-activate 1)  ; Auto activate

;; Enable semantic pasing ---
;; The rebuild-method-buffer in ECB works only if the semantic is enabled.
(require 'semantic/idle)
(defun zhchuu/semantic (MODE)
  "Custom semantic mode.
MODE: the major programming mode"
  (let ((semantic-submodes '(global-semantic-decoration-mode
			     global-semantic-idle-local-symbol-highlight-mode
			     global-semantic-highlight-func-mode
			     global-semanticdb-minor-mode
			     global-semantic-mru-bookmark-mode
			     global-semantic-idle-summary-mode
			     global-semantic-stickyfunc-mode
			     )))
    (setq semantic-default-submodes (append semantic-default-submodes semantic-submodes)
	  semantic-idle-scheduler-idle-time 1)
    (semanticdb-enable-gnu-global-databases 'MODE)
    (semantic-mode 1)))

(add-hook 'c++mode-hook #'(lambda ()) (zhchuu/semantic 'c++-mode))
(add-hook 'cmode-hook #'(lambda ()) (zhchuu/semantic 'c-mode))

;; Toggle flycheck with a key
;;(defun toggle-flycheck-error-buffer ()
;;  "toggle a flycheck error buffer."
;;  (interactive)
;;  (if (string-match-p "Flycheck errors" (format "%s" (window-list)))
;;      (dolist (w (window-list))
;;        (when (string-match-p "*Flycheck errors*" (buffer-name (window-buffer w)))
;;          (delete-window w)
;;          ))
;;    (flycheck-list-errors)
;;    )
;;  )
;;(global-set-key (kbd "<f4>") 'toggle-flycheck-error-buffer)

;; Code navivation ===
;(require-package 'helm)
;(require-package 'helm-rtags)
;(require 'rtags)
;(cmake-ide-setup)

;(add-hook 'c++-mode-hook 'trivialfis/rtags)
;(add-hook 'c-mode-hook 'trivialfis/rtags)
;(setq rtags-display-result-backend 'helm)

(provide 'init-c-cpp)
;;; init-c-cpp.el ends here
