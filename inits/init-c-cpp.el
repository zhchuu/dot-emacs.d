;;; init-c-cpp.el --- Load the C/C++ settings
;;; Commentary:

;; The C/C++ development environment includes auto completion, syntax check,
;; disassemble, navigation, refactoring, and code browsing.

;; About Auto completion: eglot + company
;; Use M-n and M-p to select candidate items.

;; About syntax check: eglot + flycheck
;; Use C-c ! l command to display errors and warnings.

;; About navigation and refactoring: eglot + clangd
;; Follow the official instructions to install clangd.
;; clangd --version to ensure the installation.

;; About disassemble:
;; M-x disaster

;; About coding browsing:
;; Use ECB package (Emacs Code Browsing).
;; M-x ecb-activate command to enable ECB.
;; The rebuild-method-buffer in ECB works only if the semantic is enabled.
;; **Note**: The ECB has it own windows management system, which doesn't get
;; along with any other windows (flycheck error window).  I may fix it later.

;;; Code:

;; Set the tab width
(setq c-basic-offset 4)

;; Code navivation ---
(require-package 'eglot)
(require 'eglot)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

;; Auto completion ---
(require-package 'company)
(require 'company)
(add-hook 'c-mode-hook 'company-mode)
(add-hook 'c++-mode-hook 'company-mode)

;; Syntax check ---
(require-package 'flycheck)
(require 'flycheck)
(autoload 'zhchuu/flycheck "init-flycheck")
(add-hook 'c++-mode-hook 'global-flycheck-mode)
(add-hook 'c-mode-hook 'global-flycheck-mode)
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


(provide 'init-c-cpp)
;;; init-c-cpp.el ends here
