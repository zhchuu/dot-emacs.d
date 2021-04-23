;;; init.el --- Load the full configuration
;;; Commentary:

;; Load all configuration
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

;;; Code:

;; Add lisp/ directory to load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/themes" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "inits" user-emacs-directory))

;; Requires
(require 'init-elpa)
(require 'init-global-settings)
(require 'init-neotree)
(require 'init-autopair)
(require 'init-theme)
(require 'init-flycheck)
(require 'init-c-cpp)
(require 'init-py)

;; TODO:
;; 1. Code navigation in C/C++.
;; 2. The compatibility of ECB and flycheck errors window.
;; 3. Latex mode.
;; 4. Column indicator in Emacs 27.

(provide 'init)
;;; init.el ends here
