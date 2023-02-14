;;; init-performance-tuning.el
;;; Commentary:

;; Code:

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

;; Improve the performance of processing long line
(setq bidi-inhibit-bpa t)
(setq-default bidi-display-reordering 'left-to-right)
(setq-default bidi-paragraph-direction 'left-to-right)

;; About fontity time
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
  ;; (setq gcmh-verbose t)
  ;; (setq garbage-collection-messages t)
  (setq gcmh-idle-delay 5)
  (setq gcmh-high-cons-threshold (* 64 1024 1024))
  (gcmh-mode 1)
  (gcmh-set-high-threshold))


(provide 'init-performance-tuning)
;;; init-performance-tuning.el ends here
