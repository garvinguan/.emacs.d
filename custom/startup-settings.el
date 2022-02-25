;; Garbage Collection Threshold
(setq gc-cons-threshold 100000000)

;; Read more bytes from a process
(setq read-process-output-max (* 1000 1000)) ;; 1mb

;; Fix performance for very long lines
(global-so-long-mode 1)

;; Causes buffer to always have the latest version (if using an external editor)
(global-auto-revert-mode t)

;; Removes Splash Screen
(setq inhibit-startup-message t)

;;Set title frame
(setq frame-title-format '("best editor in existence"))

;; remove bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(customize-set-variable 'horizontal-scroll-bar-mode nil)

;; use spaces for indentation
(setq-default indent-tabs-mode nil)

;; cursor doesn't blink
(blink-cursor-mode -1)

;; No bell
(setq ring-bell-function 'ignore)

;; Ban whitespace at end of lines, globally
(add-hook 'write-file-hooks
          '(lambda ()
             (delete-trailing-whitespace)))

;; Set up relative line numbering
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

;;(display-time)
(setq
  scroll-margin 5
  scroll-step 3
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)

;; Set font size
(set-face-attribute 'default nil :height 180)

;; Turn on winner mode for window undo/redo
(winner-mode 1)
