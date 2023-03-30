
(setq-default mode-line-format (delq 'mode-line-modes mode-line-format))
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;;;;;;;;;;;;;; Allows specific setting loading ;;;;;;;;;;;;;;;
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

(load-user-file "custom/functions.el")
(load-user-file "custom/startup-settings.el")
(load-user-file "custom/themes.el")
(load-user-file "custom/org-settings.el")

;;;;;;;;;;Window Numbering;;;;;;;;;;;;;;
(use-package winum
  :straight t
  :hook (after-init . winum-mode)
  :config
  (setq winum-auto-assign-0-to-minibuffer t
        winum-auto-setup-mode-line t
        winum-scope 'frame-local))

;; String inflection
(use-package string-inflection)

;;;;;;;;;;Evil;;;;;;;;;;;;;;
(use-package evil
  :init
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-keybinding nil) ;; set up for evil-collection
  (setq-default evil-cross-lines t)
  :ensure t
  :config
  (evil-mode 1)
  ;; Make movement keys work like they should
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

  (use-package undo-fu)
  ;; Make horizontal movement cross lines

  (use-package evil-surround
    :ensure t
    :config
    (evil--add-to-alist 'evil-surround-pairs-alist
        ?\( '("(" . ")")
        ?\[ '("[" . "]")
        ?\{ '("{" . "}"))
    (global-evil-surround-mode 1))

  (use-package neotree
    :ensure t
    )

  (use-package evil-visualstar
    :ensure t
    :config (global-evil-visualstar-mode))

  (use-package evil-exchange
    :ensure t
    :config (evil-exchange-install))

  (use-package evil-matchit
    :ensure t
    :config (global-evil-matchit-mode 1))

  (use-package evil-args
    :ensure t
    :config
    ;; bind evil-args text objects
    (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

    ;; bind evil-forward/backward-args
    (define-key evil-normal-state-map "L" 'evil-forward-arg)
    (define-key evil-normal-state-map "H" 'evil-backward-arg)
    (define-key evil-motion-state-map "L" 'evil-forward-arg)
    (define-key evil-motion-state-map "H" 'evil-backward-arg)

    ;; bind evil-jump-out-args
    (define-key evil-normal-state-map "K" 'evil-jump-out-args)
    )

  (use-package evil-indent-textobject
    :ensure t)

  (use-package evil-org
    :ensure t
    :after org
    :hook (org-mode . (lambda () evil-org-mode))
    :config

    (add-hook 'org-capture-mode-hook 'evil-insert-state)
    (evil-org-set-key-theme
	  '(textobjects insert navigation additional shift todo heading))
    (defun evil-org-open-below (count)
        "Insert a new line below point and switch to Insert state.
    The insertion will be repeated COUNT times."
        (interactive "p")
        (evil-insert-newline-below)
        (setq evil-insert-count count
            evil-insert-lines t
            evil-insert-vcount nil)
        (evil-insert-state 1)
        (add-hook 'post-command-hook #'evil-maybe-remove-spaces))
    (defun evil-org-open-above (count)
        "Insert a new line above point and switch to Insert state.
    The insertion will be repeated COUNT times."
        (interactive "p")
        (evil-insert-newline-above)
        (setq evil-insert-count count
            evil-insert-lines t
            evil-insert-vcount nil)
        (evil-insert-state 1)
        (add-hook 'post-command-hook #'evil-maybe-remove-spaces))

    (evil-define-key '(normal visual) org-mode-map (kbd "o") 'evil-org-open-below)
    (evil-define-key '(normal visual) org-mode-map (kbd "O") 'evil-org-open-above)
    )

  (use-package powerline-evil
    :ensure t
    :config
    (powerline-evil-vim-color-theme))
)

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package general
  :straight t
  :after evil
  :config
  (general-create-definer leader-def
    :states '(normal visual motion emacs)
    :keymaps 'override
    :prefix "SPC")
  (leader-def "" nil)
  (leader-def
      "SPC" 'helm-M-x
      "bd" 'kill-this-buffer
      "c"  'org-capture
      "fd" 'init-file
      "ff" 'helm-find-files
      "fn" 'load-notes-file
      "fs" 'save-buffer
      "ft" 'load-todos-file
      "gs" 'magit-status
      "jp" 'json-pretty-print-buffer
      "k" 'switch-to-buffer
      "lt" 'treemacs
      "pb" 'projectile-switch-to-buffer
      "pf" 'projectile-find-file
      "pp" 'helm-projectile-switch-project
      "ta" 'treemacs-add-project-to-workspace
      "tl" 'toggle-truncate-lines
      "w-" 'my-split-window-horizontal
      "w/" 'my-split-window-vertical
      "wU" 'winner-redo
      "wd" 'delete-window
      "wm" 'toggle-maximize-buffer
      "wu" 'winner-undo
      "xc" 'save-buffers-kill-terminal
      "xg" 'helm-projectile-rg
      "xe" 'eval-last-sexp
      "xh" 'mark-whole-buffer
      "xk" 'delete-current-buffer-file
      "1" 'winum-select-window-1
      "2" 'winum-select-window-2
      "3" 'winum-select-window-3
      "4" 'winum-select-window-4
      "5" 'winum-select-window-5
      )
  (general-define-key
    :states 'visual
    "s" 'evil-surround-region)
    (with-eval-after-load 'evil
      (general-add-hook 'after-init-hook
                        (lambda (&rest _)
                        (when-let ((messages-buffer (get-buffer "*Messages*")))
                            (with-current-buffer messages-buffer
                            (evil-normalize-keymaps))))
                        nil
                        nil
                        t))
  )

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0)
  )

(use-package flycheck)
(use-package dash)
(use-package popup)

(use-package helm
  :config
  (helm-mode 1)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (setq helm-buffer-details-flag nil)
  (define-key helm-buffer-map (kbd "C-d") 'helm-buffer-run-kill-persistent)
  (define-key helm-map (kbd "C-j") 'helm-next-line)
  (define-key helm-map (kbd "C-k") 'helm-previous-line)
  (define-key helm-map (kbd "C-h") 'helm-next-source)
  (define-key helm-map (kbd "C-l") (kbd "RET"))
  (with-eval-after-load 'helm-files
    (dolist (keymap (list helm-find-files-map helm-read-file-map))
    (define-key keymap (kbd "C-l") 'helm-execute-persistent-action)
    (define-key keymap (kbd "C-h") 'helm-find-files-up-one-level)))
  )

(use-package helm-rg
  :requires (helm)
  :config
  (setq helm-rg-default-directory 'git-root)
  (setq helm-rg-ripgrep-executable "/usr/local/bin/rg")
  )
(use-package helm-projectile)
(use-package swiper-helm)
;;(use-package smartparens)
;;(use-package wgrep-helm)

;; Open compile buffer in the origin buffer
(add-to-list 'display-buffer-alist
             '("*compilation*" display-buffer-same-window))

;;(use-package async)
(use-package magit)

(use-package treemacs
  :config
  (use-package treemacs-projectile)
  (use-package treemacs-evil)
  )

(use-package yasnippet
  :config
  (yas-global-mode t)
  )
;;(use-package yasnippet-snippets)


;;;;;;;;;;;; Rainbow ;;;;;;;;;;;;;;;;
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  )
;;(use-package rainbow-identifiers
;;  :config
;;  (setq rainbow-identifiers-mode t)
;;)

;;;;;;;;;;; Elm ;;;;;;;;;;;;;;;;;
(use-package elm-mode
  :config
  (add-hook 'elm-mode-hook 'flycheck-mode t)
  )
(use-package flycheck-elm)

;;;;;;;;;;; Scala ;;;;;;;;;;;;
;; https://scalameta.org/metals/docs/editors/emacs#installation
;; Enable defer and ensure by default for use-package
;; Keep auto-save/backup files separate from source code:  https://github.com/scalameta/metals/issues/1027
(setq use-package-always-defer t
      use-package-always-ensure t
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :interpreter
    ("scala" . scala-mode))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false"))
)

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :init (global-flycheck-mode))

(use-package lsp-mode
  ;; Optional - enable lsp-mode automatically in scala files
  :hook  (scala-mode . lsp)
         (lsp-mode . lsp-lens-mode)
  :config
  ;; Uncomment following section if you would like to tune lsp-mode performance according to
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq gc-cons-threshold 100000000) ;; 100mb
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-idle-delay 0.500)
  (setq lsp-log-io nil)
  (setq lsp-completion-provider :capf)
  (setq lsp-prefer-flymake nil)

  :hook
  (web-mode . lsp)
  (js2-mode . lsp))

;; Add metals backend for lsp-mode
(use-package lsp-metals)

;; Enable nice rendering of documentation on hover
;;   Warning: on some systems this package can reduce your emacs responsiveness significally.
;;   (See: https://emacs-lsp.github.io/lsp-mode/page/performance/)
;;   In that case you have to not only disable this but also remove from the packages since
;;   lsp-mode can activate it automatically.
;; (use-package lsp-ui)

;; lsp-mode supports snippets, but in order for them to work you need to use yasnippet
;; If you don't want to use snippets set lsp-enable-snippet to nil in your lsp-mode settings
;;   to avoid odd behavior with snippets and indentation
;; (use-package yasnippet)

;; Use company-capf as a completion provider.
;;
;; To Company-lsp users:
;;   Company-lsp is no longer maintained and has been removed from MELPA.
;;   Please migrate to company-capf.
(use-package company
  :hook (scala-mode . company-mode)
  :config
  (setq lsp-completion-provider :capf))

;; Use the Debug Adapter Protocol for running tests and debugging
(use-package posframe
  ;; Posframe is a pop-up tool that must be manually installed for dap-mode
  )
(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode)
  )
;;;;;;;;;;;; Scala End ;;;;;;;;;;;;

(use-package lsp-treemacs)

;;;;;;;;;;;; Lsp-Java ;;;;;;;;;;;;
(use-package lsp-java
  :config
  (add-hook 'java-mode-hook 'lsp)
)

;;;;;;;;;;;; Python ;;;;;;;;;;;;;;
(use-package python-mode)

;;;;;;;;;;;;;;; Docker ;;;;;;;;;;;;;;;;;;
(use-package docker)
(use-package docker-api)
(use-package docker-tramp)
(use-package helm-tramp)

;;;;;;;;;;;;;;;; Kubernetes ;;;;;;;;;;;;;;
;;(use-package k8s-mode)
;;(use-package kubernetes)
;;(use-package kubernetes-helm)
;;(use-package kubernetes-tramp)


;;;;;;;;;;;;;;;;;;;; Perspective ;;;;;;;;;;;;;;;;;;;;;;
(use-package perspective
  :bind
  ("C-x C-b" . persp-list-buffers)   ; or use a nicer switcher, see below
  :config
  (persp-mode))

;;;;;;;;;;;;;;;;;;;; Global-Modes ;;;;;;;;;;;;;;;;;;
(add-hook 'after-init-hook 'show-paren-mode)
(add-hook 'after-init-hook 'projectile-mode)
(add-hook 'after-init-hook 'helm-mode)

(setq company-idle-delay '0)
(setq company-tooltip-idle-delay '0)

;;;;;;;;;;;;; Miscelanous Functions ;;;;;;;;;;;;;;
(global-set-key (kbd "s-/") 'comment-or-uncomment-region-or-line)

;;;;;;;;;; Moves Backup Files to another directory ;;;;;;;;;;
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
(setq auto-save-file-name-transforms `((".*" "~/" t)))
(setq create-lockfiles nil)

;;;;;;;;;;;;;;; Melpa ;;;;;;;;;;;;;;;
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ;; ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
;; For Stable Packages
;; package-archive-priorities '(("melpa-stable" . 1)))
package-archive-priorities '(("melpa" . 1)))
