;;; init.el -- Main initalization script

;;; Commentary:
;; Copyright (c) 2016 Brayden Winterton
;; Author: Brayden Winterton <bwinterton@gmail.com>

;;; Code:

;; Set the scratch message *Inspiration Station*
(setq initial-scratch-message "\
;;   Success is no accident. It is hard work,
;; perseverance, learning, studying, sacrifice,
;; and most of all, love of what you are doing.
;;                 - Pele
")


;; Include term for use of hooks later on
(require 'term)

;; Include Marmalade and MELPA repos
(require 'package)
(push '("melpa" . "http://melpa.milkbox.net/packages/")
      package-archives)
(package-initialize)

;; Set exec-path from PATH
(defun set-exec-path-from-env()
  "Set the 'exec-path' variable to include the $PATH from the shell when in windowed mode."
  (let ((path-from-shell (replace-regexp-in-string
						  "[ \t\n]*$"
						  ""
						  (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-env)) ; Only set the path when run in windowed mode (not necessary in shell)

;; Tune Emacs GC (This is mostly to speed up flx-ido)
(setq gc-cons-threshold 20000000) ; Set emacs to run GC every 20MB allocated (default is .76MB)

;; Set default font
(add-to-list 'default-frame-alist '(font . "Fira Code-12"))

;; Set default indentation
(setq-default tab-width 4)

;; Inhibit startup message
(setq inhibit-startup-message t)

;; Move backups to a central location and tuning
(setq backup-directory-alist `(("." . "~/.emacs-saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 2
      kept-old-versions 2
      version-control t)

;; Smooth-er Scrolling
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; Material theme
(use-package material-theme
  :load-path "themes"
  :config (load-theme 'material t))

;; Require use-package when compiling
(eval-when-compile
  (require 'use-package))

;; Evil Mode
;; TODO: figure out why the following functions throw warnings and are not suppressed.
(declare-function evil-normal-state-p "evil")  ;; To supress warnings
(declare-function evil-motion-state-p "evil")  ;; To supress warnings
(declare-function evil-set-jump "evil") ;; To supress warnings
(use-package evil
  :ensure t
  :init
  (evil-mode 1))

;; Company Mode
(use-package company
  :ensure t
  :bind
  ("TAB" . company-indent-or-complete-common)
  ("<return>" . nil) ;; unbind the freaking enter button
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay .2)
  (setq company-echo-delay 0)
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 1))

;; Go Mode
(use-package go-mode
  :ensure t
  :init
  (setenv "GOPATH" (shell-command-to-string "$SHELL --login -i -c 'echo -n $GOPATH'")) ; set the $GOPATH
  (setq gofmt-command "goimports")
  (add-hook 'go-mode-hook #'go-awesome-mode-hook))

(defun go-awesome-mode-hook ()
  "Run all of the configuration settings for Golang when this hook is called."
  (setq tab-width 4)
  (setq indent-tabs-mode 1)
										; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
										; Godef jump key binding
  (local-set-key (kbd "C-g j") 'godef-jump))

;; Go Company
(use-package company-go
  :ensure t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-go)))


;; Flycheck
(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq-default flycheck-emacs-lisp-load-path 'inherit) ; Inherit the loadpath for elisp checks
  (defvar flycheck-check-syntax-automatically '(mode-enabled save)))

;; Rainbow Delimiters
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Magit
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :init (global-magit-file-mode))

;; Helm
(use-package helm
  :ensure t
  :bind
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files)
  :init
  (require 'helm-config)
  (helm-mode 1)
  (defvar helm-buffers-fuzzy-matching t)
  (defvar helm-recentf-fuzzy-match t)
  (defvar helm-M-x-fuzzy-match t)
  (declare-function helm-autoresize-mode "helm")  ;; To supress warnings
  (helm-autoresize-mode 1)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  ;; ignore all star buffers except scratch and ansi
  ;;(defvar helm-boring-buffer-regex-list '("\\*[hH]elm.*" "\\*magit.*"))
  (advice-add 'helm-ff-delete-char-backward :around #'helm-find-files-smart-delete))

;; Electric pair mode
(electric-pair-mode t)

;; Projectile
(use-package projectile
  :ensure t
  :init
  (projectile-global-mode)
  (setq projectile-completion-system 'helm))

;; Helm-projectile
(use-package helm-projectile
  :ensure t
  :init (helm-projectile-on))

;; Whitespace mode
;; Currently highlighting lines over 80 chars long and trailing spaces
(use-package whitespace
  :ensure t
  :init
  (setq whitespace-line-column 80) ;; Limit line length to 80 chars
  (setq whitespace-style '(face lines-tail trailing)))

;; ------------- Rust -------------

;; Racer
(use-package racer
  :ensure t
  :init
  (add-hook 'racer-mode-hook #'eldoc-mode))

;; Rust Flycheck
(use-package flycheck-rust
  :ensure t
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package company-racer
  :ensure t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-racer)))

(use-package rust-mode
  :ensure t
  :init
  (define-key rust-mode-map (kbd "M-.") #'racer-find-definition)
  (add-hook 'rust-mode-hook #'racer-mode))


;; ----------- Scala --------------

;; Ensime
(use-package ensime
  :ensure t
  :commands ensime ensime-mode)

(add-hook 'scala-mode-hook 'ensime-mode)


;; ---------- Clojure -------------

(use-package clojure-mode
  :ensure t)

(use-package cider
  :ensure t)

;; ---------- Markdown ------------

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode)))

;; --------- Javascript -----------

(defvar js-indent-level)
(setq js-indent-level 4)

;; ----------- YAML ---------------

(use-package yaml-mode
  :ensure t
  :mode (("\\.yml" . yaml-mode))
  :bind ("C-m" . newline-and-indent))

;; -------- Non Use-Package --------

;; Make ansi-mode better
(add-hook 'term-mode-hook #'better-ansi-term-hook)

;; Display line numbers on all prog-mode buffers
(add-hook 'prog-mode-hook 'linum-mode)

;; Flyspell on all programming buffers
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Whitespace mode on prog mode
(add-hook 'prog-mode-hook 'whitespace-mode)

(column-number-mode)

;; -------- Functions -----------

(defun helm-find-files-smart-delete (orig-func &rest args)
  "Check for editing of helm input then call ORIG-FUNC with ARGS.
The function is designed to be used as an advice function around
helm-ff-delete-char-backward.  The function will check to see if we are on
the end of a path and if we are then backspace will move us up a level instead
of deleting a single character.  This is designed to somewhat mimic the
functionality of 'ido-mode' for find-files."
  (declare-function helm-find-files-initial-input "helm") ;; To supress warnings
  (declare-function helm-find-files-up-one-level "helm")  ;; To supress warnings
  (if (= (length helm-pattern) (length (helm-find-files-initial-input)))
      (helm-find-files-up-one-level 1)
    (apply orig-func args)))

(defun ansi-term-paste ()
  "Allow you to paste in the 'ansi-term' by switching to line mode first."
  (interactive)
  (term-line-mode)
  (yank)
  (term-char-mode))

(defun better-ansi-term-hook ()
  "Make 'ansi-term' better with some custom code."
  (define-key term-raw-map (kbd "s-v") #'ansi-term-paste))

(defun kill-all-buffers ()
  "Kill all buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun restart-buffers ()
  "Kill all buffers and return to scratch."
  (interactive)
  (kill-all-buffers)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (insert initial-scratch-message))

;; Disable bell completely
(setq ring-bell-function 'ignore)
;;; init.el ends here
