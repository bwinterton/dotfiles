;;; init.el -- Main initalization script

;;; Commentary:
;; Copyright (c) 2016 Brayden Winterton
;; Author: Brayden Winterton <bwinterton@gmail.com>

;;; Code:

;; Include Marmalade and MELPA repos
(require 'package)
(push '("marmalade" . "http://marmalade-repo.org/packages")
      package-archives)
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
(add-to-list 'default-frame-alist '(font . "Anonymous Pro-12"))

;; Inhibit startup message
(setq inhibit-startup-message t)

;; Move backups to a central location and tuning
(setq backup-directory-alist `(("." . "~/.emacs-saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 2
      kept-old-versions 2
      version-control t)

;; Zenburn theme
(load-theme 'zenburn t)

;; Require use-package when compiling
(eval-when-compile
  (require 'use-package))

;; Evil Mode
(use-package evil
  :ensure t
  :init (evil-mode 1))

;; Go Mode
(use-package go-mode
  :ensure t
  :init
  (setenv "GOPATH" (shell-command-to-string "$SHELL --login -i -c 'echo -n $GOPATH'")) ; set the $GOPATH
  :config (add-hook 'go-mode-hook #'go-awesome-mode-hook))


(defun go-awesome-mode-hook ()
  "Run all of the configuration settings for Golang when this hook is called."
  (defvar gofmt-command "goimports")
  (setq tab-width 4)
  (setq indent-tabs-mode 1)
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Godef jump key binding
  (local-set-key (kbd "C-g j") 'godef-jump))

;; Go Autocomplete
(use-package go-autocomplete
  :load-path "go")
  
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

;; Smex
(use-package smex
  :ensure t
  :init
  (smex-initialize)
  :config
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)) ; Old M-x
  
;; Flx-Ido Mode
(use-package flx-ido
  :ensure t
  :functions ido-everywhere
  :init
  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil)) ; disble ido faces to see flx highlights


;; Auto-complete mode
(use-package auto-complete
  :ensure t
  :init
  (ac-config-default))

;; Disable bell for certain circumstances
(defun intelligent-bell ()
  "Disable the bell for certain common and personally unwanted circumstances."
  (unless (memq this-command
		'(isearch-abort
		  abort-recursive-edit
		  exit-minibuffer
		  keyboard-quit
		  mwheel-scroll
		  down
		  up
		  next-line
		  previous-line
		  backward-char
		  forward-char))
    (ding)))
(setq ring-bell-function 'intelligent-bell)

;;; init.el ends here
