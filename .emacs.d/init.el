;; Include Marmalade and MELPA repos
(require 'package)
(push '("marmalade" . "http://marmalade-repo.org/packages")
      package-archives)
(push '("melpa" . "http://melpa.milkbox.net/packages/")
      package-archives)
(package-initialize)

;; Set exec-path from PATH
(defun set-exec-path-from-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
			  "[ \t\n]*$"
			  ""
			  (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-PATH)) ; Only set the path when run in windowed mode (not necessary in shell) 

;; Move backups to a central location and tuning
(setq backup-directory-alist `(("." . "~/.emacs-saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 2
      kept-old-versions 2
      version-control t)

;; Zenburn theme
(load-theme 'zenburn t)

;; Evil Mode
(require 'evil)
(evil-mode 1)

;; Go Mode
(setenv "GOPATH" (shell-command-to-string "$SHELL --login -i -c 'echo -n $GOPATH'")) ; set the $GOPATH
(add-to-list 'load-path "~/.emacs.d/go")
(require 'go-mode-autoloads)
(defun go-awesome-mode-hook ()
  (setq gofmt-command "goimports")
  (setq tab-width 4)
  (setq indent-tabs-mode 1)
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Godef jump key binding
  (local-set-key (kbd "C-g j") 'godef-jump))

(add-hook 'go-mode-hook 'go-awesome-mode-hook)
(require 'go-autocomplete) 
  
;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(mode-enabled save))

;; Rainbow Delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Smex
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ; Old M-x

;; Flx-Ido Mode
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil) ; disble ido faces to see flx highlights

;; Tune Emacs GC (This is mostly to speed up flx-ido)
(setq gc-cons-threshold 20000000) ; Set emacs to run GC every 20MB allocated (default is .76MB)

;; Auto-complete mode
(require 'auto-complete-config)
(ac-config-default)
