;; VoidEmacs configuration

(setq inhibit-startup-message t)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)
;;(set-face-attribute 'default nil :font "Roboto Mono")

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Configure backups
(setq
 backup-by-copying t     ; preserves symlinks
 backup-directory-alist  ; store in .saves directory
  '(("." . "~/.saves")) 
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)      ; version the backups

;; Line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
;; Don't show line numbers in org / term
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

;; (require 'use-package)
(setq use-package-always-ensure t)

;; Environment setup (important for conda & direnv)
;; Note: this needs optimization. Very slow atm.
(add-to-list 'exec-path "~/.local/bin")
(use-package exec-path-from-shell
  :config
  (setenv "SHELL" "/usr/bin/zsh")
  (setq exec-path-from-shell-variables '("PATH" "CONDA_HOME"))
  (exec-path-from-shell-initialize))

;; Direnv integration
(use-package envrc
  :init (envrc-global-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package ranger
  :init
  (setq ranger-override-dired 'ranger)
  (setq ranger-show-literal nil))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :init
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package python
  :delight "π "
  :bind (("M-[" . python-nav-backward-block)
         ("M-]" . python-nav-forward-block))
  :preface
  (defun python-remove-unused-imports()
    "Removes unused imports and unused variables with autoflake."
    (interactive)
    (if (executable-find "autoflake")
        (progn
          (shell-command (format "autoflake --remove-all-unused-imports -i %s"
                                 (shell-quote-argument (buffer-file-name))))
          (revert-buffer t t t))
      (warn "python-mode: Cannot find autoflake executable."))))

;; debugger - figure this out later
;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(load "~/.emacs.d/modules/layout.el")
(load "~/.emacs.d/modules/lsp.el")
(load "~/.emacs.d/modules/vertico.el")
(load "~/.emacs.d/modules/consult.el")
(load "~/.emacs.d/modules/keybinds.el")
(load "~/.emacs.d/modules/dashboard.el")
(load "~/.emacs.d/modules/magit.el")
(load "~/.emacs.d/modules/org.el")
(load "~/.emacs.d/modules/which-key.el")



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(exec-path-from-shell corfu magit dashboard hydra evil-collection evil general embark-consult consult embark orderless marginalia vertico-posframe vertico projectile ranger helpful which-key-posframe which-key rainbow-delimiters doom-themes doom-modeline)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
