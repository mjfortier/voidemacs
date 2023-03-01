
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

;; Set backup directory
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

(require 'use-package)
(setq use-package-always-ensure t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "60ada0ff6b91687f1a04cc17ad04119e59a7542644c7c59fc135909499400ab8" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" default))
 '(package-selected-packages
   '(eglot peep-dired dirvish ranger hydra evil-collection evil-magit magit ripgrep projectile dashboard evil general all-the-icons helpful which-key rainbow-delimiters embark-consult embark orderless marginalia consult vertico-posframe vertico doom-themes doom-modeline command-log-mode use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; M-x all-the-icons-install-fonts
(use-package all-the-icons)

(use-package command-log-mode)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :config
  (setq which-key-idle-delay 0)
  (setq which-key-min-display-lines 2)
  (setq which-key-add-column-padding 3)
  :init (which-key-mode)
  :diminish which-key-mode)

(use-package which-key-posframe
  :config (which-key-posframe-mode)
  :diminish which-key-posframe-mode
  :init
  (setq which-key-posframe-border-width 3)
  (setq which-key-posframe-parameters '((right-fringe . 30))))

(use-package helpful
  :bind
    ([remap describe-key]      . helpful-key)
    ([remap describe-command]  . helpful-command)
    ([remap describe-variable] . helpful-variable)
    ([remap describe-function] . helpful-callable))

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


(load "~/.emacs.d/modules/vertico.el")
(load "~/.emacs.d/modules/consult.el")
(load "~/.emacs.d/modules/keybinds.el")
(load "~/.emacs.d/modules/dashboard.el")
(load "~/.emacs.d/modules/magit.el")
(load "~/.emacs.d/modules/org.el")

  (void/leader-keys
   "f"  '(:ignore t :which-key "file")
   "ff" '(find-file :which-key "find file"))

  (void/leader-keys
    "p"  '(:ignore t :which-key "project")
    "pp" '(projectile-switch-project :which-key "switch project")
    "pf" '(projectile-find-file :which-key "find file")
    "pg" '(consult-ripgrep :which-key "grep in project"))

  (void/leader-keys
   "q"  '(:ignore t :which-key "quit")
   "qq" '(save-buffers-kill-terminal :which-key "quit emacs"))

(use-package eglot)


;; todo:
;; treemacs
;; org mode +roam +agenda
;; configure consult / embark (whole day task)
;; further ranger config
