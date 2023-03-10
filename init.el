(setq inhibit-startup-message t)  ; No startup message (we control that)
(scroll-bar-mode -1)              ; Disable visible scrollbar
(tool-bar-mode -1)                ; Disable the toolbar
(tooltip-mode -1)                 ; Disable tooltips
(set-fringe-mode 10)              ; Give some breathing room
(menu-bar-mode -1)                ; Disable the menu bar

;; Set up the visible bell (i don't know what this does)
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

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
;; Note: 
;;(unless (package-installed-p 'use-package)
;;   (package-install 'use-package))

;; (require 'use-package)
(setq use-package-always-ensure t)

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package doom-themes
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

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Void Emacs")
  (setq dashboard-startup-banner "~/.emacs.d/media/void4.png")
  (setq dashboard-items '((agenda . 5)
                          (projects . 5)
                          (recents . 5)))
  :custom
  (dashboard-image-banner-max-height 450))

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

;; Search engine for emacs
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )


;; Allows vertico buffer in the middle of the screen, like
;; a civilized developer
(use-package vertico-posframe
  :config
  :init
  (vertico-posframe-mode))

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
)


;; Embark for consult
(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Marginalia - text decriptions in vertico
(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

;; Embark - essentially a 'right-click-at-point' framework
(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Orderless - allow space-separated fuzzy matches
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(add-to-list 'exec-path "~/.local/bin")
(use-package exec-path-from-shell
  :config
  (setenv "SHELL" "/usr/bin/zsh")
  (setq exec-path-from-shell-variables '("PATH" "CONDA_HOME"))
  (exec-path-from-shell-initialize))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :init
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package envrc
  :init (envrc-global-mode))

(use-package python
  :delight "?? "
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

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'at-point)
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-delay 1))

(use-package lsp-python-ms
  :defer 0.3
  :custom (lsp-python-ms-auto-install-server t))

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current t)    ;; Disable current candidate preview
  (corfu-preselect nil)      ;; Preselect the prompt
  (corfu-preselect-first nil)
  (corfu-popupinfo-delay 0.5)
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package magit)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(defun void/org-mode-setup ()
  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :weight 'regular :height (cdr face)))

  (org-indent-mode)
  (visual-line-mode 1)
  (setq evil-auto-indent nil)
  (setq org-directory "~/org/"))

(use-package org
  :hook (org-mode . void/org-mode-setup)
  :config
  (setq org-ellipsis " ???")
  (setq org-agenda-files (list
                          "~/org/agenda/Tasks.org"
                          "~/org/agenda/Habits.org"
                          "~/org/agenda/Unfiled.org"))
  (setq org-agenda-start-day "-1d")
  (setq org-agenda-span 14)
  (setq org-roam-directory "~/org/roam/")
  (setq org-roam-index-file "~/org/roam/index.org")

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "ACTIVE(a)" "BLOCKED" "|" "DONE(d!)")))

  (setq org-refile-targets
    '(("Tasks.org" :maxlevel . 3)
      ("Archive.org" :maxlevel . 2)))
  (setq org-refile-use-outline-path t)
  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("school" . ?s)
       ("research" . ?r)
       ("admin" . ?a)
       ("personal" . ?p)
       ("home" . ?h)
       ("family" . ?f)
       ("unfiled" . ?u)))

 (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (tags-todo "+TODO=\"ACTIVE\"-personal-unfiled"
        ((org-agenda-overriding-header "Active Tasks")))
      (tags-todo "+TODO=\"NEXT\"-personal-unfiled"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "+unfiled"
        ((org-agenda-overriding-header "Unfiled Tasks")))
      ))

    ("w" "Workflow Status"
     ((todo "BLOCKED"
            ((org-agenda-overriding-header "Blocked")
             (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
            ((org-agenda-overriding-header "Current Tasks")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "NEXT"
            ((org-agenda-overriding-header "Next Tasks")
             (org-agenda-files org-agenda-files)))
      (todo "TODO"
            ((org-agenda-overriding-header "Backlog")
             (org-agenda-files org-agenda-files)))
      ))))
(setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/org/agenda/Unfiled.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
           (file+olp+datetree "~/org/Journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
      ("jt" "Thought / Idea" entry
           (file+olp+datetree "~/org/Thoughts.org")
           "* %<%I:%M> - Thought :thought:\n\n%?\n\n"
           :empty-lines 1)
      ("jm" "Meeting" entry
           (file+olp+datetree "~/org/Meetings.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

      ;;("m" "Metrics Capture")
      ;;("mw" "Weight" table-line (file+headline "~/Projects/Code/emacs-from-scratch/OrgFiles/Metrics.org" "Weight")
       ;;"| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)
 )))


(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("???" "???" "???" "???" "???" "???" "???")))


(defun void/org-mode-visual-fill ()
  (setq visual-fill-column-width 90
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . void/org-mode-visual-fill))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (shell . t)))

(setq org-confirm-babel-evaluate nil)

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/config.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

;; General - allow for leader keys

(use-package general
    :config
    (general-create-definer void/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

;; Leader: buffer commands
(void/leader-keys
    "b"  '(:ignore t :which-key "buffers")
    "bb" '(consult-buffer :which-key "switch buffers")
    "bs" '(save-buffer :which-key "save buffer")
    "bk" '(kill-current-buffer :which-key "kill buffer"))

;; Leader: global file commands
(void/leader-keys
    "f"  '(:ignore t :which-key "file")
    "ff" '(find-file :which-key "find file"))

;; Leader: project commands
(void/leader-keys
    "p"  '(:ignore t :which-key "project")
    "pp" '(projectile-switch-project :which-key "switch project")
    "pf" '(projectile-find-file :which-key "find file")
    "pg" '(consult-ripgrep :which-key "grep in project"))

;; Leader: quit (and related)
(void/leader-keys
    "q"  '(:ignore t :which-key "quit")
    "qq" '(save-buffers-kill-terminal :which-key "quit emacs"))

;; Leader: window control
(void/leader-keys
    "w" '(:ignore t :which-key "window")
    "wu" '(evil-window-up :which-key "nav up")
    "wU" '(windmove-swap-states-up :which-key "move up")
    "wn" '(evil-window-left :which-key "nav left")
    "wN" '(windmove-swap-states-left :which-key "move left")
    "we" '(evil-window-down :which-key "nav down")
    "wE" '(windmove-swap-states-down :which-key "move down")
    "wi" '(evil-window-right :which-key "nav right")
    "wI" '(windmove-swap-states-right :which-key "move right")
    "wq" '(evil-window-delete :which-key "close window")
    "wv" '(evil-window-vnew :which-key "split vertically")
    "wh" '(evil-window-new :which-key "split horizontally")
    )

;; Leader: magit
(void/leader-keys
    "g"  '(:ignore t :which-key "git")
    "gb" '(magit-branch :which-key "branch")
    "gc" '(magit-commit :which-key "commit")
    "gC" '(magit-clone :which-key "clone")
    "gd" '(magit-diff :which-key "diff")
    "gf" '(magit-fetch :which-key "fetch")
    "gF" '(magit-pull :which-key "pull")
    "gl" '(magit-log :which-key "log")
    "gp" '(magit-status :which-key "push")
    "gs" '(magit-status :which-key "status"))

;;(use-package evil-magit)
(use-package evil
    :init
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
    :config
    (evil-mode 1))

(use-package evil-collection
    :after evil
    :config
    (evil-collection-init))

(use-package hydra)

;; Custom evil keybindings for Colemak-DH
(define-key evil-normal-state-map "t" 'evil-insert)
(define-key evil-normal-state-map "T" 'evil-insert-line)
(define-key evil-normal-state-map "j" 'evil-undo)
(define-key evil-normal-state-map "J" 'evil-redo)
(define-key evil-normal-state-map "k" 'evil-yank)
(define-key evil-normal-state-map "K" 'evil-yank-line)


(define-key evil-normal-state-map "u" 'evil-previous-line)
(define-key evil-normal-state-map "U" 'evil-window-top)
(define-key evil-normal-state-map "n" 'evil-backward-char)
(define-key evil-normal-state-map "N" 'evil-beginning-of-line)
(define-key evil-normal-state-map "e" 'evil-next-line)
(define-key evil-normal-state-map "E" 'evil-window-bottom)
(define-key evil-normal-state-map "i" 'evil-forward-char)
(define-key evil-normal-state-map "I" 'evil-end-of-line)
(define-key evil-normal-state-map "y" 'evil-forward-word-begin)
(define-key evil-normal-state-map "Y" 'evil-forward-WORD-begin)
(define-key evil-normal-state-map "l" 'evil-backward-word-begin)
(define-key evil-normal-state-map "L" 'evil-backward-WORD-begin)
(define-key evil-normal-state-map "h" 'evil-ex-search-next)
(define-key evil-normal-state-map "H" 'evil-ex-search-previous)
(define-key evil-normal-state-map (kbd "C-e") 'evil-scroll-down)


;; Repeat previous bindings for visual mode
(define-key evil-visual-state-map "u" 'evil-previous-line)
(define-key evil-visual-state-map "U" 'evil-window-top)
(define-key evil-visual-state-map "n" 'evil-backward-char)
(define-key evil-visual-state-map "N" 'evil-beginning-of-line)
(define-key evil-visual-state-map "e" 'evil-next-line)
(define-key evil-visual-state-map "E" 'evil-window-bottom)
(define-key evil-visual-state-map "i" 'evil-forward-char)
(define-key evil-visual-state-map "I" 'evil-end-of-line)
(define-key evil-visual-state-map "y" 'evil-forward-word-begin)
(define-key evil-visual-state-map "Y" 'evil-forward-WORD-begin)
(define-key evil-visual-state-map "l" 'evil-backward-word-begin)
(define-key evil-visual-state-map "L" 'evil-backward-WORD-begin)
(define-key evil-visual-state-map "h" 'evil-ex-search-next)
(define-key evil-visual-state-map "H" 'evil-ex-search-previous)
(define-key evil-visual-state-map "k" 'evil-yank)
(define-key evil-visual-state-map "K" 'evil-yank-line)
(define-key evil-normal-state-map (kbd "C-e") 'evil-scroll-down)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)

(define-key evil-window-map "u" 'evil-window-up)
(define-key evil-window-map "n" 'evil-window-left)
(define-key evil-window-map "e" 'evil-window-down)
(define-key evil-window-map "i" 'evil-window-right)
(define-key evil-window-map "U" '+evil/window-move-up)
(define-key evil-window-map "N" '+evil/window-move-left)
(define-key evil-window-map "E" '+evil/window-move-down)
(define-key evil-window-map "I" '+evil/window-move-right)

(define-key ranger-normal-mode-map "u" 'ranger-prev-file)
(define-key ranger-normal-mode-map "n" 'ranger-up-directory)
(define-key ranger-normal-mode-map "e" 'ranger-next-file)
(define-key ranger-normal-mode-map "i" 'ranger-find-file)
