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
