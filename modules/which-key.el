;; Which-key, and other tooltip related packages


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
