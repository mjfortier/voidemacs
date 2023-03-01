(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Void Emacs")
  (setq dashboard-startup-banner "~/.emacs.d/void4.png")
  (setq dashboard-items '((projects . 5)
			  ;;(agenda . 5)
			  (recents . 5)))
  :custom
  (dashboard-image-banner-max-height 450))
