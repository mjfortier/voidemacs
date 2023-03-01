(use-package magit)


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
