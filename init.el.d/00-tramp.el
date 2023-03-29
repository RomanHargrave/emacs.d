(use-package tramp
  :straight (:build t :pre-build (("make" "autoloads")) :no-autoloads t)
  :config
  (setq tramp-persistency-file-name
	(concat user-emacs-directory "tramp")))
