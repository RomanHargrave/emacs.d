(use-package magit
  :demand t
  :config
  (setq magit-save-repository-buffers 'dontask))

(use-package projectile
  :demand t
  :after eglot
  :config
  (projectile-mode 1))

(use-package editorconfig
  :config
  (editorconfig-mode 1))
