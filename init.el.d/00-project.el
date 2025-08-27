(use-package magit
  :demand t
  :config
  (setq magit-save-repository-buffers 'dontask))

(use-package forge
  :after magit)

(use-package projectile
  :demand t
  :after eglot
  :config
  (projectile-mode 1))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package treemacs
  :bind ((:map treemacs-mode-map
                ("RET" . treemacs-enter))
         (:map global-map
               ("C-," . treemacs-select-window)))
  :config
  ;; wtf treemacs, why do you bind RET in global-map???
  (general-define-key
   :keymaps 'global
   "RET" 'newline)
  (general-define-key
   :keymaps 'org-mode-map
   "C-," nil))

(use-package treemacs-projectile)
