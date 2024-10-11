(use-package eglot)

(use-package company
  :bind (([remap indent-for-tab-command] . company-indent-or-complete-common)
         :map company-active-map
         ("<return>" . nil)
         ("RET" . nil)
         ("C-RET" . 'company-complete-selection)
         ("C-<return>" . 'company-complete-selection))
  :hook (rustic-mode go-mode enh-ruby-mode web-mode)
  :after eglot)

;(use-package flycheck)
