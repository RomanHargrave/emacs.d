(use-package auctex
  :defer t
  :config
  (add-hook 'tex-mode-hook 'auto-fill-mode)
  (add-hook 'latex-mode-hook 'auto-fill-mode))

(use-package company-auctex
  :after auctex)

(use-package edit-indirect-region-latex)

(use-package latex-pretty-symbols)

(use-package latex-preview-pane)

(setq tex-fontify-script nil)
