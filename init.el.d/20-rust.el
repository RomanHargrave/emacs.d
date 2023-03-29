(use-package rustic
  :mode (("\\.rs\\'" . rustic-mode))
  :bind (("C-x m e" . lsp-rust-analyzer-expand-macro))
  :config
  (setq rustic-indent-offset 2)
  (setq rust-indent-where-clause t)
  (setq rustic-indent-method-chain t)
  (setq lsp-rust-analyzer-server-display-inlay-hints t))
