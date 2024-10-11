(use-package rustic
  :mode (("\\.rs\\'" . rustic-mode))
  :bind (("C-x m e" . lsp-rust-analyzer-expand-macro))
  :defer t
  :config
  (setq rust-indent-where-clause t
        rustic-indent-offset 2
        rustic-indent-method-chain t
        rustic-lsp-client 'eglot))
