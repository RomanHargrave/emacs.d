(use-package rustic
  :mode (("\\.rs\\'" . rustic-mode))
  :config
  (setq rust-indent-where-clause t
        rustic-indent-offset 4
        rustic-indent-method-chain t
        rustic-format-trigger 'on-save
        rustic-lsp-client 'eglot))
