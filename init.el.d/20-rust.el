(use-package rustic
  :mode (("\\.rs\\'" . rustic-mode))
  :defer t
  :config
  (setq rust-indent-where-clause t
        rustic-indent-offset 2
        rustic-indent-method-chain t
        rustic-lsp-client 'eglot))
