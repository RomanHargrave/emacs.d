(use-package cperl-mode
  :mode ("\\.pl\\'" "\\.pm\\'")
  :config
  (setq cperl-indent-level 3
        cperl-close-paren-offset -3
        cperl-continued-statement-offset 3
        cperl-indent-parens-as-block nil)
  (defalias 'perl-mode 'cperl-mode))
