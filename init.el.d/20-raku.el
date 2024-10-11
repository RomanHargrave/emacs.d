(use-package raku-mode
  :straight (raku-mode :type git :host github :repo "Raku/raku-mode")
  :mode (("\\.raku\\'" . raku-mode)
         ("\\.t6\\'"   . raku-mode)
         ("\\.pm6\\'"  . raku-mode)
         ("\\.p6\\'"   . raku-mode))
  :magic (("#!.+raku" . raku-mode)
          ("#!.+rakudo" . raku-mode)
          ("#!.+perl6" . raku-mode))
  :config
  (setq raku-indent-offset 3))
