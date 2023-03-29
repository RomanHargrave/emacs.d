(use-package dockerfile-mode :mode "Dockerfile")
(use-package erlang)
(use-package elixir-mode)
(use-package lua-mode :mode "\\.lua\\'")
(use-package robots-txt-mode :mode "robots.txt")
(use-package fish-mode :mode "\\.fish\\'" :magic "\\#!.+fish\\'")
(use-package apt-sources-list)
(use-package ansible)
(use-package go-mode
  :mode ("\\.go\\'")
  :hook ((go-mode-hook . (lambda ()
                           (setq-local tab-width 3)))))
(use-package enh-ruby-mode :mode ("\\.rb\\'" "Gemfile" "rackup.ru" "\\.rake\\'" "\\.gemspec'"))

(use-package ebuild-mode :mode "\\.ebuild\\'")
(use-package apache-mode)

(use-package ledger-mode
  :config
  (setq ledger-default-date-format ledger-iso-date-format))

(use-package mediawiki
  :mode ("/tmp/tmp_..\\.wikipedia\\.org_.+" . mediawiki-mode))

(use-package csharp-mode
  :straight (csharp-mode :type git :host github :repo "emacs-csharp/csharp-mode")
  :mode "\\.cs\\'")

(use-package krakatau-mode
  :straight (krakatau-mode :type git :host github :repo "RomanHargrave/krakatau-mode")
  :mode "\\.j\\'")

(use-package cue-mode
  :straight (cue-mode :type git :host github :repo "seblemaguer/cue-mode")
  :mode "\\.cue\\'")

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc")
  :config
  (add-hook 'markdown-mode-hook 'auto-fill-mode))

(use-package sql-indent
  :config
  (add-hook 'sql-mode-hook #'sqlind-minor-mode))

(use-package sqlup-mode
  :config
  (add-hook 'sql-mode-hook #'sqlup-mode)
  (rh/add-to-list 'sqlup-blacklist
                  '("public" "date" "id" "plans"
                    "name" "state")))

(rh/add-exts-to-mode 'fortran-mode '(ftn f77))
(rh/add-exts-to-mode 'f90-mode '(f90 f95 f03 f08))

(rh/add-exts-to-mode 'prolog-mode '(plt))

					; also get dtrt-indent, to be polite when working with other's code
(use-package dtrt-indent)

(use-package clojure-mode
  :mode ("\\.clj\\'"))

(use-package cider :after tramp)

(setq c-default-style '((other . "bsd")
                        (csharp-mode . "csharp"))
      c-basic-offset  2)

(use-package python-mode
  :mode "\\.py\\'"
  :config
  (setq python-shell-interpreter "/usr/bin/python"))

(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package coleslaw
  :straight (coleslaw :type git :host github :repo "equwal/coleslaw"
                      :fork (:host github :repo "RomanHargrave/coleslaw"))
  :config
  (coleslaw-setup))

(use-package fountain-mode
  :mode ("\\.fountain\\'" "\\.spmd\\'")
  :defer t)

(use-package nginx-mode
  :defer t)

(use-package cucumber
  :mode (("\\.feature\\'" . feature-mode)))

(use-package json-mode
  :mode ("\\.json\\'"))
