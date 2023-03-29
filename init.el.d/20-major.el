(use-package dockerfile-mode :mode "Dockerfile")

(use-package lua-mode :mode "\\.lua\\'")

(use-package robots-txt-mode :mode "robots.txt")

(use-package ansible)

(use-package go-mode
  :mode ("\\.go\\'")
  :hook ((go-mode-hook . (lambda ()
                           (setq-local tab-width 3)))))

(use-package enh-ruby-mode
  :mode ("\\.rb\\'" "Gemfile" "rackup.ru" "\\.rake\\'" "\\.gemspec'"))

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

(use-package clojure-mode
  :mode ("\\.clj\\'"))

(use-package cider :after tramp)

(setq c-default-style '((other . "bsd"))
      c-basic-offset  2)

(use-package python-mode
  :mode "\\.py\\'"
  :config
  (setq python-shell-interpreter "/usr/bin/python"))

(use-package nginx-mode
  :defer t)

(use-package json-mode
  :mode ("\\.json\\'"))
