(use-package lsp-mode
      :hook ((scala-mode-hook    . lsp)
             (php-mode-hook      . lsp)
             (python-mode-hook   . lsp)
             (d-mode-hook        . lsp)
             (perl-mode-hook     . lsp)
             (ruby-mode-hook     . lsp)
             (enh-ruby-mode-hook . lsp)
             (cperl-mode-hook    . lsp)
             (go-mode-hook       . lsp)
             (erlang-mode-hook   . lsp)
             (elixir-mode-hook   . lsp)
             (c-mode-hook        . lsp))
      :commands lsp
      :bind (("M-<tab>" . 'lsp-execute-code-action)
	     ("<f6>"    . 'lsp-rename))
      :config
      (lsp-register-client
       (make-lsp-client
        :new-connection (lsp-stdio-connection '("dub" "run" "dls"))
        :major-modes '(d-mode)
        :server-id 'dls))

      (add-to-list 'lsp-language-id-configuration '(d-mode . "d"))

      (lsp-register-client
       (make-lsp-client
        :new-connection (lsp-stdio-connection '("perl" "-MPerl::LanguageServer" "-e" "Perl::LanguageServer::run"))
        :major-modes '(perl-mode cperl-mode)
        :server-id 'perl-language-server))

      (add-to-list 'lsp-language-id-configuration '(cperl-mode . "perl"))

      (setq lsp-prefer-flymake nil)
      (setq lsp-solargraph-use-bundler t)

      (defun lsp-solargraph--build-command ()
        "Build solargraph command (modded)"
        '("fish" "-c" "rvm use && bundle exec solargraph stdio")))

(use-package lsp-ui
  :requires lsp-mode flycheck
  :bind (("<f4>" . lsp-ui-doc-glance)
	 ("<f7>" . lsp-ui-peek-find-definitions)
	 ("<f8>" . lsp-ui-peek-find-references))
  :config
  (setq lsp-ui-doc-mode t
        lsp-ui-doc-show-with-cursor nil
        lsp-ui-doc-show-with-mouse t
        lsp-ui-doc-position 'at-point
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t
        lsp-ui-imenu-auto-refresh t))

(use-package company
  :bind (([remap indent-for-tab-command] . company-indent-or-complete-common))
  :config
  (general-define-key
   :keymaps 'company-active-map
   "<return>" nil
   "RET" nil
   "C-RET" 'company-complete-selection
   "C-<return>" 'company-complete-selection))

(use-package company-lsp)

(use-package flycheck)
