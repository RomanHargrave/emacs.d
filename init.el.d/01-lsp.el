(use-package eglot
  :demand t
  :bind (:map eglot-mode-map
              ("<mouse-3>" . eglot-menu)
              ("M-v" . eglot-code-actions)
              ("<f6>" . eglot-code-actions)
              ("S-<f6>" . eglot-rename)
              ("S-<f7>" . eglot-find-declaration)))

(use-package company
  :commands (company-mode global-company-mode)
  :hook ((rustic-mode go-mode enh-ruby-mode web-mode go-mode emacs-lisp-mode) . company-mode)
  :bind (([remap indent-for-tab-command] . company-indent-or-complete-common)
         :map company-active-map
         ("<return>" . nil)
         ("RET" . nil)
         ("C-RET" . 'company-complete-selection)
         ("C-<return>" . 'company-complete-selection))
  :after eglot
  :config
  (setq company-backends
        '((:separate company-capf company-clang company-yasnippet)
          (company-dabbrev company-ispell))))
