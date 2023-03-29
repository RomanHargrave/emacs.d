(use-package web-mode
  :mode (("\\.tmpl\\'"         . web-mode)
         ("\\.ftl\\'"          . web-mode)
         ("\\.blade\\.php\\'"  . web-mode)
         ("\\.html\\'"         . web-mode)
         ("\\.css\\'"          . web-mode)
         ("\\.tpl\\'"          . web-mode)
         ("\\.vue\\'"          . web-mode)
         ("\\.erb\\'"          . web-mode)
         ("\\.haml\\'"         . web-mode)))

(setq web-mode-engines-alist
      '(("closure"    . "\\.tmpl\\'")
        ("freemarker" . "\\.ftl\\'")))

(defun web-mode-config-hook ()
  "Configuration hook for web-mode"
  (setq web-mode-markup-indent-offset 2))

;; Also configure JS indent
(setq js-indent-level 2)

(add-hook 'web-mode-hook 'web-mode-config-hook)

(use-package typescript-mode
  :mode (("\\.tsx?\\'" . typescript-mode)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection '("npx" "typescript-language-server" "--stdio"))
  :major-modes '(typescript-mode) ;; todo: maybe add web-mode
  :server-id 'tsserver))
