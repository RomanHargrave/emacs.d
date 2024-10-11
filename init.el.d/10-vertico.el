(use-package savehist
  :config
  (savehist-mode t))

(use-package vertico
  :config
  (vertico-mode t))

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

(use-package consult
  :bind (("M-n r" . consult-ripgrep)))
