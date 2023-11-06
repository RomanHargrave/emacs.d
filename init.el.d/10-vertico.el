(use-package savehist
  :init
  (savehist-mode t))

(use-package vertico
  :init
  (vertico-mode t))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

(use-package consult
  :bind (("M-n r" . consult-ripgrep)))
