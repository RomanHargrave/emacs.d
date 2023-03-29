(use-package yaml-mode
  :mode ("\\.yaml\\'" "\\.yml\\'"))

(use-package yaml-pro
  :straight (yaml-pro :type git :host github :repo "zkry/yaml-pro")
  :hook ((yaml-mode-hook . yaml-pro-mode))
  :bind (("M-<up>"   . yaml-pro-move-subtree-up)
	 ("M-<down>" . yaml-pro-move-subtree-down)))
