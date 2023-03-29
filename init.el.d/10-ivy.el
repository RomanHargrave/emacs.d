(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t)
  (define-key
   ivy-minibuffer-map
   (kbd "<tab>")
   'ivy-next-line)
  :init
  (ivy-mode t))

(use-package counsel)
(use-package counsel-projectile)
(use-package swiper)
