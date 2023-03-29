(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :bind* (("C-x C-n f" . org-roam-node-find)
          ("C-x C-n i" . org-roam-node-insert)
          ("C-x C-n c" . org-roam-capture))
  :config
  (org-roam-db-autosync-mode)
  (push '("e" "encrypted" plain "%?"
          :target (file+head "${slug}.org.gpg"
                             "#+title: ${title}\n")
          :unnarrowed t)
        org-roam-capture-templates))

(setq org-edit-src-content-indentation 0)

(use-package org-remark
  :straight (org-remark
             :type git
             :host github
             :repo "nobiot/org-remark")
  :init
  (require 'org-remark-global-tracking)
  (org-remark-global-tracking-mode t))
