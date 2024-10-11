(use-package org-roam
  :bind* (("C-x C-n f" . org-roam-node-find)
          ("C-x C-n i" . org-roam-node-insert)
          ("C-x C-n c" . org-roam-capture))
  :config
  (setq org-roam-v2-ack t)
  (org-roam-db-autosync-mode)
  (push '("e" "encrypted" plain "%?"
          :target (file+head "${slug}.org.gpg"
                             "#+title: ${title}\n")
          :unnarrowed t)
        org-roam-capture-templates))

(use-package org-remark
  :straight (org-remark
             :type git
             :host github
             :repo "nobiot/org-remark")
  :config
  (require 'org-remark-global-tracking)
  (org-remark-global-tracking-mode t))

(setq org-edit-src-content-indentation 0)

(general-define-key
 :keymaps 'org-mode-map
 "C-<tab>" 'org-indent-line)
