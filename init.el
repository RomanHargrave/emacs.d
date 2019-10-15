; Package setup before all else
(package-initialize)

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; If GNU ELPA is having package verification issues, set package-check-signature to nil (temporarily) and install gnu-elpa-keyring-update
(setq package-archives '(("gnu"     . "https://elpa.gnu.org/packages/")
                         ("melpa"   . "https://melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package helm    :ensure t)
(use-package general :ensure t)
(use-package neotree :ensure t)

; Disable tab auto-insertion
(setq-default indent-tabs-mode nil)

; Load evil
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

; general.el, keymapping
(require 'general)
(require 'neotree)

(setq version-control                t)
(setq vc-make-backup-files           t)
(setq vc-follow-symlinks             t)
(setq coding-system-for-read         'utf-8)
(setq coding-system-for-write        'utf-8)
(setq sentence-end-double-space      nil)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
(setq backup-directory-alist         `((".", "~/.emacs.d/backups")))
(setq delete-old-versions            -1)

;; Stateless global keybindings
(general-define-key
 "C-c" 'neotree-toggle
 "C-s" 'save-buffer)

;; Normal mode
(general-define-key
 :states 'normal
 "C-w <up>"    'evil-window-up
 "C-w <down>"  'evil-window-down
 "C-w <left>"  'evil-window-left
 "C-w <right>" 'evil-window-right)

;; Neotree bindings
(general-define-key
 :states 'normal
 :keymaps 'neotree-mode-map
 "RET" 'neotree-enter
 "C-c" 'neotree-toggle
 "C"   'neotree-change-root
 "R"   'neotree-refresh)

; set _ to a word character so that C-Left/C-Right/S-Left/S-Right don't skip over it
(modify-syntax-entry ?_ "w")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (helm gnu-elpa-keyring-update general neotree ## robe goto-chg))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
