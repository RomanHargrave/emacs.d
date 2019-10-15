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

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-molokai t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

;; XXX remember to run (all-the-icons-install-fonts)
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package sublimity :ensure t)

; Pin the cursor in the middle line whenever possible
(use-package centered-cursor-mode :ensure t)

; just a ton of modes
(use-package dockerfile-mode :ensure t :mode "Dockerfile")
(use-package php-mode        :ensure t :mode "\\.php\\'" :magic "#!/usr/bin/env php")

; Set font
(add-to-list 'default-frame-alist '(font . "Source Code Pro-10"))
(set-face-attribute 'default t :font "Source Code Pro-10")

; Load evil
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

; general.el, keymapping
(require 'general)
(require 'neotree)
(require 'sublimity)

; Disable tab auto-insertion
(setq-default indent-tabs-mode nil)

(setq scroll-step                    1)
(setq scroll-margin                  9)
(setq scroll-conservatively          10000)
(setq mouse-wheel-scroll-amount      '(1 ((shift) . 1)))
(setq mouse-whell-progressive-speed  nil)
(setq mouse-whell-follow-mouse       't)
(setq version-control                t)
(setq vc-make-backup-files           t)
(setq vc-follow-symlinks             t)
(setq coding-system-for-read         'utf-8)
(setq coding-system-for-write        'utf-8)
(setq sentence-end-double-space      nil)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
(setq backup-directory-alist         `(("." . "~/.emacs.d/backups")))
(setq delete-old-versions            -1)
(setq custom-file                    "~/.emacs.d/custom.el")

(sublimity-mode 1)
(show-paren-mode 1)

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
