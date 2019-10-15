(package-initialize)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; If GNU ELPA is having package verification issues, set package-check-signature to nil (temporarily) and install gnu-elpa-keyring-update
(setq package-archives '(("gnu"     . "https://elpa.gnu.org/packages/")
                         ("melpa"   . "https://melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package helm
  :ensure t
  :config
  (require 'helm-config))

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

; just a ton of modes
(use-package dockerfile-mode :ensure t :mode "Dockerfile")
(use-package php-mode        :ensure t :mode "\\.php\\'" :magic "#!/usr/bin/env php")

; Set font
(add-to-list 'default-frame-alist '(font . "Source Code Pro-10"))
(set-face-attribute 'default t :font "Source Code Pro-10")

; Load evil
(setq evil-want-keybinding nil)
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

; general.el, keymapping
(require 'general)
(require 'neotree)
(require 'sublimity)

; Load keymap fixes for evil
(use-package evil-collection
  :ensure t
  :config
  (evil-collection-translate-key nil 'evil-motion-state-map
                                 "n" "j"
                                 "e" "k"
                                 "o" "l"
                                 ;; 'o' needs to be somewhere else
                                 "l" "o")
  (define-key evil-normal-state-map (kbd "o") 'evil-forward-char)
  (evil-collection-init))

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

;; Enable pair hilighting
(show-paren-mode 1)

(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))

;; Disable toolbar and _especially_ scrollbars
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; Stateless global keybindings
(general-define-key
 "C-c" 'neotree-toggle
 "C-s" 'save-buffer)

; Normal mode
;; Window movement
(general-define-key
 :states 'normal
 :prefix "C-w"
 "<up>"    'evil-window-up
 "<down>"  'evil-window-down
 "<left>"  'evil-window-left
 "<right>" 'evil-window-right)

(general-define-key
 :states 'normal
 "SPC f e x" 'eval-buffer)

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
