(package-initialize)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; If GNU ELPA is having package verification issues, set package-check-signature to nil (temporarily) and install gnu-elpa-keyring-update
(setq package-archives '(("gnu"     . "https://elpa.gnu.org/packages/")
                         ("melpa"   . "https://melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

; Set font
(add-to-list 'default-frame-alist '(font . "Source Code Pro-10"))
(set-face-attribute 'default t :font "Source Code Pro-10")

; Load evil
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding  nil
        evil-want-integration t)
  :config
  (evil-mode 1))

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

(use-package helm
  :ensure t
  :config
  (require 'helm-config))

(use-package general :ensure t)

(use-package treemacs :ensure t)
(use-package treemacs-evil :ensure t)

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-molokai t)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; XXX remember to run (all-the-icons-install-fonts)
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

; just a ton of modes
(use-package dockerfile-mode :ensure t :mode "Dockerfile")
(use-package php-mode        :ensure t :mode "\\.php\\'" :magic "#!/usr/bin/env php")


; general.el, keymapping
(require 'general)

; Disable tab auto-insertion
(setq-default indent-tabs-mode nil)

(setq scroll-step                    1
      scroll-margin                  9
      scroll-conservatively          10000
      mouse-wheel-scroll-amount      '(1 ((shift) . 1))
      mouse-whell-progressive-speed  nil
      mouse-whell-follow-mouse       't
      version-control                t
      vc-make-backup-files           t
      vc-follow-symlinks             t
      coding-system-for-read         'utf-8
      coding-system-for-write        'utf-8
      sentence-end-double-space      nil
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t))
      backup-directory-alist         `(("." . "~/.emacs.d/backups"))
      delete-old-versions            -1
      custom-file                    "~/.emacs.d/custom.el")

;; Enable pair hilighting
(show-paren-mode 1)

(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))

;; Disable toolbar and _especially_ scrollbars
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; Stateless global keybindings
(general-define-key
 "C-s"   'save-buffer
 "A-n"   'evil-next-match
 "A-N"   'evil-previous-match)

; Normal mode
;; Window movement
(general-define-key
 :states 'normal
 :prefix "C-w"
 "<up>"    'evil-window-up
 "e"       'evil-window-up
 "<down>"  'evil-window-down
 "n"       'evil-window-down
 "<left>"  'evil-window-left
 "h"       'evil-window-left
 "<right>" 'evil-window-right
 "o"       'evil-window-right)

(general-define-key
 :states 'normal
 "SPC t o"   'treemacs
 "SPC f e x" 'eval-buffer)

;; Tree bindings
(general-define-key
 :keymaps  'treemacs-mode-map
 "SPC t o" 'treemacs
 "C-c"     'treemacs
 "RET"     'treemacs-ret-action)

;; because once was not enough
(general-define-key
 :keymaps 'treemacs-mode-map
 :prefix "C-w"
 "<up>"    'evil-window-up
 "<down>"  'evil-window-down
 "<left>"  'evil-window-left
 "<right>" 'evil-window-right)

; set _ to a word character so that C-Left/C-Right/S-Left/S-Right don't skip over it
(modify-syntax-entry ?_ "w")
