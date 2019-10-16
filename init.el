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
        evil-want-integration t
        evil-search-module    'evil-search)
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

; Popup window manager
(use-package popwin
  :ensure t
  :config
  (popwin-mode 1))

(use-package helm
  :ensure t
  :config
  (require 'helm-config)
  (helm-mode 1))

(use-package helm-ag
  :ensure t
  :config
  (setq helm-ag-base-command "rg --vimgrep --no-heading --smart-case"))

(use-package general :ensure t)

(use-package treemacs
  :ensure t
  :config
  '(treemacs-RET-actions-config
    (quote
     ((file-node-close  . treemacs-visit-node-in-most-recently-used-window)
      (file-node-open   . treemacs-visit-node-in-most-recently-used-window)
      (root-node-open   . treemacs-toggle-node)
      (root-node-closed . treemacs-toggle-node)
      (dir-node-open    . treemacs-toggle-node)
      (dir-node-closed  . treemacs-toggle-node)
      (file-node-closed . treemacs-visit-node-default)
      (tag-node-open    . treemacs-toggle-node-prefer-tag-visit)
      (tag-node-closed  . treemacs-toggle-node-prefer-tag-visit)
      (tag-node         . treemacs-visit-node-default)))))

(use-package treemacs-evil :ensure t)

(use-package magit :ensure t)
(use-package evil-magit
  :ensure t
  :config
  (setq evil-magit-state          'normal
        evil-magit-use-y-for-yank nil)
  (require 'evil-magit))

; Theme components
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

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

; modes
(use-package dockerfile-mode  :ensure t :mode "Dockerfile")
(use-package php-mode         :ensure t :mode "\\.php\\'" :magic "\#!.+php")
(use-package lua-mode         :ensure t :mode "\\.lua\\'")
(use-package apt-sources-list :ensure t)
(use-package ansible          :ensure t)

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
(menu-bar-mode -1)

;; Stateless global keybindings
(general-define-key
 "C-s"   'save-buffer
 "M-n"   'evil-next-match
 "M-N"   'evil-previous-match)

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

;; Misc. keys
(general-define-key
 :states 'normal
 ;; Open treemacs
 "SPC t m t" 'treemacs
 "SPC t m o" 'treemacs-select-window
 "SPC t m b" 'helm-buffers-list
 "SPC t t l" 'toggle-truncate-lines
 "SPC f e x" 'eval-buffer)

; helm-ag keys
(general-define-key
 :states 'normal
 "SPC s a"   'helm-ag
 "SPC s s"   'helm-ag-project-root
 "SPC s f"   'helm-ag-this-file)

(general-define-key
 :states 'normal
 "SPC g c c" 'magit-commit-create
 "SPC g c a" 'magit-commit-amend
 "SPC g c e" 'magit-commit-extend
 "SPC g c r" 'magit-commit-reword
 "SPC g a a" 'magit-stage
 "SPC g a m" 'magit-stage-modified
 "SPC g r s" 'magit-unstage
 "SPC g r a" 'magit-unstage-all
 "SPC g s t" 'magit-status)

;; treemacs-mode bindings
(general-define-key
 :keymaps  'treemacs-mode-map
 "SPC t o" 'treemacs
 "C-c"     'treemacs
 "r"       'treemacs-visit-node-in-most-recently-used-window
 "R"       'treemacs-refresh)

;; because once was not enough
(general-define-key
 :keymaps 'treemacs-mode-map
 :prefix "C-w"
 "<up>"    'evil-window-up
 "e"       'evil-window-up
 "<down>"  'evil-window-down
 "n"       'evil-window-down
 "<left>"  'evil-window-left
 "h"       'evil-window-left
 "<right>" 'evil-window-right
 "o"       'evil-window-right)

; set _ to a word character so that C-Left/C-Right/S-Left/S-Right don't skip over it
(modify-syntax-entry ?_ "w")
