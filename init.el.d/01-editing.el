(setq-default indent-tabs-mode nil)
(setq-default tab-stop-list '(3 6))
(setq-default tab-with 3)
(setq-default standard-indent 3)

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
      tab-always-insert              'complete ;; does not apply for the most part b/c company
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t))
      backup-directory-alist         `(("." . "~/.emacs.d/backups"))
      delete-old-versions            -1)

(show-paren-mode 1)

(modify-syntax-entry ?_ "w")

;; Whitespace-mode tweaks
(defface whitespace-indent-face
  '((t (:background "color-236")))
  "Highlights non-space indentation")

(defvar computed-indent-chars
  '(("\t" . 'whitespace-indent-face)))

(add-hook 'fortran-mode-hook
          (lambda ()
	    (font-lock-add-keywords nil computed-indent-chars)))

;; Electric commenting
(use-package corral)

(use-package string-edit)

(use-package yasnippet
  :straight (yasnippet :type git :host github :repo "joaotavora/yasnippet")
  :config (yas-global-mode 1))

(use-package deadgrep
  :bind ("<f5>" . deadgrep))

(use-package paredit
  :hook
  ((emacs-lisp-mode-hook . paredit-mode)
   (lisp-mode-hook       . paredit-mode)
   (clojure-mode-hook    . paredit-mode)))

(use-package macrostep
  :config
  (general-define-key
   :keymaps 'emacs-lisp-mode-map
   :major-modes t
   "C-x m e" 'macrostep-expand
   "C-x m c" 'macrostep-collapse
   "C-x m n" 'macrostep-next-macro
   "C-x m p" 'macrostep-prev-macro))
