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
(use-package corral
  :demand t)

(use-package dtrt-indent
  :hook prog-mode)

(use-package yasnippet
  :straight (yasnippet :type git :host github :repo "joaotavora/yasnippet")
  :demand t
  :config (yas-global-mode 1))

(use-package deadgrep
  :bind ("<f5>" . deadgrep))

(use-package paredit
  :hook ((emacs-lisp-mode-hook lisp-mode-hook ielm-mode-hook) . paredit-mode)
  :bind (:map paredit-mode-map
              ("C-(" . 'backward-sexp)
              ("C-)" . 'forward-sexp)))

(use-package macrostep
  :bind (:map emacs-lisp-mode-map
              ("C-x m e" . 'macrostep-expand)
              ("C-x m c" . 'macrostep-collapse)
              ("C-x m n" . 'macrostep-next-macro)
              ("C-x m p" . 'macrostep-prev-macro)))

(add-hook 'before-save-hook #'delete-trailing-whitespace)

(defun reluctant-forward (&optional arg)
  "Move point to the end of the next word or string of
non-word-constituent characters.

Do it ARG times if ARG is positive, or -ARG times in the opposite
direction if ARG is negative. ARG defaults to 1."
  (interactive "^p")
  (if (> arg 0)
      (dotimes (_ arg)
        ;; First, skip whitespace ahead of point
        (when (looking-at-p "[ \t\n]")
          (skip-chars-forward " \t\n"))
        (unless (= (point) (point-max))
          ;; Now, if we're at the beginning of a word, skip it…
          (if (looking-at-p "\\sw")
              (skip-syntax-forward "w")
            ;; …otherwise it means we're at the beginning of a string of
            ;; symbols. Then move forward to another whitespace char,
            ;; word-constituent char, or to the end of the buffer.
            (if (re-search-forward "\n\\|\\s-\\|\\sw" nil t)
                (backward-char)
              (goto-char (point-max))))))
    (dotimes (_ (- arg))
      (when (looking-back "[ \t\n]")
        (skip-chars-backward " \t\n"))
      (unless (= (point) (point-min))
        (if (looking-back "\\sw")
            (skip-syntax-backward "w")
          (if (re-search-backward "\n\\|\\s-\\|\\sw" nil t)
              (forward-char)
            (goto-char (point-min))))))))

(defun reluctant-backward (&optional arg)
  "Move point to the beginning of the previous word or string of
non-word-constituent characters.

Do it ARG times if ARG is positive, or -ARG times in the opposite
direction if ARG is negative. ARG defaults to 1."
  (interactive "^p")
  (reluctant-forward (- arg)))

(general-define-key
 "M-<right>" 'reluctant-forward
 "M-<left>"  'reluctant-backward)
