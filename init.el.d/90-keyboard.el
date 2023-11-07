(use-package which-key :config (which-key-mode 1))
;; enhanced defaults and things that should have better bindings
(general-define-key
 ;; these take inspiration from my Vim movement keys for minimak-12
 "C-n"     'next-line
 "C-e"     'previous-line
 "C-S-n"   'scroll-down
 "C-S-e"   'scroll-up
 ;; why does this have such a stupid default binding
 "M-S-k"   'backward-kill-sexp
 ;; prefer regexp isearch
 "C-s"     'isearch-forward-regexp
 "C-M-s"   'isearch-backward-regexp
 ;; region stuff
 "C-x r a" 'align
 ;; closing files opened by request, etc...
 "C-c C-c" 'server-edit
 "C-x C-b" 'switch-to-buffer
 ;; elecom huge buttons - may need to make machine-specific
 "<mouse-8>" 'scroll-up-command
 "<mouse-9>" 'scroll-down-command)

(general-define-key
 :keymaps '(paredit-mode-map emacs-lisp-mode)
 "C-(" 'backward-sexp
 "C-)" 'forward-sexp)

(use-package avy
  :bind
  ("C-c f"   . avy-goto-char-timer)
  ("C-c g"   . avy-goto-line)
  ("C-c S-g" . avy-goto-end-of-line))

(use-package hydra
  :config
  (global-set-key
   (kbd "C-x g")
   (defhydra magit-hydra (global-map "C-x C-g" :exit t)
             "Git Operations"
             ("c c" magit-commit-create    "Commit staged files" :color blue)
             ("c a" magit-commit-amend     "Amend commit" :color blue)
             ("c e" magit-commit-extend    "Extend commit" :color blue)
             ("a"   magit-stage            "Stage")
             ("r"   magit-unstage-file     "Unstage specific file")
             ("R"   magit-unstage-all      "Unstage all staged files")
             ("d d" magit-diff-unstaged    "Show unstaged changes")
             ("d s" magit-diff-staged      "Show staged changes")
             ("d f" magit-diff-buffer-file "Show changes to file at point")
             ("s"   magit-status           "Show repository status")
             ("p"   magit-push-to-remote   "Push active ref to remote")
             ("P"   magit-push-refspecs    "Push specific refs to remote")
             ("l"   magit-log              "Read log")
             ("L"   magit-log-buffer-file  "Read log for file at point")
             ("b"   magit-blame            "Start blaming")))
  (global-set-key
   (kbd "C-x w")
   (defhydra window-hydra (global-map "C-x w")
             ("w" winum-select-window-by-number "Select window number" :column "Mangement")
             ("d" rh--kill-winum                "Kill window number")
             ("q" delete-window                 "Kill active window" :color blue)
             ("|" split-window-right            "Split Right" :column "Layout")
             ("-" split-window-below            "Split Below")
             ("1" winum-select-window-1         "Window 1" :color blue :column "Select")
             ("2" winum-select-window-2         nil :color blue)
             ("3" winum-select-window-3         nil :color blue)
             ("4" winum-select-window-4         nil :color blue)
             ("5" winum-select-window-5         nil :color blue)
             ("6" winum-select-window-6         nil :color blue)
             ("7" winum-select-window-7         nil :color blue)
             ("8" winum-select-window-8         nil :color blue)
             ("9" winum-select-window-9         nil :color blue)
             ("0" winum-select-window-0-or-10   "Window 10" :color blue)
             ("h" windmove-left                 "Move Left")
             ("n" windmove-up                   "Move Up")
             ("e" windmove-down                 "Move Down")
             ("o" windmove-right                "Move Right"))))

(general-define-key
 :keymaps 'org-mode-map
 "C-<tab>" 'org-indent-line)
