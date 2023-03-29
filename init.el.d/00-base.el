;; Utility functions used in config

(defun rh/add-exprs-to-mode (mode expressions)
  "adds EXPRESSIONS to auto-mode-alist for MODE"
  (dolist (expression expressions)
    (add-to-list 'auto-mode-alist `(,expression . ,mode))))

(defun rh/add-exts-to-mode (mode exts)
  "adds EXTENSIONS to auto-mode-alias for MODE"
  (rh/add-exprs-to-mode mode (mapcar (lambda (x) (format "\\.%s\\'" x)) exts)))

(defun rh/add-to-list (list to-add)
  "adds entries in TO-ADD to LIST"
  (unless (consp to-add)
    (error "TO-ADD must be a list"))
  (let ((tgt (symbol-value list)))
    (if tgt
        (setcdr (last tgt) to-add)
      (set list to-add)))
  (symbol-value list))

(defsubst rh/car-equal (l r)
  "Compares the `car's of L and R using `equal'"
  (equal (car l) (car r)))

(use-package general)

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK")
  (exec-path-from-shell-copy-env "SSH_AGENT_PID"))

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

(setq user-inc-dir
      (expand-file-name "inc" user-emacs-directory))

;; set encoding system to UTF-8 to prevent issues with auctex
(setq coding-system-for-read 'utf-8)
(set-language-environment "UTF-8")

;; unfortunately we can't put lockfiles elsewhere for now, and these fuck up everything.
(setq create-lockfiles nil)

;; add locai info dir
(add-to-list 'Info-directory-list "~/.local/share/info")

;; fix stupid keyboard defaults
(dolist (fk  (number-sequence 1 24))
  (global-unset-key (kbd (concat "<f" (number-to-string fk) ">"))))

(dolist (key '("C-t" "C-x m"))
  (global-unset-key (kbd key)))
