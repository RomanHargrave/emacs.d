;; set up straight and pull in org from upstream rather than bundled
(defvar boostrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(defvar local-pkg (expand-file-name "dist-packages" user-emacs-directory))
(add-to-list 'load-path local-pkg)
(setq use-package-hook-name-suffix nil)

(straight-use-package 'org)
(require 'org)

(org-babel-load-file
 (expand-file-name
  (concat user-emacs-directory "settings.org")))

;; after loading settings.el, load local items
(when-let (local-el
           (file-expand-wildcards
            (concat user-emacs-directory "local.el.d/*.el")))
  (dolist (f local-el)
    (load-file f)))
